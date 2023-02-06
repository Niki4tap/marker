use std::{
    borrow::Cow,
    ffi::{OsStr, OsString},
    path::{Path, PathBuf},
    process::Command,
};

use crate::ExitStatus;

/// Package name of a lint crate.
///
/// In case of `Cargo.toml` configuration, can be either:
/// - [`PackageName::Renamed`] with `package` option,
/// - or, simply [`PackageName::Named`]
/// according to the example below:
///
/// ```toml
/// lint_a = { path = "..." }
/// # PackageName::Named("lint_a")
///
/// lint_b = { path = "..", package = "lint_c"}
/// # PackageName::Renamed {
/// #     original: "lint_c",
/// #     new:  "lint_b"
/// # }
/// ```
///
/// If the lint crate was supplied only from path, this is going to be [`PackageName::Named`]
/// with the package name being the directory name, for example in
/// case of command-line arguments:
///
/// `--lints ./marker_lints`
/// This will result in `PackageName::Named("marker_lints")`
#[derive(Debug, Clone)]
pub enum PackageName<'a> {
    /// The lint crate was renamed
    /// ```toml
    /// lint_b = { path = "...", package = "lint_c"}
    /// # PackageName::Renamed {
    /// #     original: "lint_c",
    /// #     new: "lint_b"
    /// # }
    /// ```
    Renamed { original: Cow<'a, str>, new: Cow<'a, str> },
    /// The lint crate wasn't renamed
    /// ```toml
    /// lint_a = { path = "..." }
    /// # PackageName::Named("lint_a")
    /// ```
    Named(Cow<'a, str>),
}

impl PackageName<'_> {
    /// Passes necessary flags to `cargo` during lint crate build
    pub fn cargo_args(&self, cmd: &mut Command) {
        match self {
            PackageName::Renamed { original, .. } | PackageName::Named(original) => {
                cmd.arg("--package");
                cmd.arg(original.as_ref());
            },
        }
    }
}

impl<'a> From<&'a str> for PackageName<'a> {
    fn from(value: &'a str) -> Self {
        Self::Named(value.into())
    }
}

impl<'a> From<&'a OsStr> for PackageName<'a> {
    fn from(value: &'a OsStr) -> Self {
        PackageName::Named(value.to_string_lossy())
    }
}

pub struct LintCrateSpec<'a> {
    /// See documentation for [`PackageName`]
    package_name: PackageName<'a>,
    /// Path to lint crate
    dir: &'a Path,
}

impl<'a> LintCrateSpec<'a> {
    pub fn new(package_name: PackageName<'a>, dir: &'a Path) -> Self {
        Self { package_name, dir }
    }

    /// Currently only checks for semicolons, can be extended in the future
    pub fn is_valid(&self) -> bool {
        let dir_str = self.dir.to_string_lossy();
        !dir_str.contains(';')
            && !dir_str.contains('=')
            && match &self.package_name {
                PackageName::Renamed { original, new } => {
                    !original.contains(';') && !original.contains('=') && !new.contains(';') && !new.contains('=')
                },
                PackageName::Named(name) => !name.contains(';') && !name.contains('='),
            }
    }

    /// Creates a debug build for this crate. The path of the build library
    /// will be returned, if the operation was successful.
    pub fn build(&self, target_dir: &Path, verbose: bool) -> Result<PathBuf, ExitStatus> {
        build_local_lint_crate(self, target_dir, verbose)
    }

    /// Returns the package name of the lint crate
    pub fn package_name(&self) -> &PackageName {
        &self.package_name
    }
}

/// This creates a debug build for a local crate. The path of the build library
/// will be returned, if the operation was successful.
fn build_local_lint_crate(krate: &LintCrateSpec<'_>, target_dir: &Path, verbose: bool) -> Result<PathBuf, ExitStatus> {
    if !krate.dir.exists() {
        eprintln!("The given lint can't be found, searched at: `{}`", krate.dir.display());
        return Err(ExitStatus::LintCrateNotFound);
    }

    // Compile the lint crate
    let mut cmd = Command::new("cargo");
    cmd.arg("build");
    if verbose {
        cmd.arg("--verbose");
    }
    krate.package_name.cargo_args(&mut cmd);
    let exit_status = cmd
        .current_dir(std::fs::canonicalize(krate.dir).unwrap())
        .args(["--lib", "--target-dir"])
        .arg(target_dir.as_os_str())
        .env("RUSTFLAGS", "--cap-lints=allow")
        .spawn()
        .expect("could not run cargo")
        .wait()
        .expect("failed to wait for cargo?");

    if !exit_status.success() {
        return Err(ExitStatus::LintCrateBuildFail);
    }

    // Find the final binary and return the string
    #[cfg(any(target_os = "linux", target_os = "macos"))]
    let lib_file_prefix = "lib";
    #[cfg(target_os = "windows")]
    let lib_file_prefix = "";

    // FIXME: currently this expect, that the lib name is the same as the crate dir.
    // See marker#60
    let file_name = format!(
        "{lib_file_prefix}{}",
        krate.dir.file_name().and_then(OsStr::to_str).unwrap_or_default()
    );
    // Here `debug` is attached as the crate is build with the `cargo build` command
    let mut krate_path = target_dir.join("debug").join(file_name);

    #[cfg(target_os = "linux")]
    krate_path.set_extension("so");
    #[cfg(target_os = "macos")]
    krate_path.set_extension("dylib");
    #[cfg(target_os = "windows")]
    krate_path.set_extension("dll");

    if !krate_path.exists() && !krate_path.is_file() {
        Err(ExitStatus::LintCrateLibNotFound)
    } else {
        Ok(krate_path)
    }
}

/// Builds all crates passed in, returns the env var for the adapter, if successful.
/// Env var looks like this:
/// `foo=/some/path/to/lint/crate/lib;bar=/baz/lib;`
pub fn build_all(krates: &[LintCrateSpec], target_dir: &Path, verbose: bool) -> Result<OsString, ExitStatus> {
    let mut env_var = Vec::<OsString>::with_capacity(krates.len());

    for krate in krates {
        let mut s = OsString::new();
        match krate.package_name() {
            PackageName::Renamed { new, .. } => s.push(new.as_ref()),
            PackageName::Named(name) => s.push(name.as_ref()),
        };
        s.push("=");
        s.push(krate.build(target_dir, verbose)?);
        s.push(";");
        env_var.push(s);
    }

    Ok(env_var.join(OsStr::new(";")))
}
