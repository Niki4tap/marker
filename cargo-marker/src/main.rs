#![warn(clippy::pedantic)]
#![warn(clippy::index_refutable_slice)]
#![allow(clippy::module_name_repetitions)]

mod cli;
mod config;
mod driver;
mod lints;

use std::{ffi::OsString, fs::create_dir_all, io, path::Path, process::exit};

use cli::get_clap_config;
use config::Config;
use driver::{get_driver_path, run_driver};
use lints::{LintCrateSpec, PackageName};
use once_cell::sync::Lazy;

use crate::{driver::print_driver_version, lints::build_all};

const CARGO_ARGS_SEPARATOR: &str = "--";
const VERSION: &str = concat!("cargo-marker ", env!("CARGO_PKG_VERSION"));
const LINT_KRATES_BASE_DIR: &str = "./target/marker";
const NO_LINTS_ERROR: &str = concat!(
    "Please provide at least one valid lint crate, ",
    "with the `--lints` argument, ",
    "or `[workspace.metadata.marker.lints]` in `Cargo.toml`"
);
static MARKER_LINT_DIR: Lazy<String> = Lazy::new(|| prepare_lint_build_dir("marker", "marker"));

#[derive(Debug)]
pub enum ExitStatus {
    /// The toolchain validation failed. This could happen, if rustup is not
    /// installed or the required toolchain is not installed.
    InvalidToolchain = 100,
    /// Unable to find the driver binary
    MissingDriver = 200,
    /// Nothing we can really do, but good to know. The user will have to analyze
    /// the forwarded cargo output.
    DriverInstallationFailed = 300,
    /// A general collection status, for failures originating from the driver
    DriverFailed = 400,
    /// The lint crate build failed for some reason
    LintCrateBuildFail = 500,
    /// Lint crate could not be found
    LintCrateNotFound = 501,
    /// The lint crate has been build, but the resulting binary could not be found.
    LintCrateLibNotFound = 502,
    /// General "bad config" error
    BadConfiguration = 600,
    /// No lint crates were specified -> nothing to do
    NoLints = 601,
    /// Can't deserialise `workspace.metadata.marker.lints` properly
    WrongStructure = 602,
    /// An invalid configuration value was specified
    InvalidValue = 603,
    /// Check failed
    MarkerCheckFailed = 1000,
}

/// This creates the absolute path for a given build directory.
fn prepare_lint_build_dir(dir_name: &str, info_name: &str) -> String {
    if !Path::new("Cargo.toml").exists() {
        // FIXME: This is a temporary check to ensure that we don't randomly create files.
        // This should not be part of the release and maybe be replaced by something more
        // elegant or removed completely.
        eprintln!("Cargo manifest doesn't exist (`Cargo.toml`), most likely running in the wrong directory");
        exit(-1);
    }

    let path = Path::new(LINT_KRATES_BASE_DIR).join(dir_name);
    if !path.exists() {
        create_dir_all(&path).unwrap_or_else(|_| panic!("Error while creating lint crate {info_name} directory"));
    }

    std::fs::canonicalize(path)
        .expect("This should find the directory, as we just created it")
        .display()
        .to_string()
}

fn choose_lint_crates<'a>(
    args: &'a clap::ArgMatches,
    config: Option<&'a Config>,
) -> Result<Vec<LintCrateSpec<'a>>, ExitStatus> {
    match args.get_many::<OsString>("lints") {
        Some(v) => v
            .map(|v| {
                let Some(file_name) = Path::new(v).file_name() else {
                return Err(ExitStatus::InvalidValue);
            };
                Ok(LintCrateSpec::new(
                    PackageName::Named(file_name.to_string_lossy()),
                    v.as_ref(),
                ))
            })
            .collect(),
        None => {
            if let Some(config) = config {
                config.collect_crates()
            } else {
                eprintln!("{NO_LINTS_ERROR}");
                Err(ExitStatus::NoLints)
            }
        },
    }
}

fn main() -> Result<(), ExitStatus> {
    let matches = get_clap_config().get_matches_from(
        std::env::args()
            .enumerate()
            .filter_map(|(index, value)| (!(index == 1 && value == "marker")).then_some(value))
            .take_while(|s| s != CARGO_ARGS_SEPARATOR),
    );

    let verbose = matches.get_flag("verbose");
    let test_build = matches.get_flag("test-setup");
    let dev_build = cfg!(feature = "dev-build");

    if matches.get_flag("version") {
        print_version(verbose);
        return Ok(());
    }

    let config = match Config::get_marker_config() {
        Ok(v) => Some(v),
        Err(e) => match e {
            config::ConfigFetchError::NotFound => None,
            _ => return Err(e.emit_and_convert()),
        },
    };

    match matches.subcommand() {
        Some(("setup", _args)) => driver::install_driver(verbose, dev_build),
        Some(("check", args)) => run_check(
            &choose_lint_crates(args, config.as_ref())?,
            verbose,
            dev_build,
            test_build,
        ),
        None => run_check(
            &choose_lint_crates(&matches, config.as_ref())?,
            verbose,
            dev_build,
            test_build,
        ),
        _ => unreachable!(),
    }
}

fn run_check(
    crate_entries: &[LintCrateSpec],
    verbose: bool,
    dev_build: bool,
    test_build: bool,
) -> Result<(), ExitStatus> {
    // If this is a dev build, we want to recompile the driver before checking
    if dev_build {
        driver::install_driver(verbose, dev_build)?;
    }

    if crate_entries.is_empty() {
        eprintln!("{NO_LINTS_ERROR}");
        return Err(ExitStatus::NoLints);
    }

    if !crate_entries.iter().all(LintCrateSpec::is_valid) {
        eprintln!("The absolute paths of lint crates are not allowed to contain a `;`");
        return Err(ExitStatus::InvalidValue);
    }

    println!();
    println!("Compiling Lints:");
    let target_dir = Path::new(&*MARKER_LINT_DIR);
    let env = build_all(crate_entries, target_dir, verbose)?;

    #[rustfmt::skip]
    let env = vec![
        (OsString::from("RUSTC_WORKSPACE_WRAPPER"), get_driver_path().as_os_str().to_os_string()),
        (OsString::from("MARKER_LINT_CRATES"), env)
    ];
    if test_build {
        print_env(env).unwrap();
        Ok(())
    } else {
        let cargo_args = std::env::args().skip_while(|c| c != CARGO_ARGS_SEPARATOR).skip(1);
        run_driver(env, cargo_args, verbose)
    }
}

fn print_version(verbose: bool) {
    println!("cargo-marker version: {}", env!("CARGO_PKG_VERSION"));

    if verbose {
        print_driver_version();
    }
}

#[allow(clippy::unnecessary_wraps)]
fn print_env(env: Vec<(OsString, OsString)>) -> io::Result<()> {
    // Operating systems are fun... So, this function prints out the environment
    // values to the standard output. For Unix systems, this requires `OsStr`
    // objects, as file names are just bytes and don't need to be valid UTF-8.
    // Windows, on the other hand, restricts file names, but uses UTF-16. The
    // restriction only makes it slightly better, since windows `OsString` version
    // doesn't have a `bytes()` method. Rust additionally has a restriction on the
    // stdout of windows, that it has to be valid UTF-8, which means more conversion.
    //
    // This would be so much easier if everyone followed the "UTF-8 Everywhere Manifesto"

    #[cfg(any(target_os = "linux", target_os = "macos"))]
    {
        use std::io::Write;
        use std::os::unix::prelude::OsStrExt;

        // stdout is used directly, to print the `OsString`s without requiring
        // them to be valid UTF-8
        let mut lock = io::stdout().lock();
        for (name, value) in env {
            write!(lock, "env:")?;
            lock.write_all(name.as_bytes())?;
            write!(lock, "=")?;
            lock.write_all(value.as_bytes())?;
            writeln!(lock)?;
        }
    }

    #[cfg(target_os = "windows")]
    {
        for (name, value) in env {
            if let (Some(name), Some(value)) = (name.to_str(), value.to_str()) {
                println!("env:{name}={value}");
            } else {
                unreachable!("Windows requires it's file path to be valid UTF-16 AFAIK");
            }
        }
    }

    Ok(())
}
