use cfg_if::cfg_if;
use libloading::Library;

use marker_api::context::AstContext;
use marker_api::lint::Lint;
use marker_api::LintPass;

use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::{OsStr, OsString};
use std::hash::{Hash, Hasher};

thread_local! {
    /// **Warning**
    ///
    /// Lifetimes are obviously fake here
    #[doc(hidden)]
    pub(super) static LINT_REGISTRY: RefCell<Option<&'static mut LintCrateRegistry<'static>>> = RefCell::new(None);
}

/// Splits [`OsStr`] by an ascii character
// This *maybe* works on windows, and *maybe* works on unix, I'm giving no guarantees here
fn split_os_str(s: &OsStr, c: u8) -> Vec<OsString> {
    cfg_if! {
        if #[cfg(unix)] {
            unix_split_os_str(s, c)
        } else if #[cfg(windows)] {
            windows_split_os_str(s, c)
        } else {
            unimplemented!("`split_os_str` currently works only on unix and windows")
        }
    }
}

#[cfg(unix)]
#[doc(hidden)]
fn unix_split_os_str(s: &OsStr, c: u8) -> Vec<OsString> {
    use std::os::unix::ffi::OsStrExt;

    s.as_bytes()
        .split(|byte| *byte == c)
        .map(|bytes| OsStr::from_bytes(bytes).into())
        .collect()
}

#[cfg(windows)]
#[doc(hidden)]
fn windows_split_os_str(s: &OsStr, c: u8) -> Vec<OsString> {
    use std::os::windows::ffi::*;

    let bytes: Vec<u16> = s.encode_wide().collect();

    bytes
        .split(|v| *v == u16::from(c))
        .map(|bytes| OsString::from_wide(bytes))
        .collect()
}

/// <https://doc.rust-lang.org/stable/nightly-rustc/rustc_lint/struct.LintId.html>
#[derive(Clone, Copy, Debug)]
pub(super) struct LintId {
    pub lint: &'static Lint,
}

impl LintId {
    pub(super) fn of(lint: &'static Lint) -> LintId {
        LintId { lint }
    }
}

impl PartialEq for LintId {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.lint, other.lint)
    }
}

impl Eq for LintId {}

impl Hash for LintId {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let ptr = self.lint as *const Lint;
        ptr.hash(state);
    }
}

/// This struct loads external lint crates into memory and provides a safe API
/// to call the respective methods on all of them.
#[derive(Default)]
pub struct LintCrateRegistry<'ast> {
    passes: Vec<LoadedLintCrate<'ast>>,
    /// This monkey-patches lints at runtime, currently only to support lint renaming, but might be
    /// used in the future for other purposes
    patched_lint_store: HashMap<LintId, &'static Lint>,
    expected_lint: Option<usize>,
}

impl<'ast> LintCrateRegistry<'ast> {
    /// # Errors
    /// This can return errors if the library couldn't be found or if the
    /// required symbols weren't provided.
    fn load_external_lib(name: String, lib_path: &OsStr) -> Result<LoadedLintCrate<'ast>, LoadingError> {
        let lib: &'static Library = Box::leak(Box::new(
            unsafe { Library::new(lib_path) }.map_err(|_| LoadingError::FileNotFound)?,
        ));

        let pass = LoadedLintCrate::try_from_lib(name, lib)?;

        // FIXME: Create issue for lifetimes and fix droping and pointer decl stuff

        Ok(pass)
    }

    /// # Panics
    ///
    /// Panics if a lint in the environment couldn't be loaded.
    pub fn new_from_env() -> Self {
        let mut new_self = Self::default();

        let Some((_, lint_crates_lst)) = std::env::vars_os().find(|(name, _val)| name == "MARKER_LINT_CRATES") else {
            panic!("Adapter tried to find `MARKER_LINT_CRATES` env variable, but it was not present");
        };

        for crate_spec in split_os_str(&lint_crates_lst, b';') {
            if crate_spec.is_empty() {
                continue;
            }

            let Ok([name, path]): Result<[OsString; 2], _> = split_os_str(&crate_spec, b'=').try_into() else {
                panic!("`cargo-marker` has lied to `marker_adapter`, wrong format for `MARKER_LINT_CRATES` variable: {}", crate_spec.to_string_lossy());
            };

            // This unwrap should be safe, unless `cargo-marker` has lied to us, in which case we'll just panic
            let lib = match Self::load_external_lib(name.to_str().unwrap().into(), &path) {
                Ok(v) => v,
                Err(err) => panic!("Unable to load `{}`, reason: {err:?}", name.to_string_lossy()),
            };

            new_self.passes.push(lib);
        }

        new_self
    }

    fn patch_lint(lint: LintId, name: &str) -> &'static Lint {
        Box::leak(Box::new(Lint {
            name: Box::leak(format!("{name}::{}", lint.lint.name).into_boxed_str()),
            ..*lint.lint
        }))
    }

    fn get_expected_lint(&mut self) -> &LoadedLintCrate {
        &self.passes[self
            .expected_lint
            .expect("`expected_lint` was `None` during linting, this should never happen")]
    }

    pub(super) fn get_patched_lint(&mut self, lint: LintId) -> &'static Lint {
        if self.patched_lint_store.contains_key(&lint) {
            return self.patched_lint_store[&lint];
        }

        let new_name = &self.get_expected_lint().name;
        let patched = Self::patch_lint(lint, new_name);

        self.patched_lint_store.insert(lint, patched);

        patched
    }

    pub(super) fn set_ast_context(&self, cx: &'ast AstContext<'ast>) {
        for lint_pass in &self.passes {
            lint_pass.set_ast_context(cx);
        }
    }
}

impl<'a> LintPass for LintCrateRegistry<'a> {
    fn registered_lints(&self) -> Box<[&'static Lint]> {
        let mut lints = vec![];
        for lint_pass in &self.passes {
            lints.extend_from_slice(&lint_pass.registered_lints());
        }
        lints.into_boxed_slice()
    }

    marker_api::for_each_lint_pass_fn!(crate::gen_lint_crate_reg_lint_pass_fn);
}

#[macro_export]
macro_rules! gen_lint_crate_reg_lint_pass_fn {
    (fn $fn_name:ident<'ast>(&self $(, $arg_name:ident: $arg_ty:ty)*) -> $ret_ty:ty) => {
        // Nothing these will be implemented manually
    };
    (fn $fn_name:ident<'ast>(&(mut) self $(, $arg_name:ident: $arg_ty:ty)*) -> ()) => {
        fn $fn_name<'ast>(&mut self $(, $arg_name: $arg_ty)*) {
            let passes = self.passes.clone();
            // This is probably wildly unsafe, I really don't like this, but I'm also really not sure how to avoid it
            LINT_REGISTRY.with(|v| v.replace(Some(unsafe {core::mem::transmute(self)})));

            for (idx, lint_pass) in passes.iter().enumerate() {
                LINT_REGISTRY.with(|v| v.borrow_mut().as_mut().unwrap().expected_lint = Some(idx));
                lint_pass.$fn_name($($arg_name, )*);
            }
        }
    };
}

/// This macro generates the `LoadedLintCrate` struct, and functions for
/// calling the [`LintPass`] functions. It's the counter part to
/// [`marker_api::interface::export_lint_pass`]
#[macro_export]
macro_rules! gen_LoadedLintCrate {
    (
        ($dollar:tt)
        $(fn $fn_name:ident<'ast>(& $(($mut_:tt))? self $(, $arg_name:ident: $arg_ty:ty)*) -> $ret_ty:ty;)+
    ) => {
        /// This struct holds function pointers to api functions in the loaded lint crate
        /// It owns the library instance. It sadly has to be stored as a `'static`
        /// reference due to lifetime restrictions.
        #[derive(Clone, Debug)]
        struct LoadedLintCrate<'a> {
            name: String,
            _lib: &'static Library,
            set_ast_context: libloading::Symbol<'a, for<'ast> unsafe extern "C" fn(&'ast AstContext<'ast>) -> ()>,
            $(
                $fn_name: libloading::Symbol<'a, for<'ast> unsafe extern "C" fn($($arg_ty,)*) -> $ret_ty>,
            )*
        }

        impl<'a> LoadedLintCrate<'a> {
            /// This function tries to resolve all api functions in the given library.
            fn try_from_lib(name: String, lib: &'static Library) -> Result<Self, LoadingError> {
                // get function pointers
                let get_marker_api_version = {
                    unsafe {
                        lib.get::<unsafe extern "C" fn() -> &'static str>(b"get_marker_api_version\0")
                            .map_err(|_| LoadingError::MissingLintDeclaration)?
                    }
                };
                if unsafe { get_marker_api_version() } != marker_api::MARKER_API_VERSION {
                    return Err(LoadingError::IncompatibleVersion);
                }

                let set_ast_context = unsafe {
                    lib.get::<for<'ast> unsafe extern "C" fn(&'ast AstContext<'ast>)>(b"set_ast_context\0")
                        .map_err(|_| LoadingError::MissingLintDeclaration)?
                };

                $(
                    let $fn_name = {
                        let name: Vec<u8> = stringify!($fn_name).bytes().chain(std::iter::once(b'\0')).collect();
                        unsafe {
                            lib.get::<for<'ast> unsafe extern "C" fn($($arg_ty,)*) -> $ret_ty>(&name)
                                .map_err(|_| LoadingError::MissingLintDeclaration)?
                        }
                    };
                )*
                // create Self
                Ok(Self {
                    name,
                    _lib: lib,
                    set_ast_context,
                    $(
                        $fn_name,
                    )*
                })
            }

            fn set_ast_context<'ast>(&self, cx: &'ast AstContext<'ast>) -> () {
                unsafe {
                    (self.set_ast_context)(cx)
                }
            }

            // safe wrapper to external functions
            $(
                fn $fn_name<'ast>(&self $(, $arg_name: $arg_ty)*) -> $ret_ty {
                    unsafe {
                        (self.$fn_name)($($arg_name,)*)
                    }
                }
            )*
        }

    };
}
marker_api::lint_pass_fns!(crate::gen_LoadedLintCrate);

#[derive(Debug)]
pub enum LoadingError {
    FileNotFound,
    IncompatibleVersion,
    MissingLintDeclaration,
}
