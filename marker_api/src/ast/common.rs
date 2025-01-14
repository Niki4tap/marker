mod id;
pub use id::*;
mod span;
pub use span::*;
mod callable;
pub use callable::*;
mod ast_path;
pub use ast_path::*;

use std::fmt::Debug;

use super::generic::GenericArgs;

#[non_exhaustive]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Edition {
    Edition2015,
    Edition2018,
    Edition2021,
}

#[non_exhaustive]
#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Applicability {
    /// The suggestion is definitely what the user intended, or maintains the exact meaning of the
    /// code. This suggestion should be automatically applied.
    ///
    /// In case of multiple `MachineApplicable` suggestions (whether as part of
    /// the same `multipart_suggestion` or not), all of them should be
    /// automatically applied.
    MachineApplicable,

    /// The suggestion may be what the user intended, but it is uncertain. The suggestion should
    /// result in valid Rust code if it is applied.
    MaybeIncorrect,

    /// The suggestion contains placeholders like `(...)` or `{ /* fields */ }`. The suggestion
    /// cannot be applied automatically because it will not result in valid Rust code. The user
    /// will need to fill in the placeholders.
    HasPlaceholders,

    /// The suggestion can not be automatically applied or the applicability is unknown.
    Unspecified,
}

#[non_exhaustive]
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum Abi {
    /// This is the default of the current driver, the actual ABI can vary between
    /// implementations. In general this means that the user has not selected a
    /// specific ABI.
    Default,
    C,
    /// FIXME: Remove this variant. See
    /// <https://doc.rust-lang.org/nightly/nightly-rustc/rustc_target/spec/abi/enum.Abi.html>
    Other,
}

pub struct Spanned<'ast, T> {
    pub node: T,
    pub span: &'ast Span<'ast>,
}

#[cfg(feature = "driver-api")]
impl<'ast, T> Spanned<'ast, T> {
    #[must_use]
    pub fn new(node: T, span: &'ast Span<'ast>) -> Self {
        Self { node, span }
    }
}

pub trait Attribute<'ast>: Debug {
    // FIXME: Add attribute functions
}

#[repr(C)]
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct TraitRef<'ast> {
    item_id: ItemId,
    generics: GenericArgs<'ast>,
}

#[cfg(feature = "driver-api")]
impl<'ast> TraitRef<'ast> {
    pub fn new(item_id: ItemId, generics: GenericArgs<'ast>) -> Self {
        Self { item_id, generics }
    }
}

impl<'ast> TraitRef<'ast> {
    pub fn trait_id(&self) -> ItemId {
        self.item_id
    }

    pub fn generics(&self) -> &GenericArgs<'ast> {
        &self.generics
    }
}
