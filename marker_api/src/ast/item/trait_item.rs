use crate::ast::generic::{GenericParams, TyParamBound};
use crate::ffi::FfiSlice;

use super::{AssocItemKind, CommonItemData};

/// A trait item like:
///
/// ```
/// trait Example {
///     const CONST_NO_DEFAULT: i32;
///     const CONST_WITH_DEFAULT: i32 = 99;
///     type TypeNoDefault;
///     fn method_without_default(&self);
///     fn method_with_default(&self) {}
/// }
/// ```
///
/// * See <https://doc.rust-lang.org/stable/reference/items/modules.html>
#[repr(C)]
#[derive(Debug)]
pub struct TraitItem<'ast> {
    data: CommonItemData<'ast>,
    is_unsafe: bool,
    generics: GenericParams<'ast>,
    supertraits: FfiSlice<'ast, TyParamBound<'ast>>,
    items: FfiSlice<'ast, AssocItemKind<'ast>>,
}

super::impl_item_data!(TraitItem, Trait);

impl<'ast> TraitItem<'ast> {
    pub fn is_unsafe(&self) -> bool {
        self.is_unsafe
    }

    pub fn generics(&self) -> &GenericParams<'ast> {
        &self.generics
    }

    /// A supertrait like the `Supertrait` in this example:
    ///
    /// ```
    /// # trait Supertrait {}
    /// //            vvvvvvvvvvvv
    /// trait Subtrait: Supertrait {
    ///     // ...
    /// }
    /// ```
    pub fn supertraits(&self) -> &'ast [TyParamBound<'ast>] {
        self.supertraits.get()
    }

    pub fn items(&self) -> &[AssocItemKind<'ast>] {
        self.items.get()
    }
}

#[cfg(feature = "driver-api")]
impl<'ast> TraitItem<'ast> {
    pub fn new(
        data: CommonItemData<'ast>,
        is_unsafe: bool,
        generics: GenericParams<'ast>,
        supertraits: &'ast [TyParamBound<'ast>],
        items: &'ast [AssocItemKind<'ast>],
    ) -> Self {
        Self {
            data,
            is_unsafe,
            generics,
            supertraits: supertraits.into(),
            items: items.into(),
        }
    }
}
