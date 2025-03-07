use super::CommonTyData;

#[repr(C)]
#[derive(PartialEq, Eq, Hash)]
pub struct NumTy<'ast> {
    data: CommonTyData<'ast>,
    numeric_kind: NumKind,
}

#[cfg(feature = "driver-api")]
impl<'ast> NumTy<'ast> {
    pub fn new(data: CommonTyData<'ast>, numeric_kind: NumKind) -> Self {
        Self { data, numeric_kind }
    }
}

super::impl_ty_data!(NumTy<'ast>, Num);

impl<'ast> NumTy<'ast> {
    // FIXME: Do we want to keep this method and expose the enum or hide
    // it completely behind methods?
    pub fn numeric_kind(&self) -> NumKind {
        self.numeric_kind
    }

    pub fn is_signed(&self) -> bool {
        self.numeric_kind.is_signed()
    }

    pub fn is_unsigned(&self) -> bool {
        self.numeric_kind.is_unsigned()
    }

    pub fn is_float(&self) -> bool {
        self.numeric_kind.is_float()
    }

    pub fn is_integer(&self) -> bool {
        self.numeric_kind.is_integer()
    }
}

impl<'ast> std::fmt::Debug for NumTy<'ast> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.numeric_kind)
    }
}

#[non_exhaustive]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NumKind {
    Isize,
    I8,
    I16,
    I32,
    I64,
    I128,
    Usize,
    U8,
    U16,
    U32,
    U64,
    U128,
    F32,
    F64,
}

impl NumKind {
    pub fn is_signed(&self) -> bool {
        matches!(
            self,
            NumKind::Isize
                | NumKind::I8
                | NumKind::I16
                | NumKind::I32
                | NumKind::I64
                | NumKind::I128
                | NumKind::F32
                | NumKind::F64
        )
    }

    pub fn is_unsigned(&self) -> bool {
        !self.is_signed()
    }

    pub fn is_float(&self) -> bool {
        matches!(self, NumKind::F32 | NumKind::F64)
    }

    pub fn is_integer(&self) -> bool {
        !self.is_float()
    }
}

impl std::fmt::Debug for NumKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Isize => write!(f, "isize"),
            Self::I8 => write!(f, "i8"),
            Self::I16 => write!(f, "i16"),
            Self::I32 => write!(f, "i32"),
            Self::I64 => write!(f, "i64"),
            Self::I128 => write!(f, "i128"),
            Self::Usize => write!(f, "usize"),
            Self::U8 => write!(f, "u8"),
            Self::U16 => write!(f, "u16"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::U128 => write!(f, "u128"),
            Self::F32 => write!(f, "f32"),
            Self::F64 => write!(f, "f64"),
        }
    }
}
