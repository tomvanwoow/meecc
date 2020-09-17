#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Integer {
    Char,
    Short,
    Int,
    Long,
    LongLong,
    Bool,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Float {
    Float,
    Double,
    LongDouble,
}

bitflags! {
    pub struct CVQualifier: u8 {
        const NONE     = 0b00;
        const CONST    = 0b01;
        const VOLATILE = 0b10;
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Type {
    Void,
    Char,
    Signed(Integer),
    Unsigned(Integer),
    Float(Float),
    Pointer(Box<QualType>),
    Enum(Vec<String>),
    Array(Box<QualType>, Option<usize>),
    Struct(Vec<(String, QualType)>),
    Union(Vec<(String, QualType)>),
    Function(Vec<QualType>, Box<QualType>),
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct QualType {
    qual: CVQualifier,
    typ: Type,
}