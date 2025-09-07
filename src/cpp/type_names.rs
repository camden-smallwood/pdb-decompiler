
pub enum TypeName {
    Primitive(PrimitiveType),
    Pointer(PointerType),
    Array(ArrayType),
    Function(FunctionType),
    Path(PathType),
    Enum(EnumType),
    Struct(StructType),
    Union(UnionType),
    Class(ClassType),
}

pub struct PrimitiveType {
    pub is_const: bool,
    pub is_volatile: bool,
    pub kind: pdb2::PrimitiveType,
}

pub struct PointerType {
    pub is_flat_32: bool,
    pub is_volatile: bool,
    pub is_const: bool,
    pub is_unaligned: bool,
    pub is_restrict: bool,
    pub is_mocom: bool,
    pub kind: pdb2::PointerKind,
    pub mode: pdb2::PointerMode,
    pub size: u8,
    pub underlying_type: Box<TypeName>,
}

pub struct ArrayType {
    pub element_type: Box<TypeName>,
    pub length: usize,
}

pub struct FunctionType {
    pub calling_convention: u8, // TODO: enum
    pub cxx_return_udt: bool,
    pub is_constructor: bool,
    pub is_constructor_with_virtual_bases: bool,
    pub return_type: Box<TypeName>,
    pub class_type: Option<Box<TypeName>>,
    pub this_pointer_type: Option<Box<TypeName>>,
    pub parameter_types: Vec<TypeName>,
}

pub struct PathType {
    pub segments: Vec<PathTypeSegment>,
}

pub struct PathTypeSegment {
    pub name: String,
    pub template: Option<PathTypeTemplate>,
}

pub struct PathTypeTemplate {
    pub type_names: Vec<TypeName>,
}

pub struct EnumType {
    pub path: PathType,
}

pub struct StructType {
    pub path: PathType,
}

pub struct UnionType {
    pub path: PathType,
}

pub struct ClassType {
    pub path: PathType,
}
