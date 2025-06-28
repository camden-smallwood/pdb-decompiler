#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypeDefinition {
    pub type_name: String,
    pub underlying_type: pdb2::TypeIndex,
    pub field_attributes: Option<pdb2::FieldAttributes>,
    pub pointer_attributes: Option<pdb2::PointerAttributes>,
    pub containing_class: Option<pdb2::TypeIndex>,
}

impl std::fmt::Display for TypeDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "typedef {};", self.type_name)
    }
}
