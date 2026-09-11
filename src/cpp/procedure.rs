use std::fmt::Display;
use crate::tabbed::{TabbedDisplay, TabbedDisplayer};

#[derive(Clone, Debug, PartialEq)]
pub enum Statement {
    Comment(String),
    Commented(Box<Statement>),
    Label(Label),
    Variable(Variable),
    FunctionCall(String, Vec<String>),
    Block(Block),
    #[allow(unused)]
    Return(Return),
    String(String),
    EmptyLine,
    /// A type declared inside the function body (MSVC records these as
    /// `func::__lNN::Type`). Rendered indented to the statement's depth.
    Class(std::rc::Rc<std::cell::RefCell<super::Class>>),
    Enum(super::Enum),
}

impl TabbedDisplay for Statement {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Comment(x) => {
                if x.lines().count() > 1 {
                    write!(f, "/* {x} */")?;
                } else {
                    write!(f, "// {x}")?;
                }
            }

            Statement::Commented(x) => {
                let s = TabbedDisplayer(0, x.as_ref()).to_string();
                
                if s.lines().count() > 1 {
                    // write!(f, "/*")?;
                    let lines = s.lines().collect::<Vec<_>>();
                    for (i, line) in lines.iter().enumerate() {
                        if i > 0 {
                            "".tabbed_fmt(depth, f)?;
                        }
                        write!(f, "// ")?;
                        if i < lines.len() - 1 {
                            writeln!(f, "{}", line)?;
                        } else {
                            write!(f, "{}", line)?;
                        }
                    }
                    // write!(f, "*/")?;
                } else {
                    write!(f, "// {s}")?;
                }
            }

            Statement::Label(x) => {
                write!(f, "{x}")?;
            }

            Statement::Variable(x) => {
                write!(f, "{x}")?;
            }

            Statement::FunctionCall(function, parameters) => {
                write!(f, "{}({});", function, parameters.join(", "))?;
            }

            Statement::Block(x) => {
                x.tabbed_fmt(depth, f)?;
            }

            Statement::Return(x) => {
                write!(f, "return")?;
                
                if let Some(value) = x.value.as_ref() {
                    write!(f, " ")?;
                    value.tabbed_fmt(depth, f)?;
                } else {
                    write!(f, ";")?;
                }
            }

            Statement::String(s) => {
                write!(f, "{}", s)?;
            }

            Statement::EmptyLine => {
                write!(f, "")?;
            }

            // The leading indentation for this line was already emitted by the enclosing
            // block (`"".tabbed_fmt(depth)`); set the type's own `depth` to match so its
            // `{`, members and closing `};` line up under the first line.
            Statement::Class(x) => {
                let mut class = x.borrow().clone();
                class.depth = depth as u32;
                write!(f, "{}", class)?;
            }

            Statement::Enum(x) => {
                let mut enum_type = x.clone();
                enum_type.depth = depth as u32;
                write!(f, "{}", enum_type)?;
            }
        }

        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Label {
    pub address: u64,
    pub name: String,
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:", self.name)?;
        // write!(f, " // 0x{:X}", self.address)?;
        Ok(())
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Variable {
    pub signature: String,
    pub value: Option<String>,
    pub comment: Option<String>,
}

impl Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.signature)?;

        if let Some(value) = self.value.as_ref() {
            write!(f, " = {value}")?;
        }

        write!(f, ";")?;

        if let Some(comment) = self.comment.as_ref() {
            write!(f, " // {comment}")?;
        }

        Ok(())
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Block {
    pub address: Option<u64>,
    pub statements: Vec<Statement>,
}

impl TabbedDisplay for Block {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{{")?;
        
        if let Some(address) = self.address {
            write!(f, " // block start @ 0x{address:X}")?;
        }

        writeln!(f)?;
        
        for statement in self.statements.iter() {
            // A blank separator line carries no indentation (matches top-level spacing).
            if matches!(statement, Statement::EmptyLine) {
                writeln!(f)?;
                continue;
            }

            match statement {
                Statement::Label(_) => "".tabbed_fmt(depth, f)?,
                Statement::Commented(x) if matches!(x.as_ref(), Statement::Label(_)) => "".tabbed_fmt(depth, f)?,
                _ => "".tabbed_fmt(depth + 1, f)?,
            }
            statement.tabbed_fmt(depth + 1, f)?;
            writeln!(f)?;
        }

        "}".tabbed_fmt(depth, f)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Return {
    pub value: Option<Box<Statement>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct MemberMethodData {
    pub class_type: String,
    pub declaring_class: String,
    pub this_pointer_type: Option<String>,
    pub this_adjustment: u32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Procedure {
    pub address: u64,
    pub line: Option<u32>,
    pub type_index: pdb2::TypeIndex,
    pub is_static: bool,
    pub is_inline: bool,
    pub is_extern: bool,
    pub member_method_data: Option<MemberMethodData>,
    pub declspecs: Vec<String>,
    pub name: String,
    pub signature: String,
    pub ida_signature: Option<String>,
    pub body: Option<Block>,
    pub return_type: Option<pdb2::TypeIndex>,
    pub arguments: Vec<(pdb2::TypeIndex, Option<String>)>
}

impl Display for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // `extern` and `static` are mutually exclusive storage-class specifiers.
        if self.is_extern {
            write!(f, "extern ")?;
        }

        if self.is_static {
            write!(f, "static ")?;
        }

        if self.is_inline {
            write!(f, "inline ")?;
        }

        if !self.declspecs.is_empty() {
            write!(f, "__declspec({}) ", self.declspecs.join(", "))?;
        }
        
        write!(f, "{}", self.signature)?;

        match self.body.as_ref() {
            Some(body) => {
                writeln!(f)?;
                write!(f, "{}", TabbedDisplayer(0, body))?;
            },

            None => {
                write!(f, ";")?;
            }
        }

        Ok(())
    }
}
