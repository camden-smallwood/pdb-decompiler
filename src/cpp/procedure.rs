use std::fmt::Display;
use crate::tabbed::{TabbedDisplay, TabbedDisplayer};

//
// TODO: Separate signature into separate return_type, name, and parameters fields
//

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Statement {
    Comment(String),
    Commented(Box<Statement>),
    Label(Label),
    Variable(Variable),
    FunctionCall(String, Vec<String>),
    Block(Block),
}

impl TabbedDisplay for Statement {
    fn tabbed_fmt(&self, depth: usize, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Comment(x) => {
                write!(f, "// {x}")?;
            }

            Statement::Commented(x) => {
                let s = TabbedDisplayer(0, x.as_ref()).to_string();
                
                if s.lines().count() > 1 {
                    write!(f, "/* {s} */")?;
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
        }

        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Label {
    pub address: u64,
    pub name: String,
}

impl Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:", self.name)?;
        // write!(f, " 0x{:X}", self.address)?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Default, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Procedure {
    pub address: u64,
    pub line: Option<u32>,
    pub type_index: pdb2::TypeIndex,
    pub is_static: bool,
    pub signature: String,
    pub body: Option<Block>,
}

impl Display for Procedure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_static {
            write!(f, "static ")?;
        }
        
        write!(f, "{}", self.signature)?;

        match self.body.as_ref() {
            Some(body) => {
                if self.address != 0 {
                    write!(f, " // 0x{:X}", self.address)?;
                }
                writeln!(f)?;
                write!(f, "{}", TabbedDisplayer(0, body))?;
            },

            None => {
                write!(f, ";")?;
                if self.address != 0 {
                    write!(f, " // 0x{:X}", self.address)?;
                }
            }
        }

        Ok(())
    }
}
