use std::fmt;
use std::rc::Rc;

use crate::internal_store::TrackedVector;
use crate::parser::StatementNode;

#[derive(Debug, Clone, PartialEq)]
pub struct Args {
    pub names: Vec<String>,
}

/// General value structure stored in various namespaces.
/// By design these are very cheap to clone, but are largely immutable.
#[derive(Debug, Clone, PartialEq)]
pub enum LongleafValue {
    Float(f64),
    FloatList(Rc<TrackedVector<f64>>),
    FunctionDefinition(Rc<Args>, Rc<Vec<StatementNode>>),
}

impl LongleafValue {
    pub fn type_name(&self) -> &'static str {
        use LongleafValue::*;

        match self {
            Float(_) => "float",
            FloatList(_) => "float vector",
            FunctionDefinition(_, _) => "function definition",
        }
    }
}

impl From<TrackedVector<f64>> for LongleafValue {
    fn from(tv: TrackedVector<f64>) -> LongleafValue {
        LongleafValue::FloatList(Rc::new(tv))
    }
}

impl From<f64> for LongleafValue {
    fn from(f: f64) -> LongleafValue {
        LongleafValue::Float(f)
    }
}

impl fmt::Display for LongleafValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match self {
            LongleafValue::Float(val) => write!(f, "{}", val),
            LongleafValue::FunctionDefinition(args, _expr) => {
                let len = args.names.len();
                let plural = if len > 1 { "s" } else { "" };
                write!(f, "(function which takes {} argument{})", len, plural)
            }
            LongleafValue::FloatList(vals) => {
                write!(f, "[")?;
                if vals.is_empty() {
                    write!(f, "]")?;
                    return Ok(());
                }

                if vals.len() < 10 {
                    let mut iter = vals.iter();
                    write!(f, "{}", iter.next().unwrap())?;

                    for next in iter {
                        write!(f, ", {}", next)?;
                    }
                } else {
                    write!(f, "{}", vals[0])?;

                    for v in &vals[1..5] {
                        write!(f, ", {}", v)?;
                    }

                    write!(f, ", ...")?;

                    for v in &vals[vals.len() - 5..] {
                        write!(f, ", {}", v)?;
                    }
                }

                write!(f, "]")
            }
        }
    }
}
