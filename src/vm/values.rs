use std::rc::Rc;

use super::store::TrackedVector;
use crate::parser::ExprNode;

#[derive(Debug, Clone, PartialEq)]
pub struct Args {
    pub names: Vec<String>,
}

/// General value structure stored in various namespaces.
/// By design these are very cheap to clone, but are largely immutable.
#[derive(Debug, Clone, PartialEq)]
pub enum LongleafValue {
    Float(f64),
    FloatList(Rc<TrackedVector>),
    FunctionDefinition(Rc<Args>, Rc<ExprNode>),
}

impl From<TrackedVector> for LongleafValue {
    fn from(tv: TrackedVector) -> LongleafValue {
        LongleafValue::FloatList(Rc::new(tv))
    }
}

impl From<f64> for LongleafValue {
    fn from(f: f64) -> LongleafValue {
        LongleafValue::Float(f)
    }
}
