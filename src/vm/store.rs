/// A "value store" so we can reuse very large vectors when they come up.
/// The idea is that you can "get" a new vector from the store, which may
/// or may not actually be new, but will have the correct length (and
/// therefore some junk data in there, too). You can then use it without
/// worrying about the special structure, but when you drop it, it will
/// go back to the store.
use std::borrow::{Borrow, BorrowMut};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::rc::{Rc, Weak};

use super::{EvalValue, FloatListValue};

pub struct VectorStore {
    internal_store: Rc<VectorInternalStore>,
}

struct VectorInternalStore {
    stored_vectors: RefCell<HashMap<usize, Vec<Vec<f64>>>>,
}

impl VectorStore {
    pub fn new() -> Self {
        VectorStore {
            internal_store: Rc::new(VectorInternalStore {
                stored_vectors: RefCell::new(HashMap::new()),
            }),
        }
    }

    pub fn get_vector(&self, length: usize) -> TrackedVector {
        let stored: Option<Vec<f64>> = self
            .internal_store
            .as_ref()
            .stored_vectors
            .borrow_mut()
            .get_mut(&length)
            .map(|v| v.pop())
            .flatten();

        let data: Vec<f64> = stored.unwrap_or_else(|| vec![0.0; length]);

        TrackedVector {
            data,
            home_store: Rc::downgrade(&self.internal_store),
        }
    }
}

impl VectorInternalStore {
    fn return_vector(&self, data: &mut TrackedVector) {
        let actual_data: Vec<f64> = std::mem::replace(&mut data.data, Vec::new());

        let length = actual_data.len();

        if length > 0 {
            // just push the data back onto the map to reuse
            self.stored_vectors
                .borrow_mut()
                .entry(length)
                .or_default()
                .push(actual_data);
        }
    }
}

pub struct TrackedVector {
    data: Vec<f64>,
    home_store: Weak<VectorInternalStore>,
}

impl Into<EvalValue> for TrackedVector {
    fn into(self) -> EvalValue {
        EvalValue::FloatList(Box::new(self))
    }
}

impl FloatListValue for TrackedVector {
    fn to_float_list(self: Box<Self>) -> Rc<Vec<f64>> {
        let mut actual: TrackedVector = *self;
        let data = std::mem::replace(&mut actual.data, Vec::new());
        Rc::new(data)
    }
}

impl std::fmt::Debug for TrackedVector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl Drop for TrackedVector {
    fn drop(&mut self) {
        if let Some(vis) = self.home_store.upgrade() {
            vis.as_ref().return_vector(self);
        }
    }
}

impl Borrow<Vec<f64>> for TrackedVector {
    fn borrow(&self) -> &Vec<f64> {
        &self.data
    }
}

impl BorrowMut<Vec<f64>> for TrackedVector {
    fn borrow_mut(&mut self) -> &mut Vec<f64> {
        &mut self.data
    }
}

impl Deref for TrackedVector {
    type Target = Vec<f64>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl DerefMut for TrackedVector {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_repetition() {
        let store = VectorStore::new();

        let mut a = store.get_vector(10);
        let mut b = store.get_vector(10);

        b[5] = 5.;
        a[4] = 4.;

        std::mem::drop(a);

        // c will just be a
        let c = store.get_vector(10);

        assert_eq!(c[4], 4.);
        assert_eq!(c[5], 0.);

        std::mem::drop(b);

        // d will just be b
        let d = store.get_vector(10);

        assert_eq!(d[4], 0.);
        assert_eq!(d[5], 5.);

        std::mem::drop(d);
        std::mem::drop(c);

        // e will be c, which is a, since it was dropped most recently
        let e = store.get_vector(10);

        assert_eq!(e[4], 4.);
        assert_eq!(e[5], 0.);
    }
}
