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

pub const MEMORY_CAPACITY: usize = 1 << 32; // 4 GB

#[derive(Copy, Clone, Debug)]
pub struct AllocationError {
    pub length: usize, // in number of elements; mem required is mem::size_of::<f64>() * length
}

pub struct VectorStore {
    internal_store: Rc<RefCell<VectorInternalStore>>,
}

#[derive(Copy, Clone, Debug)]
pub struct MemoryUsageReport {
    bytes_used: usize,
    bytes_reserved: usize,
    free_bytes: usize,
    total_capacity: usize,
}

mod display {
    use super::MemoryUsageReport;
    use std::fmt;

    impl fmt::Display for MemoryUsageReport {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            let mut unit: &str = "bytes";
            let mut divider: usize = 0;

            if self.total_capacity >> divider > 100_000 {
                unit = "kb";
                divider = 10;
            }

            if self.total_capacity >> divider > 100_000 {
                unit = "mb";
                divider = 20;
            }

            if self.total_capacity >> divider > 100_000 {
                unit = "gb";
                divider = 30;
            }

            writeln!(f, "Memory Usage:")?;
            writeln!(
                f,
                "  Memory Used ({}): {}",
                unit,
                self.bytes_used >> divider
            )?;
            writeln!(
                f,
                "  Memory Reserved ({}): {}",
                unit,
                self.bytes_reserved >> divider
            )?;
            writeln!(
                f,
                "  Memory Available ({}): {}",
                unit,
                self.free_bytes >> divider
            )?;
            writeln!(
                f,
                "  Total Capacity ({}): {}",
                unit,
                self.total_capacity >> divider
            )
        }
    }
}

struct VectorInternalStore {
    // Amount of memory used for objects which are in active use.
    bytes_used: usize,
    // Amount of memory used for objects which are available for
    // reuse, but which are not currently in use.
    bytes_reserved: usize,
    // Total amount of bytes allowed to be allocated (total)
    memory_capacity: usize,
    stored_vectors: HashMap<usize, Vec<Vec<f64>>>,
}

impl VectorStore {
    pub fn new() -> Self {
        VectorStore {
            internal_store: Rc::new(RefCell::new(VectorInternalStore {
                stored_vectors: HashMap::new(),
                bytes_reserved: 0,
                bytes_used: 0,
                memory_capacity: MEMORY_CAPACITY,
            })),
        }
    }

    pub fn get_memory_usage(&self) -> MemoryUsageReport {
        self.internal_store.as_ref().borrow().get_memory_usage()
    }

    pub fn garbage_collect(&self) {
        self.internal_store.as_ref().borrow_mut().garbage_collect();
    }

    pub fn track_vector(&self, data: Vec<f64>) -> TrackedVector {
        self.internal_store.as_ref().borrow_mut().bytes_used +=
            std::mem::size_of::<f64>() * data.len();

        TrackedVector {
            data,
            home_store: Rc::downgrade(&self.internal_store),
        }
    }

    pub fn get_vector(&self, length: usize) -> Result<TrackedVector, AllocationError> {
        let data: Vec<f64> = self
            .internal_store
            .as_ref()
            .borrow_mut()
            .allocate_vector(length)?;

        Ok(TrackedVector {
            data,
            home_store: Rc::downgrade(&self.internal_store),
        })
    }
}

impl VectorInternalStore {
    fn garbage_collect(&mut self) {
        self.stored_vectors.clear();
        self.bytes_reserved = 0;
    }

    fn get_memory_usage(&self) -> MemoryUsageReport {
        MemoryUsageReport {
            bytes_used: self.bytes_used,
            bytes_reserved: self.bytes_reserved,
            free_bytes: self.memory_capacity - self.bytes_reserved - self.bytes_used,
            total_capacity: self.memory_capacity,
        }
    }

    fn return_vector(&mut self, data: &mut TrackedVector) {
        let actual_data: Vec<f64> = std::mem::replace(&mut data.data, Vec::new());

        // Leaving this in here because I'm not sure which we should actually use,
        // but also currently no operations change the length of any TrackedVector,
        // so it shouldn't come up.
        assert_eq!(actual_data.len(), actual_data.capacity());

        let length = actual_data.len();

        let size = std::mem::size_of::<f64>() * length;

        assert!(self.bytes_used >= size);
        self.bytes_used -= size;
        self.bytes_reserved += size;

        if length > 0 {
            // just push the data back onto the map to reuse
            self.stored_vectors
                .entry(length)
                .or_default()
                .push(actual_data);
        }
    }

    // Error type is () because there is currently only one failure state
    // (insufficient memory) but leaving it in for now. Note that this returns
    // a Vec, assuming it will be tracked; the caller must make sure to link it
    fn allocate_vector(&mut self, length: usize) -> Result<Vec<f64>, AllocationError> {
        let mem_size = std::mem::size_of::<f64>() * length;

        if let Some(old) = self.stored_vectors.entry(length).or_default().pop() {
            self.bytes_used += mem_size;

            assert!(self.bytes_reserved >= mem_size);

            self.bytes_reserved -= mem_size;

            return Ok(old);
        }

        if self.bytes_used + self.bytes_reserved + mem_size <= self.memory_capacity {
            let new = vec![0.; length];
            self.bytes_used += mem_size;

            return Ok(new);
        }

        if self.bytes_used + mem_size <= self.memory_capacity {
            self.garbage_collect();

            let new = vec![0.; length];
            self.bytes_used += mem_size;

            return Ok(new);
        }

        Err(AllocationError { length })
    }
}

pub struct TrackedVector {
    data: Vec<f64>,
    home_store: Weak<RefCell<VectorInternalStore>>,
}

impl PartialEq for TrackedVector {
    fn eq(&self, other: &TrackedVector) -> bool {
        self.data == other.data
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
            vis.as_ref().borrow_mut().return_vector(self);
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

        let mut a = store.get_vector(10).unwrap();
        let mut b = store.get_vector(10).unwrap();

        b[5] = 5.;
        a[4] = 4.;

        std::mem::drop(a);

        // c will just be a
        let c = store.get_vector(10).unwrap();

        assert_eq!(c[4], 4.);
        assert_eq!(c[5], 0.);

        std::mem::drop(b);

        // d will just be b
        let d = store.get_vector(10).unwrap();

        assert_eq!(d[4], 0.);
        assert_eq!(d[5], 5.);

        std::mem::drop(d);
        std::mem::drop(c);

        // e will be c, which is a, since it was dropped most recently
        let e = store.get_vector(10).unwrap();

        assert_eq!(e[4], 4.);
        assert_eq!(e[5], 0.);
    }
}
