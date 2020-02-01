use std::any::{Any, TypeId};
use std::borrow::{Borrow, BorrowMut};
use std::collections::HashMap;
use std::ops::{Deref, DerefMut};
use std::sync::mpsc::{channel, Receiver, Sender};

use crate::zero::Zero;

#[derive(Copy, Clone, Debug)]
pub struct AllocationError {
    pub num_elements: usize,
    pub mem_required: usize, // in bytes
}

/// Public interface to a vector interning system which is generic and tracks
/// memory usage in a uniform way.
///
/// Provides vectors which, when dropped, return to the caches internal to this
/// structure, for reuse. If allocation goes over the set capacity, will trigger
/// a GC (clean up) immediately; this can also be done manually.
pub struct VectorStore {
    update_tx_to_clone: Sender<UpdateMessage>,
    update_rx: Receiver<UpdateMessage>,
    // allocated to things that are not in the cache, but will be returned
    // Note that in some cases this can exceed memory_capacity, e.g. if you
    // track a vector that brings the total above memory_capacity
    bytes_used: usize,
    internal_store: InternalStore,
    // will garbage collect if allocation requires more than this space
    // will throw an error if that still isn't enough
    memory_capacity: usize,
}

// TODO: bad name
/// Tracks the "A vector has been dropped, please pick it up, thanks" message
struct UpdateMessage(TypeId, Box<dyn Any>);

#[derive(Default)]
struct InternalStore {
    // allocated to things that are sitting in the cache. Just a convenience
    // function so that we don't have to sweep through all the values in self.caches
    // and count them.
    bytes_reserved: usize,
    // PRECONDITION REQUIRED
    // should always be a map from T -> HashMap<usize, Vec<T>>
    caches: HashMap<TypeId, Box<dyn Any>>,
}

pub struct TrackedVector<T>
where
    T: Zero,
{
    data: Vec<T>,
    drop_tx: Sender<UpdateMessage>,
}

fn size_of<T: Sized>(vec: &Vec<T>) -> usize {
    std::mem::size_of::<T>() * vec.capacity()
}

type CacheType<T> = HashMap<usize, Vec<Vec<T>>>;

impl VectorStore {
    pub fn new(memory_capacity: usize) -> Self {
        let (update_tx_to_clone, update_rx) = channel();
        let bytes_used = 0;
        VectorStore {
            bytes_used,
            memory_capacity,
            internal_store: Default::default(),
            update_rx,
            update_tx_to_clone,
        }
    }

    pub fn get_memory_usage(&self) -> MemoryUsageReport {
        let bytes_used = self.memory_used();
        let bytes_used_or_reserved = self.memory_used_or_reserved();
        let free_bytes = self.memory_capacity - bytes_used_or_reserved;

        MemoryUsageReport {
            bytes_used,
            bytes_reserved: bytes_used_or_reserved - bytes_used,
            free_bytes,
            total_capacity: self.memory_capacity,
        }
    }

    pub fn track_vector<T: Zero>(&mut self, data: Vec<T>) -> TrackedVector<T> {
        self.process_messages();

        self.bytes_used += size_of(&data);

        if self.memory_used_or_reserved() > self.memory_capacity {
            self.garbage_collect();
        }

        TrackedVector {
            data,
            drop_tx: self.update_tx_to_clone.clone(),
        }
    }

    pub fn garbage_collect(&mut self) {
        self.process_messages();
        self.internal_store.garbage_collect();
    }

    fn memory_used_or_reserved(&self) -> usize {
        let used = self.bytes_used;

        let reserved = self.internal_store.bytes_reserved;

        used + reserved
    }

    fn memory_used(&self) -> usize {
        self.bytes_used
    }

    fn process_messages(&mut self) {
        use std::sync::mpsc::TryRecvError;

        loop {
            match self.update_rx.try_recv() {
                Ok(update_msg) => {
                    self.process_message(update_msg);
                }
                Err(TryRecvError::Empty) => {
                    return;
                }
                Err(TryRecvError::Disconnected) => {
                    panic!("Garbage collector is broken; execution cannot continue");
                }
            }
        }
    }

    fn process_message(&mut self, msg: UpdateMessage) {
        let type_id: TypeId = msg.0;
        let content: Box<dyn Any> = msg.1;

        match content.downcast::<Vec<f64>>() {
            Ok(floats) => {
                self.return_vector(*floats);
            }
            Err(not_floats) => match not_floats.downcast::<Vec<bool>>() {
                Ok(bools) => {
                    self.return_vector(*bools);
                }
                Err(not_bools) => {
                    panic!("Received dropped vector of elem_type {:?} and full type {:?}, but have no code to handle that", type_id, not_bools.type_id());
                }
            },
        }
    }

    fn return_vector<T: Zero>(&mut self, data: Vec<T>) {
        let full_size = size_of(&data);

        assert!(self.bytes_used >= full_size);

        self.bytes_used -= full_size;
        self.internal_store.return_vector(data);
    }

    pub fn get_vector<T: Zero>(
        &mut self,
        length: usize,
    ) -> Result<TrackedVector<T>, AllocationError> {
        self.process_messages();

        let used = self.memory_used();
        let used_or_reserved = self.memory_used_or_reserved();

        let maybe_cached = self.internal_store.get_cached_vector(length);

        if let Some(cached) = maybe_cached {
            let tv = self.track_vector(cached);

            return Ok(tv);
        }

        let size_needed = std::mem::size_of::<T>() * length;

        if size_needed + used_or_reserved <= self.memory_capacity {
            let out = vec![T::zero(); length];

            let tv = self.track_vector(out);

            return Ok(tv);
        }

        if size_needed + used <= self.memory_capacity {
            self.garbage_collect();

            let out = vec![T::zero(); length];

            let tv = self.track_vector(out);

            return Ok(tv);
        }

        Err(AllocationError {
            num_elements: length,
            mem_required: size_needed,
        })
    }
}

impl InternalStore {
    fn garbage_collect(&mut self) {
        self.bytes_reserved = 0;
        self.caches.clear();
    }

    fn return_vector<T: Zero>(&mut self, mut data: Vec<T>) {
        data.shrink_to_fit();

        let size = size_of(&data);
        self.bytes_reserved += size;

        let kind = TypeId::of::<T>();

        let any_cache: &mut Box<dyn Any> = self
            .caches
            .entry(kind)
            .or_insert_with(|| Box::new(CacheType::<T>::new()));

        let cache: &mut CacheType<T> = any_cache
            .downcast_mut()
            .expect("Invariant violated; downcast should work");

        cache.entry(data.len()).or_default().push(data);
    }

    fn get_cached_vector<T: Zero>(&mut self, length: usize) -> Option<Vec<T>> {
        let kind = TypeId::of::<T>();

        let any_cache: &mut Box<dyn Any> = self
            .caches
            .entry(kind)
            .or_insert_with(|| Box::new(CacheType::<T>::new()));

        let cache: &mut CacheType<T> = any_cache
            .downcast_mut()
            .expect("Invariant violated; downcast should work");

        if let Some(old) = cache.entry(length).or_default().pop() {
            self.bytes_reserved -= size_of(&old);
            Some(old)
        } else {
            None
        }
    }
}

impl<T: PartialEq + Zero> PartialEq for TrackedVector<T> {
    fn eq(&self, other: &TrackedVector<T>) -> bool {
        self.data == other.data
    }
}

impl<T: Eq + Zero> Eq for TrackedVector<T> {}

impl<T: std::fmt::Debug + Zero> std::fmt::Debug for TrackedVector<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.data)
    }
}

impl<T: Zero> Drop for TrackedVector<T> {
    fn drop(&mut self) {
        let elem_type = TypeId::of::<T>();
        let data = std::mem::replace(&mut self.data, Vec::new());

        // The only error from send is "the receiver is dead" which returns the data;
        // if the data comes back, we just drop it, done
        let _ = self.drop_tx.send(UpdateMessage(elem_type, Box::new(data)));
    }
}

impl<T: Zero> Borrow<Vec<T>> for TrackedVector<T> {
    fn borrow(&self) -> &Vec<T> {
        &self.data
    }
}

impl<T: Zero> BorrowMut<Vec<T>> for TrackedVector<T> {
    fn borrow_mut(&mut self) -> &mut Vec<T> {
        &mut self.data
    }
}

impl<T: Zero> Deref for TrackedVector<T> {
    type Target = Vec<T>;

    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T: Zero> DerefMut for TrackedVector<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
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
