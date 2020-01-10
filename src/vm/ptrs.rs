pub struct SyncMutPointer<T> {
    pub ptr: *mut T,
}

unsafe impl<T> Send for SyncMutPointer<T> {}
unsafe impl<T> Sync for SyncMutPointer<T> {}

impl<T> From<*mut T> for SyncMutPointer<T> {
    fn from(ptr: *mut T) -> Self {
        SyncMutPointer { ptr }
    }
}
