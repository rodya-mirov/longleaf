use std::any::Any;

pub trait Zero: Copy + Sized + Any {
    fn zero() -> Self;
}

impl<T> Zero for T
where
    T: Default + Copy + Sized + Any,
{
    fn zero() -> Self {
        T::default()
    }
}
