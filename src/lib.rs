#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;

mod internal_store;
pub mod parser;
mod values;
pub mod vm;
mod zero;

#[macro_use]
mod macros;
