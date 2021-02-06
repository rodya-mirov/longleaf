use nom_locate::LocatedSpan;

mod cst;

mod parse;

/// From nom_locate. Used to identify where in the
pub(crate) type Span<'a> = LocatedSpan<&'a str>;
