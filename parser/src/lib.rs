use nom_locate::LocatedSpan;

// CST -- AST nodes enriched with enough data to recover the original source code
// Available for linting, pretty printing, error reporting, etc.
mod cst;

// Turn text into cst nodes
mod parse;

// Turn CST nodes into AST nodes
mod process;

/// From nom_locate. Used to identify where in the
pub type Span<'a> = LocatedSpan<&'a str>;

pub fn parse_stmt_ast(
    text: &str,
) -> Result<lang::ast::StmtNode, nom::Err<nom::error::Error<Span>>> {
    let s = Span::new(text);
    let (s, parsed) = parse::parse_statement(s)?;
    let _ = nom::combinator::eof(s)?;

    Ok(parsed.into())
}
