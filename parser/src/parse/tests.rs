use super::*;

// Helper so you can use ? which helps the parsers work correctly
// Just make your test be `res_test(|| { your actual code with ? and stuff; ok() })`
fn res_test<'a, T: FnOnce() -> IResult<Span<'a>, ()>>(f: T) {
    let res = f();
    assert!(res.is_ok());
}

fn ok<'a>() -> IResult<Span<'a>, ()> {
    Ok((Span::new(""), ()))
}

/// Make a default span for the purpose of a test fixture.
/// Note that if the arguments are bad, USING the span may result in UB. This is obviously
/// bad, so this is only used for unit tests, and only for making "expected" fixtures.
/// Don't use this span for anything!
fn def_span<'a>(offset: usize, line: u32) -> Span<'a> {
    unsafe { Span::new_from_raw_offset(offset, line, "", ()) }
}

#[test]
fn parse_id_tests() {
    res_test(|| {
        let s = Span::new("fhdjfhjdks fdjksfn \n dsfjnkds 1");

        let (s, actual_1) = parse_id(s)?;
        let (s, _): (_, Span) = multispace1(s)?;
        let (s, actual_2) = parse_id(s)?;
        let (s, _): (_, Span) = multispace1(s)?;
        let (s, actual_3) = parse_id(s)?;

        assert_eq!(s.fragment(), &" 1");

        assert_eq!(
            actual_1,
            cst::IdRef {
                position: def_span(0, 1),
                name: "fhdjfhjdks"
            }
        );
        assert_eq!(actual_1.position.get_column(), 1);

        assert_eq!(
            actual_2,
            cst::IdRef {
                position: def_span(11, 1),
                name: "fdjksfn"
            }
        );
        assert_eq!(actual_2.position.get_column(), 12);

        assert_eq!(
            actual_3,
            cst::IdRef {
                position: def_span(21, 2),
                name: "dsfjnkds"
            }
        );
        assert_eq!(actual_3.position.get_column(), 2);

        ok()
    });
}
