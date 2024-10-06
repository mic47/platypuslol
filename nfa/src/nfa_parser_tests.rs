use super::*;

#[test]
fn test_match_string() {
    let parser = NFA::match_string("foo").with_payload_for_final_nodes(&());
    assert_eq!(parser.parse("foo"), vec![(&(), "")],);
    assert_eq!(parser.parse("foobar"), vec![(&(), "bar")],);
    assert_eq!(parser.parse(""), vec![]);
    assert_eq!(parser.parse("f"), vec![]);
}

#[test]
fn test_non_empty_prefixes() {
    let parser = NFA::match_non_empty_prefixes("foo").with_payload_for_final_nodes(&());
    let empty: Vec<&()> = vec![];
    assert_eq!(parser.parse("f"), vec![(&(), "")],);
    assert_eq!(parser.parse_full_for_tests("foo"), vec![&()]);
    assert_eq!(parser.parse_full_for_tests("fo"), vec![&()],);
    assert_eq!(parser.parse_full_for_tests("foobar"), empty);
    assert_eq!(parser.parse_full_for_tests("fobar"), empty);
    assert_eq!(parser.parse("fbar"), vec![(&(), "bar")],);
    assert_eq!(parser.parse("obar"), vec![],);
    assert_eq!(parser.parse("bar"), vec![],);
    assert_eq!(parser.parse(""), vec![]);
}

#[test]
fn test_chain_2_elements() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_non_empty_prefixes("bar"),
    ])
    .with_payload_for_final_nodes(&());
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full_for_tests("f"), empty);
    assert_eq!(parser.parse_full_for_tests("fb"), found);
    assert_eq!(parser.parse_full_for_tests("fobar"), found);
    assert_eq!(parser.parse_full_for_tests("foobar"), found);
    assert_eq!(parser.parse_full_for_tests("fooar"), empty);
    assert_eq!(parser.parse_full_for_tests("foobar_extra"), empty);
    assert_eq!(parser.parse("foob_extra"), vec![(&(), "_extra")]);
}

#[test]
fn test_chain_3_elements_with_non_zero_space() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_one_or_more_spaces(),
        NFA::match_non_empty_prefixes("bar"),
    ])
    .with_payload_for_final_nodes(&());
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full_for_tests("f"), empty);
    assert_eq!(parser.parse_full_for_tests("f b"), found);
    assert_eq!(parser.parse_full_for_tests("fb"), empty);
    assert_eq!(parser.parse_full_for_tests("fo bar"), found);
    assert_eq!(parser.parse_full_for_tests("fobar"), empty);
    assert_eq!(parser.parse_full_for_tests("foo bar"), found);
    assert_eq!(parser.parse_full_for_tests("fooar"), empty);
    assert_eq!(parser.parse_full_for_tests("foobar_extra"), empty);
    assert_eq!(parser.parse("foo b_extra"), vec![(&(), "_extra")]);
    assert_eq!(parser.parse("foob_extra"), vec![]);
}

#[test]
fn test_chain_3_elements_with_zero_space() {
    let parser = NFA::chain(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_zero_or_more_spaces(),
        NFA::match_non_empty_prefixes("bar"),
    ])
    .with_payload_for_final_nodes(&());
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full_for_tests("f"), empty);
    assert_eq!(parser.parse_full_for_tests("f b"), found);
    assert_eq!(parser.parse_full_for_tests("fb"), found);
    assert_eq!(parser.parse_full_for_tests("fo bar"), found);
    assert_eq!(parser.parse_full_for_tests("fobar"), found);
    assert_eq!(parser.parse_full_for_tests("foo bar"), found);
    assert_eq!(parser.parse_full_for_tests("fooar"), empty);
    assert_eq!(parser.parse_full_for_tests("foobar_extra"), empty);
    assert_eq!(parser.parse("foo b_extra"), vec![(&(), "_extra")]);
    assert_eq!(parser.parse("foob_extra"), vec![(&(), "_extra")]);
}

#[test]
fn test_any_of() {
    let parser = NFA::any_of(&[
        NFA::match_non_empty_prefixes("foo"),
        NFA::match_non_empty_prefixes("feee"),
        NFA::match_string("bar"),
    ])
    .with_payload_for_final_nodes(&());
    let empty: Vec<&()> = vec![];
    let found = vec![&()];
    assert_eq!(parser.parse_full_for_tests("f"), vec![&(), &()]);
    assert_eq!(parser.parse_full_for_tests("b"), empty);
    assert_eq!(parser.parse_full_for_tests("bar"), found);
    assert_eq!(parser.parse_full_for_tests("fe"), found);

    assert_eq!(parser.parse("f"), vec![(&(), ""), (&(), "")]);

    assert_eq!(parser.parse("fbar"), vec![(&(), "bar"), (&(), "bar")]);
    assert_eq!(parser.parse(""), vec![]);
}
