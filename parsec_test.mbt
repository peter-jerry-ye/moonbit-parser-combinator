fn init {
  @assertion.tests(
    fn(check, _validate) {
      let parser = pstring("Hello").and_then(pint)
      let Some(result, rest) = parser.parse("Hello1234ASDF".to_bytes())
      check("parser test 1")
        .assert_value(result.0).should_be("Hello")
        .assert_value(result.1).should_be(1234)
        .assert_value(rest.to_string()).should_be("ASDF")
        .check_test()
    },
  ).execute_with_abort(println)
}
