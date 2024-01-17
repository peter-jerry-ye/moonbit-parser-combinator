fn init {
  @assertion.tests(
    fn(check, _validate) {
      let parser = pstring("Hello").and_then(pint)
      let Some(result, rest) = parser.parse("Hello1234ASDF".to_bytes())
      check("parse string and int").assert_value(result.0).should_be("Hello").assert_value(
        result.1,
      ).should_be(1234).assert_value(rest.to_string()).should_be("ASDF").check_test()
      let parser = pchar('H')
      let Some(result, rest) = parser.parse("Hello".to_bytes())
      check("parse char").assert_value(result).should_be('H').assert_value(
        rest.to_string(),
      ).should_be("ello").check_test()
      let parser = pchar('中')
      let Some(result, rest) = parser.parse("中文".to_bytes())
      check("parse utf8 char").assert_value(result).should_be('中').assert_value(
        rest.to_string(),
      ).should_be("文").check_test()
    },
  ).execute_with_abort(println)
}
