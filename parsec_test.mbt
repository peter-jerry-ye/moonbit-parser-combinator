fn init {
  @assertion.tests(
    fn(check, _validate) {
      let parser = pstring("Hello").and_then(pint)
      let (result, rest) = parser.parse("Hello1234ASDF".to_bytes()).unwrap()
      check("parse string and int").assert_value(result.0).should_be("Hello").assert_value(
        result.1,
      ).should_be(1234).assert_value(rest.to_string()).should_be("ASDF").check_test()
      let parser = pchar('H')
      let (result, rest) = parser.parse("Hello".to_bytes()).unwrap()
      check("parse char").assert_value(result).should_be('H').assert_value(
        rest.to_string(),
      ).should_be("ello").check_test()
      let parser = pchar('中')
      let (result, rest) = parser.parse("中文".to_bytes()).unwrap()
      check("parse utf8 char").assert_value(result).should_be('中').assert_value(
        rest.to_string(),
      ).should_be("文").check_test()
      let parser = pchar('n').repeat_n(3)
      let (result, rest) = parser.parse("nnnn".to_bytes()).unwrap()
      check("parse repeat exact").assert_value(result).should(
        fn(array) {
          array.length() == 3 && array[0] == 'n' && array[1] == 'n' && array[2] ==
          'n'
        },
        fn(array) { "\(array) should be [n, n, n]" },
      ).assert_value(rest.to_string()).should_be("n").check_test()
      let parser = pchar('n').repeat_n_with_sep(3, pchar(','))
      let (result, rest) = parser.parse("n,n,n,n".to_bytes()).unwrap()
      check("parse repeat exact with separator").assert_value(result).should(
        fn(array) {
          array.length() == 3 && array[0] == 'n' && array[1] == 'n' && array[2] ==
          'n'
        },
        fn(array) { "\(array) should be [n, n, n]" },
      ).assert_value(rest.to_string()).should_be(",n").check_test()
    },
  ).execute_with_abort(println)
}
