fn init {
  let parser = pstring("Hello").and_then(pint)
  let Some(result, _) = parser.parse("Hello1234".to_bytes())
  println(result.0 + " " + result.1.to_string())
}
