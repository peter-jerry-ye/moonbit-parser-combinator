# Parser combinator

A simple parser combinator 简单的语法分析组合子

## Usage 使用方法

```moonbit
fn init {
  let parser = pstring("Hello").and_then(pint)
  let Some(result, _) = parser.parse("Hello1234".to_bytes())
  println(result.0 + " " + result.1.to_string())
}
```