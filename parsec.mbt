/// A parser combinator
pub type Parser[Token, Value] (Seq[Token]) -> Option[(Value, Seq[Token])]

/// Parses a sequence of tokens
///
/// If the parsing succeeds, `Some(result, rest)` is returned  
/// Otherwise, `None` is returned.
/// TODO: Move to `Result[A, B]`
pub fn Parser::parse[Token, Value](
  self : Parser[Token, Value],
  sequence : Seq[Token]
) -> Option[(Value, Seq[Token])] {
  (self.0)(sequence)
}

/// Parses a token with predicate
///
/// The predicate should return `Some(value)` if the tokens fulfills
/// or `None` otherwise
pub fn pvalue[Token, Value](
  predicate : (Token) -> Option[Value]
) -> Parser[Token, Value] {
  Parser(
    fn(seq) {
      let (hd, rest) = seq.uncons()?
      let value = predicate(hd)?
      Some((value, rest))
    },
  )
}

/// Parser that always fail
pub fn pfail[Token, A]() -> Parser[Token, A] {
  Parser(fn(_tokens) { None })
}

/// Parser that always succeed with given value
pub fn pconst[Token, A](a : A) -> Parser[Token, A] {
  Parser(fn(tokens) { Some((a, tokens)) })
}

pub fn lift2[Token, A, B, C](
  f : (A, B) -> C
) -> (Parser[Token, A], Parser[Token, B]) -> Parser[Token, C] {
  fn(pa, pb) { pa.apply(pb.apply(pconst(fn(b) { fn(a) { f(a, b) } }))) }
}

pub fn sequence[Token, A](
  parser_list : List[Parser[Token, A]]
) -> Parser[Token, List[A]] {
  let consP = lift2(fn(hd, tl) { List::Cons(hd, tl) })
  match parser_list {
    Nil => pconst(Nil)
    Cons(hd, tl) => consP(hd, sequence(tl))
  }
}

pub fn Parser::and_then[Token, A, B](
  self : Parser[Token, A],
  other : Parser[Token, B]
) -> Parser[Token, (A, B)] {
  Parser(
    fn(tokens) {
      let (a, rest) = self.parse(tokens)?
      let (b, rest2) = other.parse(rest)?
      Some(((a, b), rest2))
    },
  )
}

pub fn Parser::or_else[Token, A](
  self : Parser[Token, A],
  other : Parser[Token, A]
) -> Parser[Token, A] {
  Parser(
    fn(tokens) {
      match self.parse(tokens) {
        None => other.parse(tokens)
        Some(_) as result => result
      }
    },
  )
}

pub fn Parser::or_others[Token, A](
  self : Parser[Token, A],
  others : List[Parser[Token, A]]
) -> Parser[Token, A] {
  match others {
    Nil => self
    Cons(hd, tl) => self.or_else(hd.or_others(tl))
  }
}

pub fn Parser::map[Token, A, B](
  self : Parser[Token, A],
  f : (A) -> B
) -> Parser[Token, B] {
  Parser(
    fn(tokens) {
      let (a, rest) = self.parse(tokens)?
      Some((f(a), rest))
    },
  )
}

pub fn Parser::apply[Token, A, B](
  self : Parser[Token, A],
  f : Parser[Token, (A) -> B]
) -> Parser[Token, B] {
  self.and_then(f).map(fn(pair) { (pair.1)(pair.0) })
}

pub fn Parser::repeat_n[Token, A](
  self : Parser[Token, A],
  n : Int
) -> Parser[Token, Array[A]] {
  self.repeat_n_with_sep(n, pconst(()))
}

pub fn Parser::repeat_n_with_sep[Token, A, B](
  self : Parser[Token, A],
  n : Int,
  sep : Parser[Token, B]
) -> Parser[Token, Array[A]] {
  if n < 0 {
    pfail()
  } else if n == 0 {
    pconst(Array::default())
  } else if n == 1 {
    self.map(fn(v) { Array::make(1, v) })
  } else {
    Parser(
      fn(seq) {
        let (hd, tl) = self.parse(seq)?
        let array = Array::make(n, hd)
        let parser = sep.and_then(self).omit_first()
        let mut rest = tl
        for i = 0; i < n - 1; i = i + 1 {
          let (hd, tl) = parser.parse(rest)?
          array[i + 1] = hd
          rest = tl
        }
        Some((array, rest))
      },
    )
  }
}

pub fn Parser::repeat_0_to_n[Token, A](
  self : Parser[Token, A],
  n : Int
) -> Parser[Token, List[A]] {
  self.repeat_0_to_n_with_sep(n, pconst(()))
}

pub fn Parser::repeat_0_to_n_with_sep[Token, A, B](
  self : Parser[Token, A],
  n : Int,
  separator : Parser[Token, B]
) -> Parser[Token, List[A]] {
  if n < 0 {
    pfail()
  } else if n == 0 {
    pconst(Nil)
  } else {
    Parser(
      fn(seq) {
        let sep_self = separator.and_then(self).omit_first()
        match self.parse(seq) {
          None => return Some((Nil, seq))
          Some((hd, tl)) => {
            let mut result = List::Cons(hd, Nil)
            let mut input = tl
            for i = 0; i < n - 1; i = i + 1 {
              match sep_self.parse(input) {
                None => break
                Some((hd, tl)) => {
                  result = Cons(hd, result)
                  input = tl
                }
              }
            }
            Some((reverse(result), input))
          }
        }
      },
    )
  }
}

pub fn Parser::repeat[Token, A](
  self : Parser[Token, A]
) -> Parser[Token, List[A]] {
  self.repeat_with_sep(pconst(()))
}

pub fn Parser::repeat_with_sep[Token, A, B](
  self : Parser[Token, A],
  separator : Parser[Token, B]
) -> Parser[Token, List[A]] {
  Parser(
    fn(seq) {
      let sep_self = separator.and_then(self).omit_first()
      match self.parse(seq) {
        None => return Some((Nil, seq))
        Some((hd, tl)) => {
          let (input, result) = loop tl, List::Cons(hd, Nil) {
            input, result =>
              match sep_self.parse(input) {
                Some((hd, tl)) => continue tl, Cons(hd, result)
                None => (input, result)
              }
          }
          Some((reverse(result), input))
        }
      }
    },
  )
}

test "repeat" {
  let parser = pvalue(
    fn {
      '0' => Some(0)
      '1' => Some(1)
      _ => None
    },
  )
  let repeat_n = parser.repeat_n(3)
  match repeat_n.parse(Seq::from_string("0110")) {
    None => return Err("repeat '0|1' 3 times with '0110' should succeed\n")
    Some((result, _)) => @assertion.assert_eq(result, [0, 1, 1])?
  }
  let repeat_0_to_n = parser.repeat_0_to_n(3)
  match repeat_0_to_n.parse(Seq::from_string("0110")) {
    None =>
      return Err("repeat '0|1' at most 3 times with '0110' should succeed\n")
    Some((result, _)) => @assertion.assert_eq(result, List::[0, 1, 1])?
  }
  match repeat_0_to_n.parse(Seq::from_string("01210")) {
    None =>
      return Err("repeat '0|1' at most 3 times with '01210' should succeed\n")
    Some((result, _)) => @assertion.assert_eq(result, List::[0, 1])?
  }
  match repeat_0_to_n.parse(Seq::from_string("2")) {
    None => return Err("repeat '0|1' at most 3 times with '2' should succeed\n")
    Some((result, _)) => @assertion.assert_eq(result, List::[])?
  }
  let repeat = parser.repeat()
  match repeat.parse(Seq::from_string("0110")) {
    None => return Err("repeat '0|1' any times with '0110' should succeed\n")
    Some((result, _)) => @assertion.assert_eq(result, List::[0, 1, 1, 0])?
  }
}

pub fn Parser::optional[Token, A](
  self : Parser[Token, A]
) -> Parser[Token, Option[A]] {
  self.map(Option::Some).or_else(pconst(None))
}

pub fn Parser::omit_first[Token, A, B](
  parser : Parser[Token, (A, B)]
) -> Parser[Token, B] {
  parser.map(fn(pair) { pair.1 })
}

pub fn Parser::omit_second[Token, A, B](
  parser : Parser[Token, (A, B)]
) -> Parser[Token, A] {
  parser.map(fn(pair) { pair.0 })
}

pub fn Parser::between[Token, A, B](
  self : Parser[Token, A],
  around : Parser[Token, B]
) -> Parser[Token, A] {
  around.and_then(self).omit_first().and_then(around).omit_second()
}

pub fn Parser::ref[Token, A](self : Ref[Parser[Token, A]]) -> Parser[Token, A] {
  Parser(fn(str) { self.val.parse(str) })
}

// helper functions. TODO: replace with std

fn reverse[X](list : List[X]) -> List[X] {
  fn go(acc, xs : List[X]) {
    match xs {
      Nil => acc
      Cons(x, rest) => go((Cons(x, acc) : List[X]), rest)
    }
  }

  go(Nil, list)
}

fn when[T](predicate : Bool, value : T) -> Option[T] {
  if predicate {
    Some(value)
  } else {
    None
  }
}

fn fold_left[A, B](l : List[A], f : (B, A) -> B, b : B) -> B {
  match l {
    Nil => b
    Cons(hd, tl) => fold_left(tl, f, f(b, hd))
  }
}

fn fold_right[A, B](array : Array[A], f : (A, B) -> B, b : B) -> B {
  let mut result = b
  for i = array.length() - 1; i >= 0; i = i - 1 {
    result = f(array[i], result)
  }
  result
}

// String specific parsers

pub fn pchar_such_that(predicate : (Char) -> Bool) -> Parser[Char, Char] {
  pvalue(fn { char => when(predicate(char), char) })
}

pub fn pchar(char : Char) -> Parser[Char, Char] {
  pchar_such_that(fn(c) { c == char })
}

/// parse digit
///
/// %x30-39
pub let pdigit : Parser[Char, Int] = one_of([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

pub fn one_of[A : Show](array : Array[A]) -> Parser[Char, A] {
  fold_right(
    array,
    fn(i, p) { pstring(i.to_string()).map(fn(_str) { i }).or_else(p) },
    pfail(),
  )
}

let pdigits : Parser[Char, (Option[Char], (Int, List[Int]))] = pchar('-').optional().and_then(
  pchar('0').map(fn(_ch) { (0, List::Nil) }).or_else(
    pvalue(
      fn {
        '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' as char =>
          Some(char.to_int() - 0x30)
        _ => None
      },
    ).and_then(pdigit.repeat()),
  ),
)

/// parser for integer
///
/// [ "-" ] ( %x30 / (%x31-39) *(%x30-39)
pub let pint : Parser[Char, Int] = pdigits.map(
  fn(tuple) {
    let sign = tuple.0
    let digits = tuple.1
    let i = fold_left(digits.1, fn(a, b) { 10 * a + b }, digits.0)
    match sign {
      Some(_) => -i
      None => i
    }
  },
)

/// parser for integer 64 bits
///
/// [ "-" ] ( %x30 / (%x31-39) *(%x30-39)
pub let pint64 : Parser[Char, Int64] = pdigits.map(
  fn(tuple) {
    let sign = tuple.0
    let digits = tuple.1
    let i = fold_left(
      digits.1,
      fn(a, b) { 10L * a + b.to_int64() },
      digits.0.to_int64(),
    )
    match sign {
      Some(_) => -i
      None => i
    }
  },
)

test "pint" {
  match pint.parse(Seq::from_string("12345")) {
    Some((12345, _)) => ()
    _ => return Err("match int with '12345' should return 12345")
  }
  match pint.parse(Seq::from_string("-0")) {
    Some((0, _)) => ()
    _ => return Err("match int with '-0' should return 0")
  }
  match pint.parse(Seq::from_string("0")) {
    Some((0, _)) => ()
    _ => return Err("match int with '0' should return 0")
  }
  match pint.parse(Seq::from_string("-01")) {
    Some((0, _)) => ()
    _ => return Err("match int with '-01' should return 0")
  }
  match pint.parse(Seq::from_string("-100")) {
    Some((-100, _)) => ()
    _ => return Err("match int with '-100' should return -100")
  }
}

/// parser that tries to match the given string
pub fn pstring(string : String) -> Parser[Char, String] {
  Parser(
    fn(seq) {
      let iter = loop seq, Seq::from_string(string) {
        iter, input => {
          if input.is_empty().not() {
            let (target, rest_target) = input.uncons()?
            let (char, rest) = iter.uncons()?
            if target != char {
              return None
            }
            continue rest, rest_target
          }
          break iter
        }
      }
      Some((string, iter))
    },
  )
}

test "pstring" {
  let parser = pstring("asdf")
  match parser.parse(Seq::from_string("asdfjkl;")) {
    None => return Err("match 'asdf' with 'asdfjkl;' should succeed\n")
    _ => ()
  }
  match parser.parse(Seq::from_string("jkl;")) {
    Some(_) => return Err("match 'asdf' with 'jkl;' should fail\n")
    _ => ()
  }
}
