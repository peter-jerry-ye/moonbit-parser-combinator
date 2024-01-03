struct Tokens {
  bytes : Bytes
  offset : Int
}

pub struct Parser[T] {
  priv parse : (Tokens) -> Option[(T, Tokens)]
}

pub fn parse[T](self : Parser[T], bytes : Bytes) -> Option[(T, Bytes)] {
  match (self.parse)({ bytes, offset: 0 }) {
    Some(t, tokens) => Some(
      t,
      tokens.bytes.sub_string(
        tokens.offset,
        tokens.bytes.length() - tokens.offset,
      ).to_bytes(),
    )
    None => None
  }
}

pub fn pstring(string: String) -> Parser[String] {
  {
    parse: fn(str: Tokens) {
      let bytes = string.to_bytes()
      if str.bytes.length() - str.offset < bytes.length() {
        None
      } else {
        var i = 0
        var has_result = true
        let length = bytes.length()
        while i < length, i = i + 1 {
          if bytes[i] != str.bytes[str.offset + i] {
            has_result = false
            break
          }
        }
        if has_result {
          Some(string, {..str, offset: str.offset + length})
        } else {
          None
        }
      }
    }
  }
}

pub fn pchar(char : Char) -> Parser[Char] {
  pstring(char.to_string()).map(fn (_str) { char })
}

pub fn and_then[A, B](self : Parser[A], other : Parser[B]) -> Parser[(A, B)] {
  {
    parse: fn(str : Tokens) -> Option[((A, B), Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) => match (other.parse)(tokens) {
          Some(b, rest) => Some((a, b), rest)
          None => None
        }
        None => None
      }
    },
  }
}

pub fn or_else[A](self : Parser[A], other : Parser[A]) -> Parser[A] {
  {
    parse: fn(str : Tokens) -> Option[(A, Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) => Some(a, tokens)
        None => (other.parse)(str)
      }
    },
  }
}

pub fn or_others[A](self : Parser[A], others : List[Parser[A]]) -> Parser[A] {
  match others {
    Nil => self
    Cons(hd, tl) => self.or_else(hd.or_others(tl))
  }
}

pub fn map[A, B](self : Parser[A], f : (A) -> B) -> Parser[B] {
  {
    parse: fn(str : Tokens) -> Option[(B, Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) => Some(f(a), tokens)
        None => None
      }
    },
  }
}

fn fold_right[A, B](array: Array[A], f: (A, B) -> B, b: B) -> B {
  var i = array.length() - 1
  var result = b
  while i >= 0, i = i - 1 {
    result = f(array[i], result)
  }
  result
}

pub fn one_of[A: Show](array: Array[A]) -> Parser[A] {
  fold_right(array, fn (i, p) {
    pstring(i.to_string()).map(fn (_str) { i }).or_else(p)
  }, pfail())
}

pub let pdigit: Parser[Int] = one_of([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

pub fn pfail[A]() -> Parser[A] {
  {
    parse: fn (_tokens) { None }
  }
}

pub fn pconst[A](a: A) -> Parser[A] {
  {
    parse: fn (tokens) { Some(a, tokens) }
  }
}

pub fn apply[A, B](self: Parser[A], f: Parser[(A) -> B]) -> Parser[B] {
  self.and_then(f).map(fn (pair) { pair.1(pair.0) })
}

pub fn lift2[A, B, C](f: (A, B) -> C) -> (Parser[A], Parser[B]) -> Parser[C] {
  fn (pa, pb) {
    pa.apply(pb.apply(pconst(fn (b) { fn (a) { f(a, b) }})))
  }
}

pub fn sequence[A](parser_list: List[Parser[A]]) -> Parser[List[A]] {
  let consP = lift2(fn (hd, tl) { List::Cons(hd, tl) })
  match parser_list {
    Nil => pconst(Nil)
    Cons(hd, tl) => consP(hd, sequence(tl))
  }
}

pub fn many[A](self: Parser[A]) -> Parser[List[A]] {
  fn parse_zero_or_more(input) -> (List[A], Tokens) {
    match (self.parse)(input) {
      None => (Nil, input)
      Some(a, rest) => {
        let (ls, remain) = parse_zero_or_more(rest)
        (Cons(a, ls), remain)
      }
    }
  }
  {
    parse: fn (str) {
      Some(parse_zero_or_more(str))
    }
  }
}

pub fn one_or_many[A](self: Parser[A]) -> Parser[(A, List[A])] {
  self.and_then(self.many())
}

pub fn optional[A](self: Parser[A]) -> Parser[Option[A]] {
  self.map(Option::Some).or_else(pconst(None))
}

fn fold_left[A, B](l: List[A], f: (B, A) -> B, b: B) -> B {
  match l {
    Nil => b
    Cons(hd, tl) => fold_left(tl, f, f(b, hd))
  }
}

pub let pint: Parser[Int] = pchar('-').optional().and_then(pdigit.one_or_many()).map(
  fn (tuple) {
    let sign = tuple.0
    let digits = tuple.1
    let i = fold_left(digits.1, fn (a, b) { 10 * a + b }, digits.0)
    match sign {
      Some(_) => -i
      None => i
    }
  }
)

pub fn omit_first[A, B](parser: Parser[(A, B)]) -> Parser[B] {
  parser.map(fn (pair) { pair.1 })
}
pub fn omit_second[A, B](parser: Parser[(A, B)]) -> Parser[A] {
  parser.map(fn (pair) { pair.0 })
}
pub fn between[A, B](self: Parser[A], around: Parser[B]) -> Parser[A] {
  omit_second(omit_first(around.and_then(self)).and_then(around))
}

pub fn separated_by1[A, B](self: Parser[A], separator: Parser[B]) -> Parser[(A, List[A])] {
  let sep_then = omit_first(separator.and_then(self))
  self.and_then(sep_then.many())
}

pub fn separated_by[A, B](self: Parser[A], separator: Parser[B]) -> Parser[List[A]] {
  self.separated_by1(separator).map(fn (pair) { List::Cons(pair.0, pair.1) }).or_else(pconst(Nil))
}