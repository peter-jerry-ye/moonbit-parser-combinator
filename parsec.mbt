priv struct Tokens {
  bytes : Bytes
  offset : Int
}

pub struct Parser[T] {
  priv parse : (Tokens) -> Option[(T, Tokens)]
}

pub fn Parser::parse[T](self : Parser[T], bytes : Bytes) -> Option[(T, Bytes)] {
  match (self.parse)({ bytes, offset: 0 }) {
    Some(t, tokens) =>
      Some(
        t,
        tokens.bytes.sub_string(
          tokens.offset,
          tokens.bytes.length() - tokens.offset,
        ).to_bytes(),
      )
    None => None
  }
}

pub fn pstring(string : String) -> Parser[String] {
  {
    parse: fn(str : Tokens) {
      let bytes = string.to_bytes()
      if str.bytes.length() - str.offset < bytes.length() {
        None
      } else {
        let mut i = 0
        let mut has_result = true
        let length = bytes.length()
        while i < length, i = i + 1 {
          if bytes[i] != str.bytes[str.offset + i] {
            has_result = false
            break
          }
        }
        if has_result {
          Some(string, {..str, offset: str.offset + length })
        } else {
          None
        }
      }
    },
  }
}

pub fn pchar_such_that(predicate : (Char) -> Bool) -> Parser[Char] {
  {
    parse: fn(str : Tokens) {
      let { offset, bytes } = str
      let length = bytes.length()
      if offset < length {
        let code = bytes[offset]
        let (char, tokens) = (
          if code < 0b1100_0000 {
            Some(Char::from_int(code), {..str, offset: offset + 1 })
          } else if code < 0b1110_0000 && offset + 1 < length {
            let code2 = bytes[offset + 1]
            Some(
              Char::from_int(code.land(0x1F).lsl(6).lor(code2.land(0x3F))),
              {..str, offset: offset + 2 },
            )
          } else if code < 0b1111_0000 && offset + 2 < length {
            let code2 = bytes[offset + 1]
            let code3 = bytes[offset + 2]
            Some(
              Char::from_int(
                code.land(0xF).lsl(12).lor(code2.land(0x3F).lsl(6)).lor(
                  code3.land(0x3F),
                ),
              ),
              {..str, offset: offset + 3 },
            )
          } else if code < 0b1111_1000 && offset + 3 < length {
            let code2 = bytes[offset + 1]
            let code3 = bytes[offset + 2]
            let code4 = bytes[offset + 3]
            Some(
              Char::from_int(
                code.land(0x7).lsl(18).lor(code2.land(0x3F).lsl(12)).lor(
                  code3.land(0x3F).lsl(6),
                ).lor(code4.land(0x3F)),
              ),
              {..str, offset: offset + 4 },
            )
          } else {
            None
          } : Option[(Char, Tokens)])?
        if predicate(char) {
          Some(char, tokens)
        } else {
          None
        }
      } else {
        None
      }
    },
  }
}

pub fn pchar(char : Char) -> Parser[Char] {
  pchar_such_that(fn(c) { c == char })
}

pub fn Parser::and_then[A, B](self : Parser[A], other : Parser[B]) ->
     Parser[(A, B)] {
  {
    parse: fn(str : Tokens) -> Option[((A, B), Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) =>
          match (other.parse)(tokens) {
            Some(b, rest) => Some((a, b), rest)
            None => None
          }
        None => None
      }
    },
  }
}

pub fn Parser::or_else[A](self : Parser[A], other : Parser[A]) -> Parser[A] {
  {
    parse: fn(str : Tokens) -> Option[(A, Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) => Some(a, tokens)
        None => (other.parse)(str)
      }
    },
  }
}

pub fn Parser::or_others[A](self : Parser[A], others : List[Parser[A]]) ->
     Parser[A] {
  match others {
    Nil => self
    Cons(hd, tl) => self.or_else(hd.or_others(tl))
  }
}

pub fn Parser::map[A, B](self : Parser[A], f : (A) -> B) -> Parser[B] {
  {
    parse: fn(str : Tokens) -> Option[(B, Tokens)] {
      match (self.parse)(str) {
        Some(a, tokens) => Some(f(a), tokens)
        None => None
      }
    },
  }
}

fn fold_right[A, B](array : Array[A], f : (A, B) -> B, b : B) -> B {
  let mut i = array.length() - 1
  let mut result = b
  while i >= 0, i = i - 1 {
    result = f(array[i], result)
  }
  result
}

pub fn one_of[A : Show](array : Array[A]) -> Parser[A] {
  fold_right(
    array,
    fn(i, p) { pstring(i.to_string()).map(fn(_str) { i }).or_else(p) },
    pfail(),
  )
}

pub let pdigit : Parser[Int] = one_of([0, 1, 2, 3, 4, 5, 6, 7, 8, 9])

pub fn pfail[A]() -> Parser[A] {
  { parse: fn(_tokens) { None } }
}

pub fn pconst[A](a : A) -> Parser[A] {
  { parse: fn(tokens) { Some(a, tokens) } }
}

pub fn Parser::apply[A, B](self : Parser[A], f : Parser[(A) -> B]) -> Parser[B] {
  self.and_then(f).map(fn(pair) { (pair.1)(pair.0) })
}

pub fn lift2[A, B, C](f : (A, B) -> C) -> (Parser[A], Parser[B]) -> Parser[C] {
  fn(pa, pb) { pa.apply(pb.apply(pconst(fn(b) { fn(a) { f(a, b) } }))) }
}

pub fn sequence[A](parser_list : List[Parser[A]]) -> Parser[List[A]] {
  let consP = lift2(fn(hd, tl) { List::Cons(hd, tl) })
  match parser_list {
    Nil => pconst(Nil)
    Cons(hd, tl) => consP(hd, sequence(tl))
  }
}

fn repeat[A](a : A, n : Int) -> List[A] {
  let mut list : List[A] = Nil
  let mut i = 0
  while i < n, i = i + 1 {
    list = Cons(a, list)
  }
  list
}

fn list_to_array[A](list : List[A], length : Int) -> Array[A] {
  match list {
    Cons(hd, _) => {
      let array = Array::make(length, hd)
      let mut l = list
      let mut i = 0
      while i < length, i = i + 1 {
        match l {
          Nil => break
          Cons(hd, tl) => {
            array[i] = hd
            l = tl
          }
        }
      }
      array
    }
    Nil => Array::default()
  }
}

pub fn Parser::repeat_n[A](self : Parser[A], n : Int) -> Parser[Array[A]] {
  if n <= 0 {
    pfail()
  } else {
    sequence(repeat(self, n)).map(fn(l) { list_to_array(l, n) })
  }
}

pub fn Parser::repeat_n_with_sep[A, B](self : Parser[A], n : Int,
        sep : Parser[B]) -> Parser[Array[A]] {
  if n <= 0 {
    pfail()
  } else if n == 1 {
    self.map(fn(v) { Array::make(1, v) })
  } else {
    sequence(
      Cons(self, repeat(sep.optional().and_then(self).omit_first(), n - 1)),
    ).map(fn(l) { list_to_array(l, n) })
  }
}

pub fn Parser::repeat_0_to_n[A](self : Parser[A], n : Int) -> Parser[List[A]] {
  {
    parse: fn(str) {
      let mut tokens = str
      let mut list : List[A] = Nil
      let mut i = 0
      while i < n, i = i + 1 {
        match (self.parse)(tokens) {
          Some(v, rest) => {
            tokens = rest
            list = Cons(v, list)
          }
          None => break
        }
      }
      Some(list, tokens)
    },
  }
}

pub fn Parser::repeat_0_to_n_with_sep[A, B](self : Parser[A], n : Int,
        sep : Parser[B]) -> Parser[List[A]] {
  self.and_then(sep.optional().and_then(self).omit_first().repeat_0_to_n(n - 1)).map(
    fn(p) { List::Cons(p.0, p.1) },
  ).or_else(pconst(Nil))
}

pub fn Parser::many[A](self : Parser[A]) -> Parser[List[A]] {
  fn parse_zero_or_more(input) -> (List[A], Tokens) {
    match (self.parse)(input) {
      None => (Nil, input)
      Some(a, rest) => {
        let (ls, remain) = parse_zero_or_more(rest)
        (Cons(a, ls), remain)
      }
    }
  }

  { parse: fn(str) { Some(parse_zero_or_more(str)) } }
}

pub fn Parser::one_or_many[A](self : Parser[A]) -> Parser[(A, List[A])] {
  self.and_then(self.many())
}

pub fn Parser::optional[A](self : Parser[A]) -> Parser[Option[A]] {
  self.map(Option::Some).or_else(pconst(None))
}

fn fold_left[A, B](l : List[A], f : (B, A) -> B, b : B) -> B {
  match l {
    Nil => b
    Cons(hd, tl) => fold_left(tl, f, f(b, hd))
  }
}

pub let pint : Parser[Int] = pchar('-').optional().and_then(
  pchar('0').map(fn(_ch) { (0, List::Nil) }).or_else(pdigit.one_or_many()),
).map(
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

pub let pint64 : Parser[Int64] = pchar('-').optional().and_then(
  pchar('0').map(fn(_ch) { (0, List::Nil) }).or_else(pdigit.one_or_many()),
).map(
  fn(tuple) {
    let sign = tuple.0
    let digits = tuple.1
    let i = fold_left(digits.1, fn(a, b) { 10L * a + b.to_int64() }, digits.0.to_int64())
    match sign {
      Some(_) => -i
      None => i
    }
  },
)

pub fn Parser::omit_first[A, B](parser : Parser[(A, B)]) -> Parser[B] {
  parser.map(fn(pair) { pair.1 })
}

pub fn Parser::omit_second[A, B](parser : Parser[(A, B)]) -> Parser[A] {
  parser.map(fn(pair) { pair.0 })
}

pub fn Parser::between[A, B](self : Parser[A], around : Parser[B]) -> Parser[A] {
  omit_second(omit_first(around.and_then(self)).and_then(around))
}

pub fn Parser::separated_by1[A, B](self : Parser[A], separator : Parser[B]) ->
     Parser[(A, List[A])] {
  let sep_then = omit_first(separator.and_then(self))
  self.and_then(sep_then.many())
}

pub fn Parser::separated_by[A, B](self : Parser[A], separator : Parser[B]) ->
     Parser[List[A]] {
  self.separated_by1(separator).map(fn(pair) { List::Cons(pair.0, pair.1) }).or_else(
    pconst(Nil),
  )
}
