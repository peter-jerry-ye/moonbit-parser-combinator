/// A parser combinator
pub type Parser[Token, Value] (Seq[Token]) -> Option[(Value, Seq[Token])]

/// Parses a sequence of tokens
///
/// If the parsing succeeds, `Some(result, rest)` is returned  
/// Otherwise, `None` is returned.
/// TODO: Move to `Result[A, B]`
pub fn Parser::parse[Token, Value](self : Parser[Token, Value],
        sequence : Seq[Token]) -> Option[(Value, Seq[Token])] {
  (self.0)(sequence)
}

pub fn pvalue[Token, Value](predicate : (Token) -> Option[Value]) ->
     Parser[Token, Value] {
  Parser(fn(seq) {
    let (hd, rest) = seq.uncons()?
    let value = predicate(hd)?
    Some(value, rest)
  })
}

pub fn pfail[Token, A]() -> Parser[Token, A] {
  Parser(fn(_tokens) { None })
}

pub fn pconst[Token, A](a : A) -> Parser[Token, A] {
  Parser(fn(tokens) { Some(a, tokens) })
}

pub fn lift2[Token, A, B, C](f : (A, B) -> C) ->
     (Parser[Token, A], Parser[Token, B]) -> Parser[Token, C] {
  fn(pa, pb) { pa.apply(pb.apply(pconst(fn(b) { fn(a) { f(a, b) } }))) }
}

pub fn sequence[Token, A](parser_list : List[Parser[Token, A]]) ->
     Parser[Token, List[A]] {
  let consP = lift2(fn(hd, tl) { List::Cons(hd, tl) })
  match parser_list {
    Nil => pconst(Nil)
    Cons(hd, tl) => consP(hd, sequence(tl))
  }
}

pub fn Parser::and_then[Token, A, B](self : Parser[Token, A],
        other : Parser[Token, B]) -> Parser[Token, (A, B)] {
  Parser(fn(tokens) {
    let (a, rest) = self.parse(tokens)?
    let (b, rest2) = other.parse(rest)?
    Some((a, b), rest2)
  })
}

pub fn Parser::or_else[Token, A](self : Parser[Token, A],
        other : Parser[Token, A]) -> Parser[Token, A] {
  Parser(fn(tokens) {
    match self.parse(tokens) {
      None => other.parse(tokens)
      Some(_) as result => result
    }
  })
}

pub fn Parser::or_others[Token, A](self : Parser[Token, A],
        others : List[Parser[Token, A]]) -> Parser[Token, A] {
  match others {
    Nil => self
    Cons(hd, tl) => self.or_else(hd.or_others(tl))
  }
}

pub fn Parser::map[Token, A, B](self : Parser[Token, A], f : (A) -> B) ->
     Parser[Token, B] {
  Parser(fn(tokens) {
    let (a, rest) = self.parse(tokens)?
    Some(f(a), rest)
  })
}

pub fn Parser::apply[Token, A, B](self : Parser[Token, A],
        f : Parser[Token, (A) -> B]) -> Parser[Token, B] {
  self.and_then(f).map(fn(pair) { (pair.1)(pair.0) })
}

pub fn Parser::repeat_n[Token, A](self : Parser[Token, A], n : Int) ->
     Parser[Token, Array[A]] {
  if n <= 0 {
    pfail()
  } else {
    sequence(repeat(self, n)).map(fn(l) { list_to_array(l, n) })
  }
}

pub fn Parser::repeat_n_with_sep[Token, A, B](self : Parser[Token, A], n : Int,
        sep : Parser[Token, B]) -> Parser[Token, Array[A]] {
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

pub fn Parser::repeat_0_to_n[Token, A](self : Parser[Token, A], n : Int) ->
     Parser[Token, List[A]] {
  Parser(fn(str) {
    let mut tokens = str
    let mut list : List[A] = Nil
    let mut i = 0
    while i < n, i = i + 1 {
      match self.parse(tokens) {
        Some(v, rest) => {
          tokens = rest
          list = Cons(v, list)
        }
        None => break
      }
    }
    Some(list, tokens)
  })
}

pub fn Parser::repeat_0_to_n_with_sep[Token, A, B](self : Parser[Token, A],
        n : Int, sep : Parser[Token, B]) -> Parser[Token, List[A]] {
  self.and_then(sep.optional().and_then(self).omit_first().repeat_0_to_n(n - 1)).map(
    fn(p) { List::Cons(p.0, p.1) },
  ).or_else(pconst(Nil))
}

pub fn Parser::many[Token, A](self : Parser[Token, A]) -> Parser[Token, List[A]] {
  fn parse_zero_or_more(input) -> (List[A], Seq[Token]) {
    match self.parse(input) {
      None => (Nil, input)
      Some(a, rest) => {
        let (ls, remain) = parse_zero_or_more(rest)
        (Cons(a, ls), remain)
      }
    }
  }

  Parser(fn(str) { Some(parse_zero_or_more(str)) })
}

pub fn Parser::one_or_many[Token, A](self : Parser[Token, A]) ->
     Parser[Token, (A, List[A])] {
  self.and_then(self.many())
}

pub fn Parser::optional[Token, A](self : Parser[Token, A]) ->
     Parser[Token, Option[A]] {
  self.map(Option::Some).or_else(pconst(None))
}

pub fn Parser::omit_first[Token, A, B](parser : Parser[Token, (A, B)]) ->
     Parser[Token, B] {
  parser.map(fn(pair) { pair.1 })
}

pub fn Parser::omit_second[Token, A, B](parser : Parser[Token, (A, B)]) ->
     Parser[Token, A] {
  parser.map(fn(pair) { pair.0 })
}

pub fn Parser::between[Token, A, B](self : Parser[Token, A],
        around : Parser[Token, B]) -> Parser[Token, A] {
  omit_second(omit_first(around.and_then(self)).and_then(around))
}

pub fn Parser::separated_by1[Token, A, B](self : Parser[Token, A],
        separator : Parser[Token, B]) -> Parser[Token, (A, List[A])] {
  let sep_then = omit_first(separator.and_then(self))
  self.and_then(sep_then.many())
}

pub fn Parser::separated_by[Token, A, B](self : Parser[Token, A],
        separator : Parser[Token, B]) -> Parser[Token, List[A]] {
  self.separated_by1(separator).map(fn(pair) { List::Cons(pair.0, pair.1) }).or_else(
    pconst(Nil),
  )
}

pub fn Parser::ref[Token, A](self : Ref[Parser[Token, A]]) -> Parser[Token, A] {
  Parser(fn(str) { self.val.parse(str) })
}

// helper functions. TODO: replace with std

fn repeat[A](a : A, n : Int) -> List[A] {
  let mut list : List[A] = Nil
  let mut i = 0
  while i < n, i = i + 1 {
    list = Cons(a, list)
  }
  list
}

fn when[T](predicate : Bool, value : T) -> Option[T] {
  if predicate {
    Some(value)
  } else {
    None
  }
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

fn fold_left[A, B](l : List[A], f : (B, A) -> B, b : B) -> B {
  match l {
    Nil => b
    Cons(hd, tl) => fold_left(tl, f, f(b, hd))
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

// String specific parsers

pub fn pchar_such_that(predicate : (Char) -> Bool) -> Parser[Char, Char] {
  pvalue(fn{ char => when(predicate(char), char) })
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

/// parser for integer
///
/// [ "-" ] ( %x30 / (%x31-39) *(%x30-39)
pub let pint : Parser[Char, Int] = pchar('-').optional().and_then(
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

/// parser for integer 64 bits
///
/// [ "-" ] ( %x30 / (%x31-39) *(%x30-39)
pub let pint64 : Parser[Char, Int64] = pchar('-').optional().and_then(
  pchar('0').map(fn(_ch) { (0, List::Nil) }).or_else(pdigit.one_or_many()),
).map(
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

/// parser that tries to match the given string
pub fn pstring(string : String) -> Parser[Char, String] {
  Parser(fn(seq) {
    let mut input = Seq::from_string(string)
    let mut iter = seq
    while input.is_empty().not() {
      let (target, rest_target) = input.uncons()?
      let (char, rest) = seq.uncons()?
      if target != char {
        return None
      }
      input = rest_target
      iter = rest
    }
    Some(string, iter)
  })
}
