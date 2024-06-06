/// A possibily infinate lazy list
pub type Seq[T] () -> (T, Seq[T])? derive(Debug)

/// Checks if the sequence is empty
pub fn Seq::is_empty[T](seq : Seq[T]) -> Bool {
  match (seq.0)() {
    None => true
    Some(_) => false
  }
}

pub fn Seq::default[T]() -> Seq[T] {
  Seq(fn() { None })
}

/// Unwrap a sequence
///
/// If `seq` is empty, then `seq.uncons()` is `None`  
/// Else is `Some(hd, tl)` where `hd` is the head of the sequence
/// and `tl` is the tail
pub fn Seq::uncons[T](seq : Seq[T]) -> (T, Seq[T])? {
  (seq.0)()
}

/// Map the sequence
///
/// If the sequence is x0, x1, ... then `seq.map(f)` is f(x0), f(x1), ...
pub fn Seq::map[T1, T2](seq : Seq[T1], f : (T1) -> T2) -> Seq[T2] {
  Seq(fn() { seq.uncons().map(fn { (hd, tl) => (f(hd), tl.map(f)) }) })
}

/// Construct a sequence from list
pub fn Seq::from_list[T](list : List[T]) -> Seq[T] {
  Seq(
    fn() {
      match list {
        Nil => None
        Cons(hd, tl) => Some((hd, Seq::from_list(tl)))
      }
    },
  )
}

/// Construct a sequence from string
///
/// Contents are copied, so no modification to the original string
/// will affect the sequence.  
pub fn Seq::from_string(str : String) -> Seq[Char] {
  fn aux(offset : Int) -> Seq[Char] {
    Seq(
      fn() -> (Char, Seq[Char])? {
        if offset >= str.length() {
          return None
        }
        let ch = str[offset]
        if ch.to_int() >= 0xd800 && ch.to_int() <= 0xdbff {
          let ch2 = str[offset + 1]
          Some(
            (
              Char::from_int(
                0x10000 + ch.to_int().lsl(10) + ch2.to_int() - 0xdc00,
              ),
              aux(offset + 2),
            ),
          )
        } else {
          Some((ch, aux(offset + 1)))
        }
      },
    )
  }

  aux(0)
}

pub fn length[T](self : Seq[T]) -> Int {
  loop self.uncons(), 0 {
    None, cumul => cumul
    Some((_, tl)), cumul => continue tl.uncons(), cumul + 1
  }
}

pub fn to_string[T : Show](self : Seq[T]) -> String {
  loop self.uncons(), "" {
    None, cumul => cumul
    Some((hd, tl)), cumul => continue tl.uncons(), cumul + hd.to_string()
  }
}

test "Seq from string" {
  let sequence = Seq::from_string("沪A")
  @assertion.assert_eq(sequence.length(), 2)?
  @assertion.assert_false(sequence.is_empty())?
  inspect(sequence.uncons(), content="Some((沪, A))")?
  let (_, sequence) = sequence.uncons().unwrap()
  @assertion.assert_false(sequence.is_empty())?
  inspect(sequence.uncons(), content="Some((A, ))")?
  let (_, sequence) = sequence.uncons().unwrap()
  @assertion.assert_true(sequence.is_empty())?
  let sequence = Seq::from_string("🀄️")
  @assertion.assert_eq(sequence.length(), 2)?
  inspect(sequence.uncons(), content="Some((🀄, \u{fe0f}))")?
}
