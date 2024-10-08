/// A possibily infinate lazy list
pub type Seq[T] () -> (T, Seq[T])?

/// Checks if the sequence is empty
pub fn Seq::is_empty[T](seq : Seq[T]) -> Bool {
  match (seq._)() {
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
  (seq._)()
}

/// Map the sequence
///
/// If the sequence is x0, x1, ... then `seq.map(f)` is f(x0), f(x1), ...
pub fn Seq::map[T1, T2](seq : Seq[T1], f : (T1) -> T2) -> Seq[T2] {
  Seq(fn() { seq.uncons().map(fn { (hd, tl) => (f(hd), tl.map(f)) }) })
}

/// Construct a sequence from list
pub fn Seq::from_list[T](list : @immut/list.T[T]) -> Seq[T] {
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
                0x10000 + (ch.to_int() - 0xd800).lsl(10) + ch2.to_int() - 0xdc00,
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

/// Construct a sequence from array
pub fn Seq::from_array[T](array : ArrayView[T]) -> Seq[T] {
  Seq(
    fn() {
      match array {
        [] => None
        [hd, .. as rest] => Some((hd, Seq::from_array(rest)))
      }
    },
  )
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

pub fn Seq::output[T : Show](self : Seq[T], log : Logger) -> Unit {
  loop self.uncons() {
    None => ()
    Some((hd, tl)) => {
      (hd as Show).output(log)
      continue tl.uncons()
    }
  }
}

test "Seq from string" {
  let sequence = Seq::from_string("沪A")
  assert_eq!(sequence.length(), 2)
  assert_false!(sequence.is_empty())
  inspect!(sequence.uncons(), content="Some(('沪', 'A'))")
  let (_, sequence) = sequence.uncons().or_error!(Failure("Shouldn't happen"))
  assert_false!(sequence.is_empty())
  inspect!(sequence.uncons(), content="Some(('A', ))")
  let (_, sequence) = sequence.uncons().or_error!(Failure("Shouldn't happen"))
  assert_true!(sequence.is_empty())
  let sequence = Seq::from_string("🀄️")
  assert_eq!(sequence.length(), 2)
  inspect!(sequence.uncons(), content="Some(('🀄', '\u{fe0f}'))")
}

test "Seq from array" {
  let sequence = Seq::from_array("沪A".iter().collect()[:])
  assert_eq!(sequence.length(), 2)
  assert_false!(sequence.is_empty())
  inspect!(sequence.uncons(), content="Some(('沪', 'A'))")
  let (_, sequence) = sequence.uncons().or_error!(Failure("Shouldn't happen"))
  assert_false!(sequence.is_empty())
  inspect!(sequence.uncons(), content="Some(('A', ))")
  let (_, sequence) = sequence.uncons().or_error!(Failure("Shouldn't happen"))
  assert_true!(sequence.is_empty())
  let sequence = Seq::from_array("🀄️".iter().collect()[:])
  assert_eq!(sequence.length(), 2)
  inspect!(sequence.uncons(), content="Some(('🀄', '\u{fe0f}'))")
}
