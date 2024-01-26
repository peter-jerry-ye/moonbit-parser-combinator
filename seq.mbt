/// A possibily infinate lazy list
pub type Seq[T] () -> Option[(T, Seq[T])] derive(Debug)

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
pub fn Seq::uncons[T](seq : Seq[T]) -> Option[(T, Seq[T])] {
  (seq.0)()
}

/// Map the sequence
///
/// If the sequence is x0, x1, ... then `seq.map(f)` is f(x0), f(x1), ...
pub fn Seq::map[T1, T2](seq : Seq[T1], f : (T1) -> T2) -> Seq[T2] {
  Seq(fn() {
    let (hd, tl) = seq.uncons()?
    Some(f(hd), tl.map(f))
  })
}

/// Construct a sequence from list
pub fn Seq::from_list[T](list : List[T]) -> Seq[T] {
  Seq(fn() {
    match list {
      Nil => None
      Cons(hd, tl) => Some(hd, Seq::from_list(tl))
    }
  })
}

/// Construct a sequence from string
///
/// Contents are copied, so no modification to the original string
/// will affect the sequence.  
pub fn Seq::from_string(str : String) -> Seq[Char] {
  let bytes = str.to_bytes()
  fn aux(offset : Int) -> Seq[Char] {
    Seq(fn() -> Option[(Char, Seq[Char])] {
      let length = bytes.length()
      if offset < length {
        let code = bytes[offset]
        let (char, new_offset) = (
          if code < 0b1100_0000 {
            Some(Char::from_int(code), offset + 1)
          } else if code < 0b1110_0000 && offset + 1 < length {
            let code2 = bytes[offset + 1]
            Some(
              Char::from_int(code.land(0x1F).lsl(6).lor(code2.land(0x3F))),
              offset + 2,
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
              offset + 3,
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
              offset + 4,
            )
          } else {
            None
          } : Option[(Char, Int)])?
        Some(char, aux(new_offset))
      } else {
        None
      }
    })
  }

  aux(0)
}

test "Seq from string" {
  let sequence = Seq::from_string("沪A")
  if sequence.is_empty() {
    abort("sequence is not empty")
  }
  let (hd, sequence) = sequence.uncons().unwrap()
  if hd != '沪' {
    abort("sequence should start with '沪'")
  }
  let (hd, sequence) = sequence.uncons().unwrap()
  if hd != 'A' {
    abort("sequence should start with 'A")
  }
  if sequence.is_empty().not() {
    abort("sequence should be empty")
  }
}
