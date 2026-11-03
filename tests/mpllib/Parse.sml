structure Parse =
struct

  fun parseDigit char =
    let
      val code = Char.ord char
      val code0 = Char.ord #"0"
      val code9 = Char.ord #"9"
    in
      if code < code0 orelse code9 < code then
        NONE
      else
        SOME (code - code0)
    end

  fun parseInt s =
    let
      val n = Seq.length s
      fun c i = Seq.nth s i

      fun build x i =
        if i >= n then SOME x else
        case c i of
          #"," => build x (i+1)
        | #"_" => build x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => build (x * 10 + dig) (i+1)
    in
      if n = 0 then NONE
      else if (c 0 = #"-" orelse c 0 = #"~") then
        Option.map (fn x => x * ~1) (build 0 1)
      else if (c 0 = #"+") then
        build 0 1
      else
        build 0 0
    end

  fun parseReal s =
    let
      val n = Seq.length s
      fun c i = Seq.nth s i

      fun buildAfterE x i =
        Option.map (fn e => x * Math.pow (10.0, Real.fromInt e))
          (parseInt (Seq.subseq s (i, n-i)))

      fun buildAfterPoint m x i =
        if i >= n then SOME x else
        case c i of
          #"," => buildAfterPoint m x (i+1)
        | #"_" => buildAfterPoint m x (i+1)
        | #"." => NONE
        | #"e" => buildAfterE x (i+1)
        | #"E" => buildAfterE x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => buildAfterPoint (m * 0.1) (x + m * (Real.fromInt dig)) (i+1)

      fun buildBeforePoint x i =
        if i >= n then SOME x else
        case c i of
          #"," => buildBeforePoint x (i+1)
        | #"_" => buildBeforePoint x (i+1)
        | #"." => buildAfterPoint 0.1 x (i+1)
        | #"e" => buildAfterE x (i+1)
        | #"E" => buildAfterE x (i+1)
        | cc =>
            case parseDigit cc of
              NONE => NONE
            | SOME dig => buildBeforePoint (x * 10.0 + Real.fromInt dig) (i+1)
    in
      if n = 0 then NONE
      else if (c 0 = #"-" orelse c 0 = #"~") then
        Option.map (fn x => x * ~1.0) (buildBeforePoint 0.0 1)
      else
        buildBeforePoint 0.0 0
    end

  fun parseString s =
    CharVector.tabulate (Seq.length s, Seq.nth s)

  (* read a Word16, big endian, starting at index i *)
  fun r16b bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes i)
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+1)))
    in
      Word16.fromLarge w
    end

  (* read a Word32, big endian, starting at index i *)
  fun r32b bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes i)
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+1)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+2)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+3)))
    in
      Word32.fromLarge w
    end

  (* read a Word64, big endian, starting at index i *)
  fun r64b bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes i)
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+1)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+2)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+3)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+4)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+5)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+6)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+7)))
    in
      w
    end

  (* read a Word16, little endian, starting at index i *)
  fun r16l bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes (i+1))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes i))
    in
      Word16.fromLarge w
    end

  (* read a Word32, little endian, starting at index i *)
  fun r32l bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes (i+3))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+2)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+1)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes i))
    in
      Word32.fromLarge w
    end

  (* read a Word64, little endian, starting at index i *)
  fun r64l bytes i =
    let
      infix 2 << orb
      val op<< = Word64.<<
      val op orb = Word64.orb

      val w = Word8.toLarge (Seq.nth bytes (i+7))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+6)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+5)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+4)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+3)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+2)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes (i+1)))
      val w = (w << 0w8) orb (Word8.toLarge (Seq.nth bytes i))
    in
      w
    end

end
