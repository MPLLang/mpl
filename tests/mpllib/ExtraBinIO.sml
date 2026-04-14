structure ExtraBinIO =
struct

  fun w8 file (w: Word8.word) = BinIO.output1 (file, w)

  fun w64b file (w: Word64.word) =
    let
      val w8 = w8 file
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w56));
      w8 (Word8.fromLarge (w >> 0w48));
      w8 (Word8.fromLarge (w >> 0w40));
      w8 (Word8.fromLarge (w >> 0w32));
      w8 (Word8.fromLarge (w >> 0w24));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w32b file (w: Word32.word) =
    let
      val w8 = w8 file
      val w = Word32.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w24));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w32l file (w: Word32.word) =
    let
      val w8 = w8 file
      val w = Word32.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge w);
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge (w >> 0w16));
      w8 (Word8.fromLarge (w >> 0w24))
    end

  fun w16b file (w: Word16.word) =
    let
      val w8 = w8 file
      val w = Word16.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge (w >> 0w8));
      w8 (Word8.fromLarge w)
    end

  fun w16l file (w: Word16.word) =
    let
      val w8 = w8 file
      val w = Word16.toLarge w
      open Word64
      infix 2 >> andb
    in
      w8 (Word8.fromLarge w);
      w8 (Word8.fromLarge (w >> 0w8))
    end

  fun wrgb file ({red, green, blue}: Color.pixel) =
    ( w8 file red; w8 file green; w8 file blue )

end
