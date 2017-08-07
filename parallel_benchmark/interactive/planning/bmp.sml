structure Bmp =
struct

structure V = Vector

type pixel = Word8.word * Word8.word * Word8.word

type bmp =
     {width: int,
      height: int,
      pixels: pixel V.vector V.vector}

exception InvalidBMP

fun open_bmp (s: string) =
    let fun ltoint v off =
            let val b4 = V.sub (v, off)
                val b3 = V.sub (v, off + 1)
                val b2 = V.sub (v, off + 2)
                val b1 = V.sub (v, off + 3)
                val i1 = Word8.toInt b1
                val i2 = Word8.toInt b2
                val i3 = Word8.toInt b3
                val i4 = Word8.toInt b4
            in
                i1 * 256 * 256 * 256 +
                i2 * 256 * 256 +
                i3 * 256 +
                i4
            end
        fun dwtoint v off =
            let val w1 = ltoint v off
                val w2 = ltoint v (off + 4)
            in
                w1 * 256 * 256 * 256 * 256 +
                w2
            end
        val is = BinIO.openIn s
        val words = BinIO.inputAll is
        val _ = BinIO.closeIn is
        val off = ltoint words (2 + 4 + 2 + 2)
        val width = ltoint words (2 + 4 + 2 + 2 + 4 + 4)
        val height = ltoint words (2 + 4 + 2 + 2 + 4 + 4 + 4)
        fun row v off =
            V.tabulate (width,
                        fn i => (V.sub (v, off + i * 4 + 1),
                                 V.sub (v, off + i * 4 + 2),
                                 V.sub (v, off + i * 4 + 3)))
(*        val _ = print ("width: " ^ (Int.toString width) ^ "\n")
        val _ = print ("height: " ^ (Int.toString height) ^ "\n") *)
    in
        {height = height,
         width = width,
         pixels = V.tabulate (height,
                              fn r => row words (off + r * width * 4))}
        handle Subscript => raise InvalidBMP
    end
end
