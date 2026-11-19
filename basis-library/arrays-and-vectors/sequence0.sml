(* Copyright (C) 2015,2017 Matthew Fluet.
 * Copyright (C) 2014 Rob Simmons.
 * Copyright (C) 2013 Matthew Fluet.
 * Copyright (C) 1999-2008 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 * Copyright (C) 1997-2000 NEC Research Institute.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)


signature PRIM_SEQUENCE_PARAMS =
sig
  type 'a sequence
  type 'a elt
  type 'a prim_array
  type 'a prim_vector
  val copyUnsafe: 'a elt prim_array * SeqIndex.int * 'a sequence * SeqIndex.int * SeqIndex.int -> unit
  (* fromArray should be constant time. *)
  val fromArray: 'a elt prim_array -> 'a sequence
  val isMutable: bool
  val length: 'a sequence -> SeqIndex.int
  val sameArray: 'a elt prim_array * 'a sequence -> bool
  val subUnsafe: 'a sequence * SeqIndex.int -> 'a elt
  val uninitIsNop: 'a sequence -> bool
  val uninitUnsafe: 'a sequence * SeqIndex.int -> unit
  val updateUnsafe: 'a sequence * SeqIndex.int * 'a elt -> unit

  val primArrayAllocUnsafe: SeqIndex.int -> 'a prim_array
  val primArraySubUnsafe: 'a prim_array * SeqIndex.int -> 'a
  val primArrayUpdateUnsafe: 'a prim_array * SeqIndex.int * 'a -> unit
  val primArrayLength: 'a prim_array -> SeqIndex.int
  val primVectorFromArrayUnsafe: 'a prim_array -> 'a prim_vector
end


functor PrimSequence (S: PRIM_SEQUENCE_PARAMS)
  :> PRIM_SEQUENCE where type 'a sequence = 'a S.sequence
                   where type 'a elt = 'a S.elt
                   where type 'a prim_array = 'a S.prim_array
                   where type 'a prim_vector = 'a S.prim_vector =
   struct
      val op +! = SeqIndex.+!
      val op + = SeqIndex.+
      val op -! = SeqIndex.-!
      val op < = SeqIndex.<
      val op <= = SeqIndex.<=
      val op > = SeqIndex.>
      val op >= = SeqIndex.>=
      val gtu = SeqIndex.gtu
      val geu = SeqIndex.geu
      val ! = Primitive.Ref.deref
      val op := = Primitive.Ref.assign
      fun (f o g) x = f (g x) 

      type 'a sequence = 'a S.sequence
      type 'a elt = 'a S.elt
      type 'a prim_array = 'a S.prim_array
      type 'a prim_vector = 'a S.prim_vector

      local
         fun valOf x: Primitive.Int32.int = case x of SOME y => y | NONE => 0
         fun doit (precision, fromInt, maxInt') = 
            if Primitive.Int32.>= (valOf SeqIndex.precision, precision)
               then fromInt maxInt'
            else SeqIndex.maxInt'
         structure S = 
            Int_ChooseInt 
            (type 'a t = SeqIndex.int
             val fInt8 = doit (valOf Primitive.Int8.precision,
                               SeqIndex.schckFromInt8,
                               Primitive.Int8.maxInt')
             val fInt16 = doit (valOf Primitive.Int16.precision,
                                SeqIndex.schckFromInt16,
                                Primitive.Int16.maxInt')
             val fInt32 = doit (valOf Primitive.Int32.precision,
                                SeqIndex.schckFromInt32,
                                Primitive.Int32.maxInt')
             val fInt64 = doit (valOf Primitive.Int64.precision,
                                SeqIndex.schckFromInt64,
                                Primitive.Int64.maxInt')
             val fIntInf = SeqIndex.maxInt')
      in 
         val maxLen = S.f
      end

      fun length s = S.length s

      fun unsafeArrayAlloc n = S.primArrayAllocUnsafe n
      fun arrayAlloc n =
         if Primitive.Controls.safe
            andalso gtu (n, maxLen)
            then raise Size
            else unsafeArrayAlloc n
      fun unsafeAlloc n = S.fromArray (unsafeArrayAlloc n)
      fun alloc n = S.fromArray (arrayAlloc n)
      val unsafeFromArray = S.fromArray
      exception CreateAlreadyGotVector
      exception CreateVectorNotFull
      fun create n =
        let
           val a = arrayAlloc n
           val subLim : SeqIndex.t ref = ref 0
           fun sub i =
              if Primitive.Controls.safe andalso geu (i, !subLim) then
                 raise Subscript
              else
                 S.primArraySubUnsafe (a, i)
           val updateLim : SeqIndex.t ref = ref 0
           fun update (i, x) =
              if Primitive.Controls.safe andalso geu (i, !updateLim) then
                 if i = !updateLim andalso i < n then
                    (S.primArrayUpdateUnsafe (a, i, x);
                     subLim := i + 1;
                     updateLim := i + 1)
                 else
                    raise Subscript
              else
                 S.primArrayUpdateUnsafe (a, i, x)
           val gotIt = ref false
           fun done () =
              if !gotIt then
                 raise CreateAlreadyGotVector
              else
                 if n = !updateLim then
                    (gotIt := true;
                     updateLim := 0;
                     S.fromArray a)
                 else
                    raise CreateVectorNotFull
        in
           {done = done,
            sub = sub,
            update = update}
        end

      fun unfoldi (n, b, f) =
         let
            val a = arrayAlloc n
            fun loop (i, b)  =
               if i >= n
                  then b
               else
                  let
                     val (x, b) = f (i, b)
                     val () = S.primArrayUpdateUnsafe (a, i, x)
                  in
                     loop (i +! 1, b)
                  end
            val b = loop (0, b)
         in
            (S.fromArray a, b)
         end

      fun unfold (n, b, f) = unfoldi (n, b, f o #2)

      fun tabulate (n, f) =
         #1 (unfoldi (n, (), fn (i, ()) => (f i, ())))
            
      fun new (n, x) = tabulate (n, fn _ => x)
      
      structure Slice =
         struct
            type 'a sequence = 'a sequence
            type 'a elt = 'a elt
            type 'a prim_array = 'a prim_array
            datatype 'a t = T of {seq: 'a sequence, 
                                  start: SeqIndex.int, len: SeqIndex.int}
            type 'a slice = 'a t
            
            fun length (T {len, ...}) = len
            fun unsafeSub (T {seq, start, ...}, i) =
               S.subUnsafe (seq, start +! i)
            fun sub (sl as T {len, ...}, i) =
               if Primitive.Controls.safe andalso geu (i, len)
                  then raise Subscript
                  else unsafeSub (sl, i)
            fun unsafeUpdate (T {seq, start, ...}, i, x) =
               S.updateUnsafe (seq, start +! i, x)
            fun update (sl as T {len, ...}, i, x) =
               if Primitive.Controls.safe andalso geu (i, len)
                  then raise Subscript
               else unsafeUpdate (sl, i, x)
            fun uninitIsNop (T {seq, ...}) =
               S.uninitIsNop seq
            fun unsafeUninit (T {seq, start, ...}, i) =
               S.uninitUnsafe (seq, start +! i)
            fun uninit (sl as T {len, ...}, i) =
               if Primitive.Controls.safe andalso geu (i, len)
                  then raise Subscript
               else unsafeUninit (sl, i)
            local
               fun smallCopy {dst: 'a elt prim_array, di: SeqIndex.int,
                              src: 'a sequence, si: SeqIndex.int,
                              len: SeqIndex.int,
                              overlap: unit -> bool} =
                  let
                     fun move i = S.primArrayUpdateUnsafe (dst, di +! i, S.subUnsafe (src, si +! i))
                     val len = len -! 1
                  in
                     if overlap ()
                        then let
                                fun loop i =
                                   if i < 0
                                      then ()
                                      else (move i; loop (i -! 1))
                             in
                                loop len
                             end
                        else let
                                fun loop i =
                                   if i > len
                                      then ()
                                      else (move i; loop (i +! 1))
                             in
                                loop 0
                             end
                  end
               val smallCopyLimit = 5
               fun maybeSmallCopy {dst: 'a elt prim_array, di: SeqIndex.int,
                                   src: 'a sequence, si: SeqIndex.int,
                                   len: SeqIndex.int,
                                   overlap: unit -> bool} =
                  if len < smallCopyLimit
                     then smallCopy {dst = dst, di = di,
                                     src = src, si = si,
                                     len = len,
                                     overlap = overlap}
                     else S.copyUnsafe (dst, di, src, si, len)
            in
               fun unsafeCopy {dst: 'a elt prim_array, di: SeqIndex.int,
                               src = T {seq = src, start = si, len}} =
                  maybeSmallCopy {dst = dst, di = di,
                                  src = src, si = si,
                                  len = len,
                                  overlap = fn () => false}
               fun copy {dst: 'a elt prim_array, di: SeqIndex.int,
                         src =  T {seq = src, start = si, len}} =
                  if Primitive.Controls.safe
                     andalso (gtu (di, S.primArrayLength dst)
                              orelse gtu (di +! len, S.primArrayLength dst))
                     then raise Subscript
                     else let
                             fun overlap () =
                                S.sameArray (dst, src)
                                andalso si < di
                                andalso di <= si +! len
                          in
                             maybeSmallCopy {dst = dst, di = di,
                                             src = src, si = si,
                                             len = len,
                                             overlap = overlap}
                          end
            end
            fun full (seq: 'a sequence) : 'a slice =
               T {seq = seq, start = 0, len = S.length seq}
            fun unsafeSubslice (T {seq, start, len}, start', len') = 
               T {seq = seq, 
                  start = start +! start',
                  len = (case len' of
                            NONE => len -! start'
                          | SOME len' => len')}
            fun unsafeSlice (seq, start, len) =
               unsafeSubslice (full seq, start, len)
            fun subslice (T {seq, start, len}, start', len') =
               case len' of
                  NONE => 
                     if Primitive.Controls.safe 
                        andalso gtu (start', len)
                        then raise Subscript
                        else T {seq = seq,
                                start = start +! start',
                                len = len -! start'}
                | SOME len' => 
                     if Primitive.Controls.safe
                        andalso (gtu (start', len)
                                 orelse gtu (len', len -! start'))
                        then raise Subscript
                        else T {seq = seq,
                                start = start +! start',
                                len = len'}
            fun slice (seq: 'a sequence, start, len) =
               subslice (full seq, start, len)
            fun base (T {seq, start, len}) = 
               (seq, start, len)
            fun isEmpty sl = length sl = 0
            fun getItem (sl as T {seq, start, len}) =
               if isEmpty sl
                  then NONE
               else SOME (S.subUnsafe (seq, start), 
                          T {seq = seq, 
                             start = start +! 1, 
                             len = len -! 1})
            fun foldli f b (T {seq, start, len}) =
               let
                  val min = start
                  val len = len -! 1
                  val max = start +! len
                  fun loop (i, b) =
                     if i > max then b
                     else loop (i +! 1, f (i -! min, S.subUnsafe (seq, i), b))
               in loop (min, b)
               end
            fun foldri f b (T {seq, start, len}) =
               let
                  val min = start
                  val len = len -! 1
                  val max = start +! len
                  fun loop (i, b) =
                     if i < min then b
                     else loop (i -! 1, f (i -! min, S.subUnsafe (seq, i), b))
               in loop (max, b)
               end
            local
               fun make foldi f b sl = foldi (fn (_, x, b) => f (x, b)) b sl
            in
               fun foldl f = make foldli f
               fun foldr f = make foldri f
            end
            fun appi f sl = foldli (fn (i, x, ()) => f (i, x)) () sl
            fun app f sl = appi (f o #2) sl 
            fun mapi f (T {seq, start, len}) = 
               tabulate (len, fn i => f (i, S.subUnsafe (seq, start +! i)))
            fun map f sl = mapi (f o #2) sl
            fun findi p (T {seq, start, len}) = 
               let
                  val min = start
                  val len = len -! 1
                  val max = start +! len
                  fun loop i =
                     if i > max
                        then NONE
                     else let val z = (i -! min, S.subUnsafe (seq, i))
                          in if p z
                                then SOME z
                             else loop (i +! 1)
                          end
               in loop min
               end
            fun find p sl = 
               case findi (p o #2) sl of
                  NONE => NONE
                | SOME (_, x) => SOME x 
            fun existsi p sl = 
               case findi p sl of
                  NONE => false
                | SOME _ => true
            fun exists p sl = existsi (p o #2) sl
            fun alli p sl = not (existsi (not o p) sl)
            fun all p sl = alli (p o #2) sl
            fun collate cmp (T {seq = seq1, start = start1, len = len1},
                             T {seq = seq2, start = start2, len = len2}) =
               let
                  val min1 = start1
                  val min2 = start2
                  val max1 = start1 +! len1
                  val max2 = start2 +! len2
                  fun loop (i, j) =
                     case (i >= max1, j >= max2) of
                        (true, true) => EQUAL
                      | (true, false) => LESS
                      | (false, true) => GREATER
                      | (false, false) => 
                           (case cmp (S.subUnsafe (seq1, i), 
                                      S.subUnsafe (seq2, j)) of
                              EQUAL => loop (i +! 1, j +! 1)
                            | ans => ans)
               in loop (min1, min2)
               end
            fun sequence (sl as T {seq, start, len}): 'a sequence =
               if S.isMutable orelse (start <> 0 orelse len <> S.length seq)
                  then let
                          val a = arrayAlloc len
                       in
                          S.copyUnsafe (a, 0, seq, start, len)
                          ; S.fromArray a
                       end
               else seq
            fun append (sl1: 'a slice, sl2: 'a slice): 'a sequence =
               if length sl1 = 0
                  then sequence sl2
               else if length sl2 = 0
                  then sequence sl1
               else
                  let
                     val (seq1, start1, len1) = base sl1
                     val (seq2, start2, len2) = base sl2
                     val n = len1 +! len2
                     val a = arrayAlloc n
                  in
                     S.copyUnsafe (a, 0, seq1, start1, len1)
                     ; S.copyUnsafe (a, len1, seq2, start2, len2)
                     ; S.fromArray a
                  end
            fun split (T {seq, start, len}, i) =
               (unsafeSlice (seq, start, SOME (i -! start)),
                unsafeSlice (seq, i, SOME (len -! (i -! start))))
            fun splitl f (sl as T {seq, start, len}) =
               let
                  val stop = start +! len
                  fun loop i =
                     if i >= stop
                        then i
                     else if f (S.subUnsafe (seq, i))
                             then loop (i +! 1)
                          else i
               in split (sl, loop start)
               end
            fun splitr f (sl as T {seq, start, len}) =
               let
                  fun loop i =
                     if i < start
                        then start
                     else if f (S.subUnsafe (seq, i))
                             then loop (i -! 1)
                          else i +! 1
               in split (sl, loop (start +! len -! 1))
               end
            fun splitAt (T {seq, start, len}, i) =
               if Primitive.Controls.safe andalso gtu (i, len)
                  then raise Subscript
               else (unsafeSlice (seq, start, SOME i),
                     unsafeSlice (seq, start +! i, SOME (len -! i)))
            fun dropl p s = #2 (splitl p s)
            fun dropr p s = #1 (splitr p s)
            fun takel p s = #1 (splitl p s)
            fun taker p s = #2 (splitr p s)
            fun position (eq: 'a elt * 'a elt -> bool)
                         (seq': 'a sequence)
                         (sl as T {seq, start, len}) =
               let
                  val len' = S.length seq'
                  val max = start +! len -! len' +! 1
                  (* loop returns the index of the front of the suffix. *)
                  fun loop i =
                     if i >= max
                        then start +! len
                     else let
                             fun loop' j =
                                if j >= len'
                                   then i
                                else if eq (S.subUnsafe (seq, i +! j), 
                                            S.subUnsafe (seq', j))
                                        then loop' (j +! 1)
                                     else loop (i +! 1)
                          in loop' 0
                          end
               in split (sl, loop start)
               end
            fun span (eq: 'a sequence * 'a sequence -> bool)
                     (T {seq, start, ...},
                      T {seq = seq', start = start', len = len'}) =
               if Primitive.Controls.safe andalso 
                  (not (eq (seq, seq')) orelse start' +! len' < start)
                  then raise Span
               else unsafeSlice (seq, start, SOME ((start' +! len') -! start))
         end

      local
         fun make f seq = f (Slice.full seq)
         fun make2 f (seq1, seq2) = f (Slice.full seq1, Slice.full seq2)
      in
         fun sub (seq, i) = Slice.sub (Slice.full seq, i)
         fun unsafeSub (seq, i) = Slice.unsafeSub (Slice.full seq, i)
         fun update (seq, i, x) = Slice.update (Slice.full seq, i, x)
         fun unsafeUpdate (seq, i, x) = Slice.unsafeUpdate (Slice.full seq, i, x)
         fun uninitIsNop seq = Slice.uninitIsNop (Slice.full seq)
         fun uninit (seq, i) = Slice.uninit (Slice.full seq, i)
         fun unsafeUninit (seq, i) = Slice.unsafeUninit (Slice.full seq, i)
         fun copy {dst, di, src} = Slice.copy {dst = dst, di = di, src = Slice.full src}
         fun unsafeCopy {dst, di, src} = Slice.unsafeCopy {dst = dst, di = di, src = Slice.full src}
         fun appi f = make (Slice.appi f)
         fun app f = make (Slice.app f)
         fun mapi f = make (Slice.mapi f)
         fun map f = make (Slice.map f)
         fun foldli f b = make (Slice.foldli f b)
         fun foldl f b = make (Slice.foldl f b)
         fun foldri f b = make (Slice.foldri f b)
         fun foldr f b = make (Slice.foldr f b)
         fun findi p = make (Slice.findi p)
         fun find p = make (Slice.find p)
         fun existsi p = make (Slice.existsi p)
         fun exists p = make (Slice.exists p)
         fun alli p = make (Slice.alli p)
         fun all p = make (Slice.all p)
         fun collate cmp = make2 (Slice.collate cmp)
         fun append seqs = make2 Slice.append seqs
         fun duplicate seq = make Slice.sequence seq
      end

   end


signature PRIM_RAW_ARRAY =
sig
  type 'a array
  type 'a rawarr
  val allocUnsafe: SeqIndex.int -> 'a rawarr
  val length: 'a rawarr -> SeqIndex.int
  val toArrayUnsafe: 'a rawarr -> 'a array
  val uninitIsNop: 'a rawarr -> bool
  val uninitUnsafe: 'a rawarr * SeqIndex.int -> unit
end


functor WrapRawArray(S: sig
                          include PRIM_RAW_ARRAY
                          val maxLen: SeqIndex.int
                        end) =
struct
  type 'a rawarr = 'a S.rawarr

  val length = S.length

  val unsafeAlloc = S.allocUnsafe
  fun alloc n =
      if Primitive.Controls.safe
        andalso SeqIndex.gtu (n, S.maxLen)
        then raise Size
        else unsafeAlloc n

  val unsafeToArray = S.toArrayUnsafe

  val uninitIsNop = S.uninitIsNop
  val unsafeUninit = S.uninitUnsafe
  fun uninit (a, i) =
      if Primitive.Controls.safe andalso SeqIndex.geu (i, length a)
        then raise Subscript
        else unsafeUninit (a, i)
end


functor MakeArrayPrimSequence (S: sig
                                    include PRIM_SEQUENCE_PARAMS
                                    structure Raw: PRIM_RAW_ARRAY
                                      where type 'a array = 'a prim_array
                                  end) =
struct

  local
    structure P = PrimSequence(S)
  in
    open P
    type 'a array = 'a prim_array
    type 'a t = 'a prim_array
    type 'a vector = 'a prim_vector
    structure Slice = 
      struct
          open Slice
          fun vector sl = 
            let
                val a = unsafeArrayAlloc (length sl)
                val () = unsafeCopy {dst = a, di = 0, src = sl}
            in
                S.primVectorFromArrayUnsafe a
            end
          fun modifyi f sl =
            appi (fn (i, x) => unsafeUpdate (sl, i, f (i, x))) sl
          fun modify f sl = modifyi (fn (_, x) => f x) sl
      end
    fun vector s = Slice.vector (Slice.full s)
    fun modifyi f s = Slice.modifyi f (Slice.full s)
    fun modify f s = Slice.modify f (Slice.full s) 
  end

  structure Raw = WrapRawArray(open S.Raw val maxLen = maxLen)

end



functor MakeVectorPrimSequence (S: PRIM_SEQUENCE_PARAMS) =
struct
  local
    structure P = PrimSequence(S)
  in
    open P
    type 'a vector = 'a prim_vector
    type 'a t = 'a prim_vector
    type 'a array = 'a prim_array
    fun updateVector (v, i, x) =
      if Primitive.Controls.safe andalso SeqIndex.geu (i, length v)
          then raise Subscript
      else let
              val a = S.primArrayAllocUnsafe (length v)
              val () = copy {dst = a, di = 0, src = v}
              val () = S.primArrayUpdateUnsafe (a, i, x)
            in
              S.primVectorFromArrayUnsafe a
            end
  end
end


structure Primitive = struct
open Primitive

structure Array = MakeArrayPrimSequence(
  type 'a sequence = 'a array
  type 'a elt = 'a
  type 'a prim_array = 'a array
  type 'a prim_vector = 'a vector
  val sameArray = op =
  val copyUnsafe = Primitive.Array.copyArrayUnsafe
  val fromArray = fn a => a
  val isMutable = true
  val length = Primitive.Array.length
  val subUnsafe = Primitive.Array.subUnsafe
  val uninitIsNop = Primitive.Array.uninitIsNop
  val uninitUnsafe = Primitive.Array.uninitUnsafe
  val updateUnsafe = Primitive.Array.updateUnsafe
                                     
  val primArrayAllocUnsafe = Primitive.Array.allocUnsafe
  val primArraySubUnsafe = Primitive.Array.subUnsafe
  val primArrayUpdateUnsafe = Primitive.Array.updateUnsafe
  val primArrayLength = Primitive.Array.length
  val primVectorFromArrayUnsafe = Primitive.Vector.fromArrayUnsafe
  
  structure Raw =
  struct
    type 'a array = 'a array
    open Primitive.Array.Raw
  end
)

structure ArrayAos = MakeArrayPrimSequence(
  type 'a sequence = 'a ArrayAos.t
  type 'a elt = 'a
  type 'a prim_array = 'a ArrayAos.t
  type 'a prim_vector = 'a VectorAos.t
  val sameArray = op =
  val copyUnsafe = Primitive.ArrayAos.copyArrayUnsafe
  val fromArray = fn a => a
  val isMutable = true
  val length = Primitive.ArrayAos.length
  val subUnsafe = Primitive.ArrayAos.subUnsafe
  val uninitIsNop = Primitive.ArrayAos.uninitIsNop
  val uninitUnsafe = Primitive.ArrayAos.uninitUnsafe
  val updateUnsafe = Primitive.ArrayAos.updateUnsafe
                                     
  val primArrayAllocUnsafe = Primitive.ArrayAos.allocUnsafe
  val primArraySubUnsafe = Primitive.ArrayAos.subUnsafe
  val primArrayUpdateUnsafe = Primitive.ArrayAos.updateUnsafe
  val primArrayLength = Primitive.ArrayAos.length
  val primVectorFromArrayUnsafe = Primitive.VectorAos.fromArrayUnsafe
  
  structure Raw =
  struct
    type 'a array = 'a ArrayAos.t
    open Primitive.ArrayAos.Raw
  end
)
         

structure Vector = MakeVectorPrimSequence(
  exception Vector_uninitIsNop
  exception Vector_uninitUnsafe
  exception Vector_updateUnsafe
  type 'a sequence = 'a vector
  type 'a elt = 'a
  type 'a prim_array = 'a array
  type 'a prim_vector = 'a vector
  val copyUnsafe = Primitive.Array.copyVectorUnsafe
  val fromArray = Primitive.Vector.fromArrayUnsafe
  val isMutable = false
  val length = Vector.length
  val sameArray = fn _ => false
  val subUnsafe = Primitive.Vector.subUnsafe
  val uninitIsNop = fn _ =>
                    raise Vector_uninitIsNop
  val uninitUnsafe = fn _ =>
                    raise Vector_uninitUnsafe
  val updateUnsafe = fn _ =>
                    raise Vector_updateUnsafe
                                                        
  val primArrayAllocUnsafe = Primitive.Array.allocUnsafe
  val primArraySubUnsafe = Primitive.Array.subUnsafe
  val primArrayUpdateUnsafe = Primitive.Array.updateUnsafe
  val primArrayLength = Primitive.Array.length
  val primVectorFromArrayUnsafe = Primitive.Vector.fromArrayUnsafe
)


structure VectorAos = MakeVectorPrimSequence(
  exception VectorAos_uninitIsNop
  exception VectorAos_uninitUnsafe
  exception VectorAos_updateUnsafe
  type 'a sequence = 'a VectorAos.t
  type 'a elt = 'a
  type 'a prim_array = 'a ArrayAos.t
  type 'a prim_vector = 'a VectorAos.t
  val copyUnsafe = Primitive.ArrayAos.copyVectorUnsafe
  val fromArray = Primitive.VectorAos.fromArrayUnsafe
  val isMutable = false
  val length = VectorAos.length
  val sameArray = fn _ => false
  val subUnsafe = Primitive.VectorAos.subUnsafe
  val uninitIsNop = fn _ =>
                    raise VectorAos_uninitIsNop
  val uninitUnsafe = fn _ =>
                    raise VectorAos_uninitUnsafe
  val updateUnsafe = fn _ =>
                    raise VectorAos_updateUnsafe
                                                        
  val primArrayAllocUnsafe = Primitive.ArrayAos.allocUnsafe
  val primArraySubUnsafe = Primitive.ArrayAos.subUnsafe
  val primArrayUpdateUnsafe = Primitive.ArrayAos.updateUnsafe
  val primArrayLength = Primitive.ArrayAos.length
  val primVectorFromArrayUnsafe = Primitive.VectorAos.fromArrayUnsafe
)

end

structure Array =
   struct
      type 'a array = 'a array
   end
structure Vector =
   struct
      type 'a vector = 'a vector
   end

structure ArrayAos =
   struct
      type 'a t = 'a Primitive.ArrayAos.t
   end
structure VectorAos =
   struct
      type 'a t = 'a Primitive.VectorAos.t
   end