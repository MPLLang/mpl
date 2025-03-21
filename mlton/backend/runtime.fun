(* Copyright (C) 2024 Sam Westrick.
 * Copyright (C) 2009,2016-2017,2019-2022 Matthew Fluet.
 * Copyright (C) 2002-2007 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

functor Runtime (S: RUNTIME_STRUCTS): RUNTIME =
struct

open S

structure GCField =
   struct
      datatype t =
         AtomicState
       | CurSourceSeqIndex
       | ExnStack
       | Frontier
       | Limit
       | LimitPlusSlop
       | PromoStackTop
       | PromoStackBot
       | SignalIsPending
       | SpareHeartbeatTokens
       | StackBottom
       | StackLimit
       | StackTop

      local
         fun make name =
            Bytes.fromIntInf
            (Control.StrMap.lookupIntInf
             (Promise.force Control.Target.consts,
              "offset::gcState." ^ name))
      in
         val offset =
            fn AtomicState => make "atomicState"
             | CurSourceSeqIndex => make "sourceMaps.curSourceSeqIndex"
             | ExnStack => make "exnStack"
             | Frontier => make "frontier"
             | Limit => make "limit"
             | LimitPlusSlop => make "limitPlusSlop"
             | PromoStackTop => make "promoStackTop"
             | PromoStackBot => make "promoStackBot"
             | SignalIsPending => make "signalsInfo.signalIsPending"
             | SpareHeartbeatTokens => make "spareHeartbeatTokens"
             | StackBottom => make "stackBottom"
             | StackLimit => make "stackLimit"
             | StackTop => make "stackTop"
      end

      val toString =
         fn AtomicState => "AtomicState"
          | CurSourceSeqIndex => "CurSourceSeqIndex"
          | ExnStack => "ExnStack"
          | Frontier => "Frontier"
          | Limit => "Limit"
          | LimitPlusSlop => "LimitPlusSlop"
          | PromoStackTop => "PromoStackTop"
          | PromoStackBot => "PromoStackBot"
          | SignalIsPending => "SignalIsPending"
          | SpareHeartbeatTokens => "SpareHeartbeatTokens"
          | StackBottom => "StackBottom"
          | StackLimit => "StackLimit"
          | StackTop => "StackTop"

      val layout = Layout.str o toString

      val volatile =
         fn AtomicState => true
          | CurSourceSeqIndex => true
          | Limit => true
          | SignalIsPending => true
          | StackTop => let
                          (*
                           * `stackTop` is marked `volatile` in
                           * `struct GC_state` because it is read by
                           * `GC_handleSigProf` for time profiling.
                           *)
                        in
                           !Control.profile = Control.ProfileTime
                        end
          | StackBottom => let
                             (*
                              * `stackBottom` is marked `volatile` in
                              * `struct GC_state` because it is read by
                              * `GC_handleSigProf` for time profiling.  The
                              * mutator does not need to treat `stackBottom` as
                              * `volatile`, because it only reads `stackBottom`
                              * and only the (synchronously executed) GC writes
                              * `stackBottom`.
                              *)
                           in
                              false
                           end
          | _ => false end

structure RObjectType =
   struct
      datatype t =
         Normal of {hasIdentity: bool,
                   bytesNonObjptrs: Bytes.t,
                   numObjptrs: int}
       | Sequence of {hasIdentity: bool,
                    bytesNonObjptrs: Bytes.t,
                    numObjptrs: int}
       | Stack
       | Weak of {gone: bool}

      fun layout (t: t): Layout.t =
         let
            open Layout
         in
            case t of
               Normal {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Normal ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Sequence {hasIdentity, bytesNonObjptrs, numObjptrs} =>
                  seq [str "Sequence ",
                       record [("hasIdentity", Bool.layout hasIdentity),
                               ("bytesNonObjptrs", Bytes.layout bytesNonObjptrs),
                               ("numObjptrs", Int.layout numObjptrs)]]
             | Stack => str "Stack"
             | Weak {gone} =>
                  seq [str "Weak",
                       record [("gone", Bool.layout gone)]]

         end
      val _ = layout (* quell unused warning *)
   end

(* see gc/object.h *)
local
   val maxTypeIndex = Int.pow (2, 19)
in
   (* see gc/object.c:buildHeaderFromTypeIndex *)
   fun typeIndexToHeader typeIndex =
      (Assert.assert ("Runtime.header", fn () =>
                      0 <= typeIndex
                      andalso typeIndex < maxTypeIndex)
       ; Word.orb (0w1, Word.<< (Word.fromInt typeIndex, 0w1)))
end

(* see gc/object.h *)
val objptrSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.objptr)

(* see gc/object.h *)
val headerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.header)
val headerOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (headerSize ()))

(* see gc/sequence.h *)
val sequenceLengthSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.seqIndex)
val sequenceLengthOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (Bytes.+ (headerSize (),
                                            sequenceLengthSize ())))

(* see gc/sequence.h *)
val sequenceCounterSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.seqIndex)
val sequenceCounterOffset : unit -> Bytes.t =
   Promise.lazy (fn () => Bytes.~ (Bytes.+ (Bytes.+ (headerSize (),
                                                     sequenceLengthSize ()),
                                            sequenceCounterSize ())))

(* see gc/object.h and gc/sequence.h *)
val sequenceMetaDataSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.sequenceMetaData)
val normalMetaDataSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.normalMetaData)

val cpointerSize : unit -> Bytes.t =
   Promise.lazy (Bits.toBytes o Control.Target.Size.cpointer)
val labelSize = cpointerSize

(* See gc/heap.h. *)
val limitSlop = Bytes.fromInt 512

(* See gc/frame.h. *)
val maxFrameSize = Bytes.fromInt (Int.pow (2, 16))

end
