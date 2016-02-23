(* Copyright (C) 2015 Ram Raghunathan
 *
 * MLton is released under a BSD-style license.
 * See the file MLton-LICENSE for details.
 *)

structure MLtonHM:> MLTON_HM =
    struct
        structure PrimHM = Primitive.MLton.HM

        structure HierarchicalHeap =
            struct
                structure PrimHH = PrimHM.HierarchicalHeap

                datatype 'a hierarchicalHeap = Invalid |
                                               Unit of 'a PrimHH.t |
                                               Poly of 'a PrimHH.t
                datatype 'a t = T of 'a hierarchicalHeap ref

                fun die (f : string) (s: string): 'a =
                    (PrimitiveFFI.Stdio.print (String.concat
                                                   [f, ": ", s, "\n"]);
                     PrimitiveFFI.Posix.Process.exit 1;
                     let
                         exception DieFailed
                     in
                         raise DieFailed
                     end)

                fun applyToHH (caller : string)
                              (invalidMsg : string)
                              (f : 'a PrimHH.t -> 'b)
                              (T (ref hh) : 'a t) =
                    case hh
                     of Invalid => die caller invalidMsg
                      | Unit hh => f hh
                      | Poly hh => f hh

                fun new ((): unit) : unit t =
                    let
                        val hh : unit PrimHH.t = PrimHH.newHierarchicalHeap ()
                    in
                        T (ref (Unit (hh)))
                    end

                val set : 'a t -> unit =
                 fn hh => applyToHH "MLtonHM.HierarchicalHeap.set"
                                    "Tried to set Invalid HH"
                                    PrimHH.setHierarchicalHeap
                                    hh

                fun get (() : unit) : unit t =
                    T (ref (Unit (PrimHH.getHierarchicalHeap ())))

                fun setLevel ((hh, level) : 'a t * int) : unit =
                    applyToHH "MLtonHM.HierarchicalHeap.setLevel"
                              "Tried to set level on Invalid HH"
                              (fn hh => PrimHH.setLevel (hh,
                                                         Word32.fromInt level))
                              hh

                val getLevel : 'a t -> int =
                 fn hh => applyToHH "MLtonHM.HierarchicalHeap.getLevel"
                                    "Tried to get level of Invalid HH"
                                    (Word32.toInt o PrimHH.getLevel)
                                    hh

                fun appendChild ((T (ref parentHH),
                                  T (ref childHH),
                                  stealLevel) : ('a t * 'b t * int)) : unit =
                    let
                        val die = die "MLtonHM.HierarchicalHeap.appendChild"
                        val stealLevel = Word32.fromInt stealLevel
                    in
                        case (parentHH, childHH)
                         of (Invalid, _) =>
                            die "Tried to append to Invalid parent"
                          | (Unit _, Invalid) =>
                            die "Tried to append Invalid child"
                          | (Poly _, Invalid)=>
                            die "Tried to append Invalid child"

                          | (Unit parentHH, Unit childHH) =>
                            PrimHH.appendChildHeap (parentHH,
                                                    childHH,
                                                    stealLevel)
                          | (Unit parentHH, Poly childHH) =>
                            PrimHH.appendChildHeap (parentHH,
                                                    childHH,
                                                    stealLevel)
                          | (Poly parentHH, Unit childHH) =>
                            PrimHH.appendChildHeap (parentHH,
                                                    childHH,
                                                    stealLevel)
                          | (Poly parentHH, Poly childHH) =>
                            PrimHH.appendChildHeap (parentHH,
                                                    childHH,
                                                    stealLevel)
                    end

                fun setReturnValue ((hh as T hhr, r) : 'a t * 'b) : 'b t =
                    let
                        val newHH =
                            applyToHH "MLtonHM.HierarchicalHeap.setReturnValue"
                                      "Tried to set return value of Invalid HH"
                                      (fn hh => PrimHH.setReturnValue (hh, r))
                                      hh
                    in
                        hhr := Invalid;
                        T (ref (Poly newHH))
                    end

                fun mergeIntoParentAndGetReturnValue (T hhr : 'a t) : 'a option =
                    let
                        val die = die ("MLtonHM.HierarchicalHeap." ^
                                       "mergeIntoParentHeapAndGetReturnValue")

                        val r =
                            case !hhr
                             of Invalid => die "Tried to merge Invalid HH"
                              | Unit hh => (PrimHH.mergeIntoParentHeap hh;
                                            NONE)
                              | Poly hh =>
                                SOME (PrimHH.mergeIntoParentHeapAndGetReturnValue hh)
                    in
                        hhr := Invalid;
                        r
                    end

                val promoteChunks : 'a t -> unit =
                 fn hh => applyToHH "MLtonHM.HierarchicalHeap.getLevel"
                                    "Tried to get level of Invalid HH"
                                    PrimHH.promoteChunks
                                    hh

                val useHierarchicalHeap: unit -> unit =
                    PrimHH.setCurrentThreadUseHierarchicalHeap
            end

        val enterGlobalHeap: unit -> unit = PrimHM.enterGlobalHeap
        val exitGlobalHeap: unit -> unit = PrimHM.exitGlobalHeap
        val explicitEnterGlobalHeap: Word32.word -> unit =
            PrimHM.explicitEnterGlobalHeap
        val explicitExitGlobalHeap: unit -> Word32.word =
            PrimHM.explicitExitGlobalHeap

        val registerQueue: Word32.word * 'a array -> unit = PrimHM.registerQueue
        val registerQueueLock: Word32.word * int ref -> unit =
            PrimHM.registerQueueLock
    end
