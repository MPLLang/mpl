(* Copyright (C) 2024 Colin McDonald.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

signature SPORK_NESTING_STRUCTS =
sig
  include RSSA_TRANSFORM_STRUCTS
end

signature SPORK_NESTING =
sig
  include SPORK_NESTING_STRUCTS
  val nesting: Function.t -> (Spid.t -> int)
end

functor SporkNesting (S : SPORK_NESTING_STRUCTS): SPORK_NESTING =
struct
  open S
  fun nesting (f : Function.t) : (Spid.t -> int) =
      let val init =
              (0, fn (id : Spid.t) =>
                     Error.bug ("Rssa.SporkNesting: bad spid lookup "
                                ^ Layout.toString (Spid.layout id)))
          fun node (Tree.T (Block.T {transfer, ...}, ts) : Block.t Tree.t,
                    (depth : int, lup : Spid.t -> int)) : (int * (Spid.t -> int)) =
              let val (depth', lup') = case transfer of
                      Transfer.Spork {spid = id, ...} =>
                        (depth + 1,
                         fn id' => if Spid.equals (id, id') then
                                     depth
                                   else
                                     lup id')
                    | Transfer.Spoin {spid, ...} => (depth - 1, lup)
                    | _ => (depth, lup)
              in
                Tree.Seq.fold (ts, (depth', lup'), node)
              end
      in
        #2 (node (Function.dominatorTree f, init))
      end
end
