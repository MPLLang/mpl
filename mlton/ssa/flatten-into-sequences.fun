functor FlattenIntoSequences(S: SSA2_TRANSFORM_STRUCTS): SSA2_TRANSFORM =
struct
  open S


  (* ========================================================================
   * just some quick utilities
   *)

  fun vector_iterate_prefixes (f: 'b * 'a -> 'b) (b: 'b) (v: 'a Vector.t) =
    let
      fun loop prev_accs acc i =
        if i >= Vector.length v then (Vector.fromListRev prev_accs, acc)
        else loop (acc :: prev_accs) (f (acc, Vector.sub (v, i))) (i + 1)
    in
      loop [] b 0
    end


  (* ========================================================================
   * type rewrites
   *)


  (* returns NONE if flattening has no change *)
  fun try_flatten_tuples (x as {elt: Type.t, isMutable: bool}) :
    {elt: Type.t, isMutable: bool} vector option =
    case Type.dest elt of
      Type.Object {con = ObjectCon.Tuple, args} =>
        if Prod.someIsMutable args then
          NONE
        else
          let
            val flattened = Vector.concatV (Vector.map (Prod.dest args, fn x =>
              case try_flatten_tuples x of
                NONE => Vector.new1 x
              | SOME elements => elements))
            val flat_with_mutability_propagated =
              Vector.map (flattened, fn {elt, isMutable = isMutable'} =>
                {elt = elt, isMutable = isMutable orelse isMutable'})
          in
            SOME flat_with_mutability_propagated
          end

    | _ => NONE


  (* all types can be locally rewritten without any context *)
  fun try_rewrite_type (ty: Type.t) : Type.t option =
    case Type.dest ty of
    (* Aos-layout sequences get their tuples flattened and unboxed *)
      Type.Object
        {con = ObjectCon.Sequence ArrayLayout.Aos, args: Type.t Prod.t} =>
        if
          Vector.forall (Prod.dest args, fn {elt, isMutable} =>
            Option.isNone (Option.andThen (try_rewrite_type elt, fn elt' =>
              try_flatten_tuples {elt = elt', isMutable = isMutable})))
        then NONE
        else SOME (rewrite_sequence_aos_type args)

    (* Default-layout sequences potentially need their element types rewritten,
     * but aren't flattened here. Note that deep flattening may still occur,
     * but isn't mandated. *)
    | Type.Object
        {con = ObjectCon.Sequence ArrayLayout.Default, args: Type.t Prod.t} =>
        if
          Vector.forall (Prod.dest args, fn {elt, ...} =>
            Option.isNone (try_rewrite_type elt))
        then
          NONE
        else
          SOME (Type.sequence ArrayLayout.Default
            (Prod.map (args, rewrite_type)))

    | Type.Object {con, args} =>
        if
          Vector.forall (Prod.dest args, fn {elt, ...} =>
            Option.isNone (try_rewrite_type elt))
        then
          NONE
        else
          SOME (Type.object {con = con, args = Prod.map (args, rewrite_type)})

    | Type.Weak ty' => Option.map (try_rewrite_type ty', Type.weak)
    | Type.CPointer => NONE
    | Type.IntInf => NONE
    | Type.Thread => NONE
    | Type.Datatype tycon => NONE
    | Type.Real real_size => NONE
    | Type.Word word_size => NONE


  and rewrite_sequence_aos_type args =
    let
      val rewritten = Prod.map (args, rewrite_type)
      val flat_and_rewritten =
        Prod.make (Vector.concatV (Vector.map (Prod.dest rewritten, fn x =>
          case try_flatten_tuples x of
            NONE => Vector.new1 x
          | SOME elements => elements)))
    in
      Type.sequence ArrayLayout.Default flat_and_rewritten
    end


  and rewrite_sequence_soa_type args =
    let
      val rewritten = Prod.map (args, rewrite_type)
      val flat_and_rewritten =
        Prod.make (Vector.concatV (Vector.map (Prod.dest rewritten, fn x =>
          case try_flatten_tuples x of
            NONE => Vector.new1 x
          | SOME elements => elements)))

      fun make_one_sequence_component {elt, isMutable} =
        { elt = Type.sequence ArrayLayout.Default (Prod.make
            (Vector.new1 {elt = elt, isMutable = isMutable}))
        , isMutable = false
        }
      val soa = Type.tuple (Prod.make
        (Vector.map (Prod.dest flat_and_rewritten, make_one_sequence_component)))
    in
      soa
    end


  and rewrite_type ty =
    case try_rewrite_type ty of
      NONE => ty
    | SOME ty' => ty'


  (* A "ground" type is where flattening stops. *)
  fun is_ground_type ty =
    case Type.dest ty of
      Type.Object {con = ObjectCon.Tuple, args} => Prod.someIsMutable args
    | _ => true


  fun remap_offset sequence_ty offset =
    case Type.dest sequence_ty of
      Type.Object {con = ObjectCon.Sequence ArrayLayout.Aos, args: Type.t Prod.t} =>
        let
          val lens = Vector.map (Prod.dest args, fn x =>
            case try_flatten_tuples x of
              NONE => 1
            | SOME elts => Vector.length elts)
          val (new_offsets, _) = vector_iterate_prefixes op+ 0 lens

          val (new_offset, count) =
            (Vector.sub (new_offsets, offset), Vector.sub (lens, offset))

          val () = Control.diagnostics (fn show =>
            let
              open Layout
            in
              show (seq
                [ str "remap_offset "
                , Type.layout sequence_ty
                , str " "
                , Int.layout offset
                , str "; lens = "
                , Vector.layout Int.layout lens
                , str "; new_offsets = "
                , Vector.layout Int.layout lens
                , str "; result = "
                , Int.layout new_offset
                , str " "
                , Int.layout count
                ])
            end)
        in
          (new_offset, count)
        end

    | _ =>
        Error.bug
          ("FlattenIntoSequences.remap_offset: expected flattened-layout sequence argument, but got "
           ^ Layout.toString (Type.layout sequence_ty))


  (* ========================================================================
   * rewriting expressions, statements, blocks, transfers
   *)


  (* reconstruct var:ty from the flattened ground elements in ground_vs_tys
   * for example:
   *   to reconstruct x:(int*(real*bool))
   *   from [i:int, r:real, b:bool]
   *   we generate the following statements:
   *     x_inner = Object.Tuple(r, b)
   *     x = Object.Tuple(i, x_inner)
   *)
  fun make_pack_statements (var, ty) ground_vs_tys =
    if is_ground_type ty then
      let
        val () =
          if Vector.length ground_vs_tys = 1 then
            ()
          else
            Error.bug
              ("FlattenIntoSequences.make_pack_statements: ground mismatch")
        val (var_src, ty_src) = Vector.sub (ground_vs_tys, 0)
        (* val () =
          if same_type (rewrite_type ty, ty_src) then
            ()
          else
            Error.bug
              ("FlattenIntoSequences.make_pack_statements: type mismatch: "
               ^
               Layout.toString (Layout.seq
                 [ Type.layout (rewrite_type ty)
                 , Layout.str " "
                 , Type.layout ty_src
                 ])) *)
      in
        Vector.new1
          (Statement.Bind
             {var = SOME var, ty = rewrite_type ty, exp = Exp.Var var_src})
      end
    else
      case Type.dest ty of
        Type.Object {con = ObjectCon.Tuple, args} =>
          let
            val lens = Vector.map (Prod.dest args, fn x =>
              case try_flatten_tuples x of
                NONE => 1
              | SOME elts => Vector.length elts)
            val (ground_starts, _) = vector_iterate_prefixes op+ 0 lens
            val component_vs_tys = Vector.map (Prod.dest args, fn {elt, ...} =>
              (Var.newNoname (), rewrite_type elt))
            val packs =
              Vector.concatV
                (Vector.mapi (component_vs_tys, fn (i, (v', ty')) =>
                   let
                     val ground_start = Vector.sub (ground_starts, i)
                     val ground_len = Vector.sub (lens, i)
                     val grounds = Vector.tabulate (ground_len, fn j =>
                       Vector.sub (ground_vs_tys, ground_start + j))
                   in
                     make_pack_statements (v', ty') grounds
                   end))
            val final = Statement.Bind
              { var = SOME var
              , ty = rewrite_type ty
              , exp = Exp.Object
                  {con = NONE, args = Vector.map (component_vs_tys, #1)}
              }
          in
            Vector.concat [packs, Vector.new1 final]
          end
      | _ =>
          Error.bug
            ("FlattenIntoSequences.make_pack_statements: attempting to pack non-tuple")


  fun make_load_statements (base, offset, ground_vs_tys, readBarrier) =
    Vector.mapi (ground_vs_tys, fn (idx, (v, ty)) =>
      Statement.Bind
        { var = SOME v
        , ty = ty
        , exp =
            Exp.Select
              {base = base, offset = offset + idx, readBarrier = readBarrier}
        })


  fun try_transform_select get_var_type (var, ty, base, offset, readBarrier) =
    case base of
      Base.Object _ => NONE
    | Base.SequenceSub {index, sequence} =>
        case try_rewrite_type (get_var_type sequence) of
          NONE => NONE
        | SOME new_type =>
            let
              val (new_offset, ground_count) =
                remap_offset (get_var_type sequence) offset
              val ground_vs = Vector.tabulate (ground_count, fn _ =>
                Var.newNoname ())
              val ground_tys =
                case Type.dest new_type of
                  Type.Object {con = ObjectCon.Sequence ArrayLayout.Aos, args} =>
                    let
                      val args = Prod.dest args
                    in
                      Vector.tabulate (ground_count, fn i =>
                        #elt (Vector.sub (args, new_offset + i)))
                    end
                | _ =>
                    Error.bug
                      ("FlattenIntoSequences.try_transform_select: bug!")

              val () = Control.diagnostics (fn show =>
                let
                  open Layout
                in
                  show (seq
                    [ str "try_transform_select "
                    , Type.layout (get_var_type sequence)
                    , str " "
                    , Int.layout offset
                    , str " -> "
                    , Type.layout new_type
                    , str " "
                    , Int.layout new_offset
                    , str " "
                    , Int.layout ground_count
                    , str "; ground_tys = "
                    , Vector.layout Type.layout ground_tys
                    ])
                end)

              val () =
                (* sanity check *)
                if Vector.length ground_tys = ground_count then
                  ()
                else
                  Error.bug
                    ("FlattenIntoSequences.try_transform_select: ground mismatch")

              val ground_vs_tys = Vector.zip (ground_vs, ground_tys)
              val loads =
                make_load_statements
                  (base, new_offset, ground_vs_tys, readBarrier)

              val packs = make_pack_statements (var, ty) ground_vs_tys
            in
              SOME (Vector.concat [loads, packs])
            end


  fun transform_bind get_var_type {exp, ty, var} =
    let
      fun no_change () =
        Vector.new1
          (Statement.Bind {exp = exp, ty = rewrite_type ty, var = var})
    in
      case exp of
        Exp.Select {base, offset, readBarrier} =>
          (case
             try_transform_select get_var_type
               (Option.valOf var, ty, base, offset, readBarrier)
           of
             NONE => no_change ()
           | SOME ss => ss)

      | _ => no_change ()
    end


  (* v:ty must be a tuple of all immutable fields, which might recursively
   * contain immutable tuples in some positions. Here, we are unpacking
   * its contents in preparation for a flattened store in a sequence.
   *
   * The idea is to replace
   *   S[i] := v
   * with something like this:
   *   // unpacking part, handling nested flattens as necessary
   *   v0 = #0 v
   *   v1 = #1 v
   *   ...
   *   vn = ...
   *   // a bunch of stores
   *   S[i][0] := v0
   *   S[i][1] := v1
   *   ...
   *   S[i][n] := vn
   *
   * This function returns (ss, vs) where:
   *   - ss is all of the unpacking statements, and
   *   - vs is all of the final unpacked vars, in the correct order.
   *     (There will be exactly one store statement generated per v in vs.)
   *
   * Note that `ss` could be larger than `vs` due to nesting. For example,
   * if we unpack a tuple `x = (1, (2, 3))` then we get the following `ss`:
   *   x0 = #0 x
   *   x_inner = #1 x
   *   x1 = #0 x_inner
   *   x2 = #1 x_inner
   * but only three `vs`, one for each ground component:
   *   [ x0, x1, x2 ]
   *)
  fun make_unpack_statements (v: Var.t, ty: Type.t) :
    Statement.t vector * Var.t vector =
    let
      val () = Control.diagnostics (fn show =>
        let
          open Layout
        in
          show
            (seq
               [ str "make_unpack_statements "
               , Var.layout v
               , str " "
               , Type.layout ty
               ])
        end)

      fun error msg =
        Error.bug
          ("FlattenIntoSequences.make_unpack_statements: " ^ msg ^ ":  "
           ^ Layout.toString (Var.layout v) ^ " of type "
           ^ Layout.toString (Type.layout ty))

      fun unpack_one (v', ty') idx =
        Statement.Bind
          { var = SOME v'
          , ty = rewrite_type ty'
          , exp =
              Exp.Select
                {base = Base.Object v, offset = idx, readBarrier = false}
          }

      fun unpack_component_at_idx (idx, {elt = component_ty, isMutable}) =
        if isMutable then
          error
            ("trying to unpack mutable component at tuple index "
             ^ Int.toString idx)
        else
          let
            val component_var = Var.newNoname ()
            val unpack_here = unpack_one (component_var, component_ty) idx
          in
            (* stop recursively unpacking when we get to the bottom *)
            if is_ground_type component_ty then
              (Vector.new1 unpack_here, Vector.new1 component_var)
            else
              let
                val (nested_unpacks, nested_grounds) =
                  make_unpack_statements (component_var, component_ty)
              in
                ( Vector.concat [Vector.new1 unpack_here, nested_unpacks]
                , nested_grounds
                )
              end
          end
    in
      case Type.dest ty of
        Type.Object {con = ObjectCon.Tuple, args} =>
          let
            val (unpacks, grounds) = Vector.unzip
              (Vector.mapi (Prod.dest args, unpack_component_at_idx))
          in
            (Vector.concatV unpacks, Vector.concatV grounds)
          end
      | _ => error "trying to unpack non-tuple"
    end


  fun make_store_statements (base, offset, ground_vs, writeBarrier) =
    Vector.mapi (ground_vs, fn (idx, v) =>
      Statement.Update
        { base = base
        , offset = offset + idx
        , value = v
        , writeBarrier = writeBarrier
        })


  fun try_transform_update get_var_type {base, offset, value, writeBarrier} =
    case base of
      Base.Object _ => NONE
    | Base.SequenceSub {index, sequence} =>
        case try_rewrite_type (get_var_type sequence) of
          NONE => NONE
        | SOME new_type =>
            let
              val old_type = get_var_type sequence
              val (new_offset, ground_count) =
                remap_offset (get_var_type sequence) offset

              val () = Control.diagnostics (fn show =>
                let
                  open Layout
                in
                  show (seq
                    [ str "try_transform_update "
                    , Type.layout old_type
                    , str " "
                    , Int.layout offset
                    , str " -> "
                    , Type.layout new_type
                    , str " "
                    , Int.layout new_offset
                    , str " "
                    , Int.layout ground_count
                    ])
                end)

              val (unpacks, ground_vs) =
                if is_ground_type (get_var_type value) then
                  (Vector.new0 (), Vector.new1 value)
                else
                  make_unpack_statements (value, get_var_type value)

              val () =
                (* sanity check *)
                if ground_count = Vector.length ground_vs then
                  ()
                else
                  Error.bug
                    ("FlattenIntoSequences.try_transform_update: ground mismatch")
              val stores =
                make_store_statements
                  (base, new_offset, ground_vs, writeBarrier)
            in
              SOME (Vector.concat [unpacks, stores])
            end


  fun transform_statement get_var_type (s: Statement.t) : Statement.t vector =
    case s of
      Statement.Bind (xx as {exp: Exp.t, ty: Type.t, var: Var.t option}) =>
        transform_bind get_var_type xx
    | Statement.Profile _ => Vector.new1 s
    | Statement.Update
        (xx as
           {base: Var.t Base.t, offset: int, value: Var.t, writeBarrier: bool}) =>
        case try_transform_update get_var_type xx of
          NONE => Vector.new1 s
        | SOME ss => ss


  fun transform_transfer get_var_type t =
    case t of
      Transfer.Runtime {args, prim, return} =>
        Transfer.Runtime
          {args = args, prim = Prim.map (prim, rewrite_type), return = return}
    | _ => t


  fun transform_block get_var_type block =
    let
      val
        Block.T
          { args: (Var.t * Type.t) vector
          , label: Label.t
          , statements: Statement.t vector
          , transfer: Transfer.t
          } = block

      val args = Vector.map (args, fn (var, ty) => (var, rewrite_type ty))
      val statements = Vector.concatV
        (Vector.map (statements, transform_statement get_var_type))
      val transfer = transform_transfer get_var_type transfer
    in
      Block.T
        { args = args
        , label = label
        , statements = statements
        , transfer = transfer
        }
    end


  fun transform_function get_var_type (func: Function.t) : Function.t =
    let
      val
        { args: (Var.t * Type.t) vector
        , blocks: Block.t vector
        , inline: InlineAttr.t
        , name: Func.t
        , raises: Type.t vector option
        , returns: Type.t vector option
        , start: Label.t
        } = Function.dest func

      val args = Vector.map (args, fn (var, ty) => (var, rewrite_type ty))
      val raises = Option.map (raises, fn ts => Vector.map (ts, rewrite_type))
      val returns = Option.map (returns, fn ts => Vector.map (ts, rewrite_type))
      val blocks = Vector.map (blocks, transform_block get_var_type)
    in
      Function.new
        { args = args
        , blocks = blocks
        , inline = inline
        , name = name
        , raises = raises
        , returns = returns
        , start = start
        }
    end


  fun transform_datatype get_var_type (Datatype.T {cons, tycon: Tycon.t}) =
    let
      val cons: {args: Type.t Prod.t, con: Con.t} vector = cons
    in
      Datatype.T
        { cons = Vector.map (cons, fn {args, con} =>
            {con = con, args = Prod.map (args, rewrite_type)})
        , tycon = tycon
        }
    end


  (* ========================================================================
   * main entrypoint for this pass
   *)


  (* would want to keep this turned on in practice, but can disable for
   * debugging. (flattenIntoSequences relies on shrinking to avoid
   * unnecessary intermediate allocations, but shrinking obscures what
   * the pass did.)
   *)
  val do_shrink = true


  fun transform2 (program as Program.T {datatypes, functions, globals, main}) =
    let
      val {get = get_var_type: Var.t -> Type.t, set = set_var_type, ...} =
        Property.getSetOnce
          (Var.plist, Property.initRaise ("varType", Var.layout))

      val () = Program.foreachVar (program, set_var_type)

      val datatypes = Vector.map (datatypes, transform_datatype get_var_type)

      val functions =
        if do_shrink then
          List.revMap (functions, transform_function get_var_type)
        else
          List.map (functions, transform_function get_var_type)

      val globals = Vector.concatV
        (Vector.map (globals, transform_statement get_var_type))

      val program = Program.T
        { datatypes = datatypes
        , functions = functions
        , globals = globals
        , main = main
        }

      val () = Program.clear program
    in
      if do_shrink then shrink program else program
    end
end
