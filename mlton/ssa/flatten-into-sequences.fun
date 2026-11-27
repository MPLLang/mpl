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
          Vector.forall (Prod.dest args, fn x as {elt, ...} =>
            Option.isNone (try_flatten_tuples x)
            andalso Option.isNone (try_rewrite_type elt))
        then NONE
        else SOME (rewrite_sequence_aos_type args)

    (* Soa-layout sequences are always rewritten: they become a tuple of
     * Default-layout sequences, one per (flattened) ground component. *)
    | Type.Object
        {con = ObjectCon.Sequence ArrayLayout.Soa, args: Type.t Prod.t} =>
        SOME (rewrite_sequence_soa_type args)

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


  (* The ground components of a flattened sequence with element product
   * `args`. Which fields get flattened is decided on the *original* types
   * (as in remap_offset, make_pack_statements and make_unpack_statements);
   * only then is each ground component's type rewritten. This matters because
   * rewriting can change the arity of a type: a Soa sequence (a ground,
   * originally) is rewritten into a tuple of sequences, which must not be
   * flattened further. *)
  and flattened_grounds args : {elt: Type.t, isMutable: bool} vector =
    Vector.map
      (Vector.concatV (Vector.map (Prod.dest args, fn x =>
         case try_flatten_tuples x of
           NONE => Vector.new1 x
         | SOME elements => elements)),
       fn {elt, isMutable} => {elt = rewrite_type elt, isMutable = isMutable})

  and rewrite_sequence_aos_type args =
    Type.sequence ArrayLayout.Aos (Prod.make (flattened_grounds args))

  and soa_grounds args = flattened_grounds args

  (* The Default-layout sequence types that make up a Soa sequence: one per
   * ground component. If there are no ground components (e.g., a unit
   * element type), we still keep a single (fieldless) sequence, so that the
   * length of the Soa sequence is preserved. *)
  and soa_component_types args : Type.t vector =
    let
      val grounds = soa_grounds args
    in
      if Vector.isEmpty grounds then
        Vector.new1 (Type.sequence ArrayLayout.Default (Prod.empty ()))
      else
        Vector.map (grounds, fn g =>
          Type.sequence ArrayLayout.Default (Prod.make (Vector.new1 g)))
    end

  and rewrite_sequence_soa_type args =
    Type.tuple (Prod.make (Vector.map (soa_component_types args, fn ty =>
      {elt = ty, isMutable = false})))


  and rewrite_type ty =
    case try_rewrite_type ty of
      NONE => ty
    | SOME ty' => ty'


  (* A "ground" type is where flattening stops. *)
  fun is_ground_type ty =
    case Type.dest ty of
      Type.Object {con = ObjectCon.Tuple, args} => Prod.someIsMutable args
    | _ => true


  fun soa_args ty : Type.t Prod.t option =
    case Type.dest ty of
      Type.Object {con = ObjectCon.Sequence ArrayLayout.Soa, args} => SOME args
    | _ => NONE

  fun is_soa ty = Option.isSome (soa_args ty)

  (* Layouts whose elements are flattened by this pass. *)
  fun is_flattening_layout lay =
    case lay of
      ArrayLayout.Aos => true
    | ArrayLayout.Soa => true
    | ArrayLayout.Default => false

  fun sequence_layout ty : ArrayLayout.t option =
    case Type.dest ty of
      Type.Object {con = ObjectCon.Sequence lay, ...} => SOME lay
    | _ => NONE


  fun remap_offset sequence_ty offset =
    case Type.dest sequence_ty of
      Type.Object {con = ObjectCon.Sequence lay, args: Type.t Prod.t} =>
        if not (is_flattening_layout lay) then
          Error.bug
            ("FlattenIntoSequences.remap_offset: expected flattened-layout sequence argument, but got "
             ^ Layout.toString (Type.layout sequence_ty))
        else
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
                , Vector.layout Int.layout new_offsets
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
            (* keep the original component types here: the recursive call
             * decides groundness on the original type, and rewrites it when
             * binding *)
            val component_vs_tys = Vector.map (Prod.dest args, fn {elt, ...} =>
              (Var.newNoname (), elt))
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


  (* Select component j (a Default-layout sequence) out of the tuple that a
   * Soa sequence `sequence` (of old type `old_ty`) is represented by. *)
  fun soa_component (sequence: Var.t, old_ty: Type.t, j: int) :
    Statement.t * Var.t * Type.t =
    let
      val comp_tys = soa_component_types (Option.valOf (soa_args old_ty))
      val comp_ty = Vector.sub (comp_tys, j)
      val c = Var.newNoname ()
    in
      ( Statement.Bind
          { var = SOME c
          , ty = comp_ty
          , exp =
              Exp.Select
                {base = Base.Object sequence, offset = j, readBarrier = false}
          }
      , c
      , comp_ty
      )
    end

  (* All components of a Soa sequence. *)
  fun soa_components (sequence: Var.t, old_ty: Type.t) :
    Statement.t vector * Var.t vector =
    let
      val n = Vector.length (soa_component_types (Option.valOf (soa_args old_ty)))
      val (ss, cs) = Vector.unzip (Vector.tabulate (n, fn j =>
        let val (s, c, _) = soa_component (sequence, old_ty, j)
        in (s, c)
        end))
    in
      (ss, cs)
    end


  fun try_transform_select get_var_type (var, ty, base, offset, readBarrier) =
    case base of
      Base.Object _ => NONE
    | Base.SequenceSub {index, sequence} =>
        let
          val old_ty = get_var_type sequence
          val lay = sequence_layout old_ty
        in
          case lay of
            SOME ArrayLayout.Aos =>
              (case try_rewrite_type old_ty of
                 NONE => NONE
               | SOME new_type =>
                   let
                     val (new_offset, ground_count) = remap_offset old_ty offset
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
                           , Type.layout old_ty
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

                     val ground_vs_tys = Vector.zip (ground_vs, ground_tys)
                     val loads =
                       make_load_statements
                         (base, new_offset, ground_vs_tys, readBarrier)

                     val packs = make_pack_statements (var, ty) ground_vs_tys
                   in
                     SOME (Vector.concat [loads, packs])
                   end)

          | SOME ArrayLayout.Soa =>
              let
                val (new_offset, ground_count) = remap_offset old_ty offset
                val grounds = soa_grounds (Option.valOf (soa_args old_ty))
                val (loads, ground_vs_tys) = Vector.unzip
                  (Vector.tabulate (ground_count, fn i =>
                     let
                       val j = new_offset + i
                       val (get_comp, c, _) = soa_component (sequence, old_ty, j)
                       val v = Var.newNoname ()
                       val v_ty = #elt (Vector.sub (grounds, j))
                       val load =
                         Statement.Bind
                           { var = SOME v
                           , ty = v_ty
                           , exp =
                               Exp.Select
                                 { base = Base.SequenceSub {index = index, sequence = c}
                                 , offset = 0
                                 , readBarrier = readBarrier
                                 }
                           }
                     in
                       (Vector.new2 (get_comp, load), (v, v_ty))
                     end))
                val packs = make_pack_statements (var, ty) ground_vs_tys
              in
                SOME (Vector.concat [Vector.concatV loads, packs])
              end

          | _ => NONE
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


  (* Unpack a value about to be stored into a flattened sequence, returning
   * the unpacking statements and the ground vars. *)
  fun unpack_for_store get_var_type (value: Var.t) =
    let
      val value_ty = get_var_type value
    in
      if is_ground_type value_ty then
        (Vector.new0 (), Vector.new1 value)
      else
        make_unpack_statements (value, value_ty)
    end


  fun try_transform_update get_var_type {base, offset, value, writeBarrier} =
    case base of
      Base.Object _ => NONE
    | Base.SequenceSub {index, sequence} =>
        let
          val old_type = get_var_type sequence
        in
          case sequence_layout old_type of
            SOME ArrayLayout.Aos =>
              (case try_rewrite_type old_type of
                 NONE => NONE
               | SOME new_type =>
                   let
                     val (new_offset, ground_count) =
                       remap_offset old_type offset

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
                       unpack_for_store get_var_type value

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
                   end)

          | SOME ArrayLayout.Soa =>
              let
                val (new_offset, ground_count) = remap_offset old_type offset
                val (unpacks, ground_vs) = unpack_for_store get_var_type value
                val () =
                  if ground_count = Vector.length ground_vs then
                    ()
                  else
                    Error.bug
                      ("FlattenIntoSequences.try_transform_update: Soa ground mismatch")
                val stores = Vector.mapi (ground_vs, fn (i, v) =>
                  let
                    val (get_comp, c, _) =
                      soa_component (sequence, old_type, new_offset + i)
                  in
                    Vector.new2
                      ( get_comp
                      , Statement.Update
                          { base = Base.SequenceSub {index = index, sequence = c}
                          , offset = 0
                          , value = v
                          , writeBarrier = writeBarrier
                          }
                      )
                  end)
              in
                SOME (Vector.concat [unpacks, Vector.concatV stores])
              end

          | _ => NONE
        end


  (* ========================================================================
   * sequence literals and sequence primitives
   *)

  (* `args` are the field values of each element of a sequence literal whose
   * (old) sequence type has element product `elt_args`. Returns the unpacking
   * statements, and for each element its ground vars. *)
  fun unpack_sequence_literal get_var_type (elt_args, args) =
    let
      val fields = Prod.dest elt_args
      val (ss, grounds) = Vector.unzip (Vector.map (args, fn elem =>
        let
          val (ss, gs) = Vector.unzip (Vector.mapi (elem, fn (f, x) =>
            if Option.isNone (try_flatten_tuples (Vector.sub (fields, f))) then
              (Vector.new0 (), Vector.new1 x)
            else
              make_unpack_statements (x, get_var_type x)))
        in
          (Vector.concatV ss, Vector.concatV gs)
        end))
    in
      (Vector.concatV ss, grounds)
    end


  fun bool_const (var, b) =
    let
      val con = if b then Con.truee else Con.falsee
      val variant = Var.newNoname ()
    in
      Vector.new2
        ( Statement.Bind
            { var = SOME variant
            , ty = Type.conApp (con, Prod.empty ())
            , exp = Exp.Object {con = SOME con, args = Vector.new0 ()}
            }
        , Statement.Bind
            { var = var
            , ty = Type.bool
            , exp = Exp.Inject {variant = variant, sum = Tycon.bool}
            }
        )
    end


  fun bind_unit var =
    case var of
      NONE => Vector.new0 ()
    | SOME _ =>
        Vector.new1 (Statement.Bind {var = var, ty = Type.unit, exp = Exp.unit})


  (* Lower a primitive applied to Soa sequences into primitives applied to the
   * Default-layout component sequences. Returns NONE when no argument is a
   * Soa sequence (or the result isn't one, for allocation). *)
  fun try_transform_prim get_var_type {var, ty, prim, args} =
    let
      fun arg i = Vector.sub (args, i)
      fun arg_ty i = get_var_type (arg i)
      val new_ty = rewrite_type ty

      fun prim_stmt (var, ty, prim, args) =
        Statement.Bind
          {var = var, ty = ty, exp = Exp.PrimApp {prim = prim, args = args}}

      (* Apply `prim` componentwise. `make_args j cs` builds the arguments for
       * component j, given the components of each Soa argument. The result is
       * either a tuple of the per-component results (when `result_comp_tys`
       * is SOME) or unit. *)
      fun componentwise (soa_arg_idxs, result_comp_tys, make_args) =
        let
          val (gets, comps) = Vector.unzip (Vector.map (soa_arg_idxs, fn i =>
            soa_components (arg i, arg_ty i)))
          val n = Vector.length (Vector.sub (comps, 0))
          val apps = Vector.tabulate (n, fn j =>
            let
              val cs = Vector.map (comps, fn c => Vector.sub (c, j))
            in
              case result_comp_tys of
                NONE => (prim_stmt (NONE, Type.unit, prim, make_args (j, cs)), NONE)
              | SOME tys =>
                  let
                    val r = Var.newNoname ()
                  in
                    ( prim_stmt (SOME r, Vector.sub (tys, j), prim, make_args (j, cs))
                    , SOME r
                    )
                  end
            end)
          val (app_ss, results) = Vector.unzip apps
          val final =
            case result_comp_tys of
              NONE => bind_unit var
            | SOME _ =>
                Vector.new1 (Statement.Bind
                  { var = var
                  , ty = new_ty
                  , exp = Exp.Object
                      {con = NONE, args = Vector.map (results, Option.valOf)}
                  })
        in
          SOME (Vector.concat [Vector.concatV gets, app_ss, final])
        end

      fun result_comp_tys () =
        soa_component_types (Option.valOf (soa_args ty))

      (* Apply a unary prim to component 0 only. *)
      fun on_component0 () =
        let
          val (gets, cs) = Vector.unzip (Vector.map (args, fn x =>
            if is_soa (get_var_type x) then
              let val (s, c, _) = soa_component (x, get_var_type x, 0)
              in (Vector.new1 s, c)
              end
            else (Vector.new0 (), x)))
        in
          SOME (Vector.concat
            [Vector.concatV gets, Vector.new1 (prim_stmt (var, new_ty, prim, cs))])
        end
    in
      case prim of
        Prim.Array_alloc {raw, layout = ArrayLayout.Soa} =>
          (* n -> (alloc n, alloc n, ...) *)
          let
            val comp_tys = result_comp_tys ()
            val (ss, rs) = Vector.unzip (Vector.map (comp_tys, fn cty =>
              let
                val r = Var.newNoname ()
              in
                ( prim_stmt
                    ( SOME r
                    , cty
                    , Prim.Array_alloc {raw = raw, layout = ArrayLayout.Default}
                    , args
                    )
                , r
                )
              end))
          in
            SOME (Vector.concat
              [ ss
              , Vector.new1 (Statement.Bind
                  { var = var
                  , ty = new_ty
                  , exp = Exp.Object {con = NONE, args = rs}
                  })
              ])
          end

      | Prim.Array_alloc {layout = ArrayLayout.Aos, ...} => NONE
      | Prim.Array_alloc {layout = ArrayLayout.Default, ...} => NONE

      | Prim.Array_length =>
          if is_soa (arg_ty 0) then on_component0 () else NONE

      | Prim.MLton_eq =>
          if is_soa (arg_ty 0) then on_component0 () else NONE

      | Prim.MLton_equal =>
          if is_soa (arg_ty 0) then on_component0 () else NONE

      | Prim.Array_toArray =>
          if is_soa (arg_ty 0) then
            componentwise (Vector.new1 0, SOME (result_comp_tys ()), fn (_, cs) => cs)
          else NONE

      | Prim.Array_toVector =>
          if is_soa (arg_ty 0) then
            componentwise (Vector.new1 0, SOME (result_comp_tys ()), fn (_, cs) => cs)
          else NONE

      | Prim.Array_uninit =>
          (* (a, i) -> uninit (a_j, i) for each j *)
          if is_soa (arg_ty 0) then
            componentwise (Vector.new1 0, NONE, fn (_, cs) =>
              Vector.new2 (Vector.sub (cs, 0), arg 1))
          else NONE

      | Prim.Array_copyArray =>
          (* (dst, di, src, si, len) -> copy (dst_j, di, src_j, si, len) *)
          if is_soa (arg_ty 0) then
            componentwise (Vector.new2 (0, 2), NONE, fn (_, cs) =>
              Vector.new5
                (Vector.sub (cs, 0), arg 1, Vector.sub (cs, 1), arg 3, arg 4))
          else NONE

      | Prim.Array_copyVector =>
          if is_soa (arg_ty 0) then
            componentwise (Vector.new2 (0, 2), NONE, fn (_, cs) =>
              Vector.new5
                (Vector.sub (cs, 0), arg 1, Vector.sub (cs, 1), arg 3, arg 4))
          else NONE

      | Prim.Array_uninitIsNop =>
          (* uninit is a nop iff it is a nop for every component. Components
           * holding only scalars are always nops. With exactly one possibly
           * non-scalar component we can ask that component; with more than
           * one, we conservatively answer false (always safe: it just means
           * the caller will uninit each element). *)
          (case soa_args (arg_ty 0) of
             NONE => NONE
           | SOME sargs =>
               let
                 val comp_tys = soa_component_types sargs
                 fun is_scalar_component cty =
                   case Type.dest cty of
                     Type.Object {con = ObjectCon.Sequence _, args} =>
                       Vector.forall (Prod.dest args, fn {elt, ...} =>
                         case Type.dest elt of
                           Type.Real _ => true
                         | Type.Word _ => true
                         | Type.CPointer => true
                         | _ => false)
                   | _ => false
                 val candidates =
                   List.keepAll
                     ( List.tabulate (Vector.length comp_tys, fn j => j)
                     , fn j => not (is_scalar_component (Vector.sub (comp_tys, j)))
                     )
                 fun ask j =
                   let
                     val (s, c, _) = soa_component (arg 0, arg_ty 0, j)
                   in
                     SOME (Vector.new2
                       (s, prim_stmt (var, new_ty, prim, Vector.new1 c)))
                   end
               in
                 case candidates of
                   [] => ask 0
                 | [j] => ask j
                 | _ => SOME (bool_const (var, false))
               end)

      | _ => NONE
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
               ( case var of SOME v => v | NONE => Var.newNoname ()
               , ty, base, offset, readBarrier)
           of
             NONE => no_change ()
           | SOME ss => ss)

      | Exp.PrimApp {prim, args} =>
          (case
             try_transform_prim get_var_type
               {var = var, ty = ty, prim = prim, args = args}
           of
             NONE => no_change ()
           | SOME ss => ss)

      | Exp.Sequence {args} =>
          (* sequence literal, e.g. from Vector_vector or Array_array *)
          (case Type.dest ty of
             Type.Object {con = ObjectCon.Sequence ArrayLayout.Aos, args = elt_args} =>
               if Option.isNone (try_rewrite_type ty) then no_change ()
               else
                 let
                   val (ss, grounds) =
                     unpack_sequence_literal get_var_type (elt_args, args)
                 in
                   Vector.concat [ss, Vector.new1 (Statement.Bind
                     { var = var
                     , ty = rewrite_type ty
                     , exp = Exp.Sequence {args = grounds}
                     })]
                 end

           | Type.Object {con = ObjectCon.Sequence ArrayLayout.Soa, args = elt_args} =>
               let
                 val (ss, grounds) =
                   unpack_sequence_literal get_var_type (elt_args, args)
                 val comp_tys = soa_component_types elt_args
                 val no_grounds = Vector.isEmpty (soa_grounds elt_args)
                 val (comp_ss, comp_vs) = Vector.unzip (Vector.mapi (comp_tys, fn (j, cty) =>
                   let
                     val c = Var.newNoname ()
                     val elems = Vector.map (grounds, fn g =>
                       if no_grounds then Vector.new0 ()
                       else Vector.new1 (Vector.sub (g, j)))
                   in
                     ( Statement.Bind
                         {var = SOME c, ty = cty, exp = Exp.Sequence {args = elems}}
                     , c
                     )
                   end))
               in
                 Vector.concat
                   [ ss
                   , comp_ss
                   , Vector.new1 (Statement.Bind
                       { var = var
                       , ty = rewrite_type ty
                       , exp = Exp.Object {con = NONE, args = comp_vs}
                       })
                   ]
               end

           | _ => no_change ())

      | _ => no_change ()
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
