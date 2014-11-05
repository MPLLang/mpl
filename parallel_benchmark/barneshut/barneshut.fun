functor BarnesHut (A: SIMARG) : SIM =
struct

  open A
                  (* #particles  C.O.M.    moment  bounding  children *)
  datatype t = Node of int * pointmass * Matrix.t * box * children
  and children = Octants of t list (* at least one child has a particle *)
               | Particles of particle list (* will never be split, >= 2 particles *)
               | Particle of particle (* for boxes with exactly one particle *)
               | Empty (* contains no particles *)

  fun fold fb fp x (Node (_, _, _, box, children)) = 
      let 
        val x = fb (box, x)
      in
        fold' fb fp x children
      end
  and fold' fb fp x (Octants ts) =
      foldl (fn (t, x) => fold fb fp x t) x ts
    | fold' _ fp x (Particles ps) =
      foldl fp x ps
    | fold' _ fp x (Particle p) = fp (p, x)
    | fold' _ _ x Empty = x

  fun empty box = Node (0, { position = Vector.zero, mass = 0.0 },
                        Matrix.zero, box, Empty)

  (* Make the i'th octant of box *)
  fun octant (box : box) i =
      let 
        val op* = Vector.scale

        val (x, y, z) = #position box
        val (dimension as (dx, dy, dz)) = 0.5 * (#dimension box)

        fun myEmpty position = empty { position = position, 
                                       dimension = dimension }
      in
        case i of 0 => myEmpty (x, y, z)
                | 1 => myEmpty (x + dx, y, z)
                | 2 => myEmpty (x, y + dy, z)
                | 3 => myEmpty (x + dx, y + dy, z)
                | 4 => myEmpty (x, y, z + dz)
                | 5 => myEmpty (x + dx, y, z + dz)
                | 6 => myEmpty (x, y + dy, z + dz)
                | 7 => myEmpty (x + dx, y + dy, z + dz)
                | _ => raise Sim "can only build 8 octants"
      end

  fun boxToString { position = (x, y, z), dimension = (dx, dy, dz) } =
      concat ["(", Real.toString x, " + ", Real.toString dx, ", ",
              Real.toString y, " + ", Real.toString dy, ", ",
              Real.toString z, " + ", Real.toString dz, ")"]

  fun contains (box as { position, dimension }) pos =
      let
        val op+ = Vector.+
        val op< = Vector.<
        val op<= = Vector.<=
      in
        position <= pos andalso
        pos < position + dimension
      end

  fun printTree (box, children) =
      let
        fun pr prefix (box, children) =
            let
              val () = print (concat [prefix, boxToString box, " ",
                                      case children of
                                        Particle p => "1"
                                      | Particles ps => (Int.toString (length ps))
                                      | _ => "", "\n"])
            in
              case children of
                Octants ts => app (fn (Node (_, _, _, box, children)) => 
                                       pr (prefix ^ "  ") (box, children)) ts
              | _ => ()
            end
      in
        pr "" (box, children)
      end

  fun makeNode (box, children as Particle { position, mass, ... }) =
      let in
        Node (1, { position = position, mass = mass },
              Matrix.zero, box, children)
      end
    | makeNode (box, children as Particles ps) =
      let
        val op@ = Vector.+ infix 6 @
        val op* = Vector.scale

        val (mass, weighted_pos) = 
            foldl (fn ({ position, mass, ... } : particle, 
                       (sum_of_mass, weighted_pos)) => 
                      (mass + sum_of_mass, mass * position @ weighted_pos))
                  (0.0, Vector.zero) ps
        val position = (1.0 / mass) * weighted_pos
      in
        Node (length ps, { mass = mass, position = position }, 
              Matrix.zero, box, children)
      end
    | makeNode (box, Empty) = empty box
    | makeNode (box, Octants ts) =
      let
        val op@ = Vector.+ infix 6 @
        val op* = Vector.scale
        val op- = Vector.-
        val x = Vector.outer infix 7 x
        val dot = Vector.dot infix 7 dot

        val (count, mass, weighted_pos) = 
            foldl (fn (Node (count, { position, mass }, _, _, _), 
                       (total, sum_of_mass, weighted_pos)) => 
                      (count + total, mass + sum_of_mass, 
                       mass * position @ weighted_pos))
                  (0, 0.0, Vector.zero) ts
        val position = (1.0 / mass) * weighted_pos

        (* calculate quadrupole for one node based on children ts *)
        val moment = 
            let
              val op+ = Matrix.+
              val op* = Matrix.scale
              val op~ = Matrix.- infix 6 ~

              fun Q_child (Node (_, { position = child_pos, mass }, moment, _, _)) = 
                  let
                    val del_r = child_pos - position
                  in
                    moment + (mass * (3.0 * (del_r x del_r)
                                                ~ del_r dot del_r * Matrix.I))
                  end
            in
              foldl (fn (n, acc) => Q_child n + acc) Matrix.zero ts
            end
      in
        if count = 0 then
          (* Prune empty trees *)
          empty box
        else if count = 1 then
          (* Prune trees with only one particle as well. *)
          let 
            fun findParticle (p, NONE) = SOME p
              | findParticle (_, SOME _) =
                raise Sim "found 2 particles after count = 1"

            val p = case fold' (fn (_, y) => y) findParticle NONE (Octants ts) of
                      NONE => raise Sim "found 0 particles after count = 1"
                    | SOME p => p
          in
            Node (count, { position = position, mass = mass },
                  Matrix.zero, box, Particle p)
          end
        else
          Node (count, { position = position, mass = mass },
                moment, box, Octants ts)
      end

  (* Insert a particle into the given pseudo-tree
       (* pseudo-tree = tree without aggregate data (e.g. moment) *)
       invariant: the particle must lie within the given bounding box *)
  and insertParticle (p, (box, Particles ps)) =
      let in
        (* This is a "small" box, so we don't split *)
        (box, Particles (p :: ps))
      end
    | insertParticle (p, (box, Empty)) =
      let in
        (box, Particle p)
      end
    | insertParticle (p, (box, Particle p')) =
      if Vector.< (#dimension box, epsilon) then
        (* Small box -- don't split *)
        insertParticle (p, (box, Particles [p']))
      else
        (* Split! *)
        let
          (* Make new children *)
          val ts = List.tabulate (8, octant box)
          (* Add the two particles *)
          val (box, children) = insertParticle (p, (box, Octants ts))
          val (box, children) = insertParticle (p', (box, children))
        in
          (box, children)
        end
    | insertParticle (p as { position, ... } : particle,
                      (box, Octants ts)) =
      let
        (* Find the octant that should contain the particle and recur *)
        val (ts', ts) = List.partition (fn (Node (_, _, _, box, _)) => 
                                           contains box position) ts
        val (box', children') =
            case ts' of [Node (_, _, _, box', children')] => (box', children')
                      | nil => raise Sim ("insertParticle found 0 "
                                          ^ "children containing particle "
                                          ^ (particleToString p))
                      | _ => raise Sim ("insertParticle found 2+ "
                                        ^ "children containing particle")
        val t' = makeNode (insertParticle (p, (box', children')))
      in
        (box, Octants (t' :: ts))
      end

  (* Builds the Barnes-Hut tree with the addition of a new particle.  That
   particle need not lie within the bounding box of the given tree *)
  fun addParticle (p as { position, ... } : particle,
                   Node (_, _, _, box, children)) =
      let
        fun add (box, children) =
            if contains box position then 
              (* If the new particle falls within the tree then add it *)
              makeNode (insertParticle (p, (box, children)))
            else 
              (* Expand the tree to cover a larger volume *)
              let
                val op* = Vector.scale
                (* old box *)
                val (x0, y0, z0) = #position box
                (* particle position *)
                val (px, py, pz) = position

                (* Extend the box in the direction of the new point.  NB, this
                  may not include the new point, but if not, we will double
                  again when we recur. Also, remember which octant the old
                  tree will fall in. *)
                val (dx, dy, dz) = #dimension box
                val (x, i) = if px < x0 then (x0 - dx, 1) else (x0, 0)
                val (y, i) = if py < y0 then (y0 - dy, i + 2) else (y0, i)
                val (z, i) = if pz < z0 then (z0 - dz, i + 4) else (z0, i)

                (* Double each dimension *)
                val box' = { position = (x, y, z), dimension = 2.0 * #dimension box }

                (* Make the other seven octants *)
                val ts = List.tabulate (8, 
                                        fn j => if i = j then (* Use the original *)
                                                  makeNode (box, children)
                                                else (* Make a new one *)
                                                  octant box' j)

              in
                (* Then try to add the particle to the expanded tree *)
                add (box', Octants ts)
              end

        val t = add (box, children)
        (* possibly "prune" from the top *)
        val t = case t of Node (count, _, _, _, Octants ts) =>
                          (case List.find (fn (Node (count', _, _, _, _)) => 
                                              count = count') ts of
                             SOME t' => t'
                           | NONE => t)
                        | _ => t
      in
        t
      end

  val terms = ref 0

  fun calculateForce maxSeq (p as { position = part_pos, ... } : particle) t =
    let
      val op- = Vector.-

      (* tests if a cell is well-separated from a particle *)
      fun separatedFrom (Node (count, { position = mass_pos, ... }, _, 
                               { dimension, ... }, _)) =
          if count = 1 then true
          else 
            let
              val dot = Vector.dot infix 7 dot

              val del_r = mass_pos - part_pos
              val d = Math.sqrt (del_r dot del_r)
              val s = max3 dimension
            in
              s < theta * d
            end

      (* calculates force on a particle due to a cell *)
      fun calculateFromNode (Node (count, { mass, position = mass_pos }, moment, _, _)) =
          if count = 0 then Vector.zero
          else
            let 
(*
              val () = terms := !terms + 1
*)
              val del_r = mass_pos - part_pos
              val == = Vector.== infix 4 ==
            in
              if del_r == Vector.zero
              then Vector.zero
              else
                let
                  val scale = Vector.scale infix 7 scale                      
                  val dot = Vector.dot infix 7 dot
                  val mul = Matrix.vec infix 7 mul
                                             
                  val r_squared = del_r dot del_r
                  val abs_r = Math.sqrt r_squared
                  val mrcubed = mass / (r_squared * abs_r)
                  val acc = mrcubed scale del_r 
                in 
                  if count = 1 then acc else
                  let
                    val del_r5inv = 1.0 / (r_squared * r_squared * abs_r)
                    val moment_del_r = moment mul del_r
                    val phi_moment = ~2.5 * (del_r dot moment_del_r) * del_r5inv / r_squared
                  in      
                    (acc - (phi_moment scale del_r)) 
                      - (del_r5inv scale moment_del_r)
                  end
                end
            end

      val op+ = Vector.+
    in
      if separatedFrom t then
        calculateFromNode t
      else
        case t of
          Node (count, _, _, _, Octants ts) =>

          if false (* count > maxSeq * 10 *) then
            A.fold 1 Vector.+
                   (fn i => calculateForce maxSeq p (List.nth (ts, i)))
                   Vector.zero (length ts)
          else

            foldl (fn (child, acc) => calculateForce maxSeq p child + acc)
                  Vector.zero ts
        (* Reached a leaf *)
        | _ => calculateFromNode t
    end

  fun update maxSeq del_t t =
      let
(*
        val () = terms := 0
*)
        fun updateParticle (p as { position, mass, velocity }) =
            let
              val op@ = Vector.+ infix 6 @
              val scale = Vector.scale infix 7 scale
                                             
              val F = calculateForce maxSeq p t
              val a = mass scale F
              val v = velocity @ del_t scale a
            in
              { position = position @ del_t scale v,
                velocity = v, 
                mass = mass }
            end

        fun updateNode (t as Node (_, _, _, box, Empty)) = (t, nil)
          | updateNode (Node (_, _, _, box, Particle p)) =
            let
              val p = updateParticle p
            in
              if contains box (#position p) then
                (makeNode (insertParticle (p, (box, Empty))), nil)
              else
                (empty box, [p])
            end
          | updateNode (Node (_, _, _, box, Particles ps)) =
            let
              val ps = map updateParticle ps
              val (ps, ps') = List.partition (fn ({ position, ... } : particle) => 
                                                 contains box position) ps
              val (box, children) = foldl insertParticle (box, Empty) ps
              val t = makeNode (box, children)
            in
              (t, ps')
            end
          | updateNode (Node (count, _, _, box, Octants ts)) =
            let
              val (ts, ps) = 
                  if count > maxSeq then
                    A.fold 1 (fn ((ts, ps), (ts', ps')) => 
                                      (ts @ ts', ps @ ps'))
                           (fn i => 
                               let val (t, ps) = updateNode (List.nth (ts, i))
                               in ([t], ps) end)
                           (nil, nil)
                           (length ts)
                  else
                    foldl (fn (t, (ts, ps)) =>
                              let val (t, ps') = updateNode t
                              in (t :: ts, ps' @ ps) end)
                          (nil, nil) 
                          ts

              (* Some particles might have moved between our children... split
                those out *)
              val (ps, ps') = List.partition (fn ({ position, ... } : particle) => 
                                                 contains box position) ps

              (* Insert those particles which still fall within our box *)
              val (box, children) = foldl insertParticle (box, Octants ts) ps
              val t = makeNode (box, children)
            in
              (t, ps')
            end

        val (t, ps) = updateNode t
        (* Add the remaining particles back in *)
        val t = foldl addParticle t ps
(*
        val () = print (Int.toString (!terms) ^ " ")

        val () = print "\n"
        val (box, children) = case t of Node (_, _, _, box, children) => (box, children)
        val () = printTree (box, children)
*)
      in
        t
      end
end
