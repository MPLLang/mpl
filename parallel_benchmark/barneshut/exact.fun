functor Exact (A: SIMARG) : SIM =
struct

  open A

  type t = particle list

  fun empty _ = nil

  val addParticle = op::

  fun update _ del_t t =
      let
        val == = Vector.== infix 4 ==
        val op+ = Vector.+
        val op- = Vector.-
        val dot = Vector.dot infix 7 dot
        val scale = Vector.scale infix 7 scale

        fun inner pos ({ position = pos', mass, ...} : particle)=
          let
            val del_r = pos' - pos
          in
            if del_r == Vector.zero 
            then Vector.zero
            else
              let
                val r_squared = del_r dot del_r
                val abs_r = Math.sqrt r_squared
                val m_over_r_cubed = mass / (r_squared * abs_r)
                val F_i = m_over_r_cubed scale del_r
              in
                F_i
              end
          end
            
        fun outer (p as { position, mass, velocity }) =
            let
              val op+ = Vector.+
              val op* = Vector.scale

              val F = foldl (fn (p, F) => inner position p + F) Vector.zero t
              val a = mass * F
              val v = velocity + del_t * a
            in
              { position = position + del_t * v,
                velocity = v, 
                mass = mass }
            end
      in
        (* PERF could be in parallel *)
        map outer t
      end

  fun fold fb = foldl

end
