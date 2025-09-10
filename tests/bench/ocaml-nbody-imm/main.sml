structure CLA = CommandLineArgs

val num_domains = Concurrency.numberOfProcessors

val n = CLA.parseInt "n" 500
val num_bodies = CLA.parseInt "num_bodies" 1024
val gran = CLA.parseInt "gran" 20

val pi = 3.141592653589793
val solar_mass = 4.0 * pi * pi
val days_per_year = 365.24

type planet =
  { x: real,  y: real,  z: real
  , vx: real, vy: real, vz: real
  , mass: real
  }

fun advance bodies dt =
  let
    fun velocity i =
      let
        val b = Array.sub (bodies, i)
        val (vx, vy, vz) = (#vx b, #vy b, #vz b)
      in
        Util.loop (0, Array.length bodies) (vx, vy, vz) (fn ((vx, vy, vz), j) =>
          let
            val b' = Array.sub (bodies, j)
          in
            if i <> j then
              let
                val dx = #x b - #x b'
                val dy = #y b - #y b'
                val dz = #z b - #z b'
                val dist2 = dx * dx + dy * dy + dz * dz
                val mag = dt / (dist2 * Math.sqrt(dist2))
              in
                ( vx - dx * #mass b' * mag
                , vy - dy * #mass b' * mag
                , vz - dz * #mass b' * mag
                )
              end
            else (vx, vy, vz)
          end)
      end

    val velocities = PureSeq.tabulateG gran velocity num_bodies
  in
    Util.for (0, num_bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
        val (vx, vy, vz) = PureSeq.nth velocities i
      in
        Array.update (bodies, i,
          { x = #x b + dt * vx
          , y = #y b + dt * vy
          , z = #z b + dt * vz
          , vx = vx
          , vy = vy
          , vz = vz
          , mass = #mass b
          })
      end)
  end


fun energy bodies =
  let
  in
    SeqBasis.reduce 1 op+ 0.0 (0, Array.length bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
        val e = ref 0.0
      in
        e := !e + 0.5 * #mass b *
          (#vx b * #vx b + #vy b * #vy b + #vz b * #vz b);

        Util.for (i+1, Array.length bodies) (fn j =>
          let
            val b' = Array.sub (bodies, j)
            val dx = #x b - #x b'
            val dy = #y b - #y b'
            val dz = #z b - #z b'
            val distance = Math.sqrt (dx * dx + dy * dy + dz * dz)
          in
            e := !e - (#mass b * #mass b') / distance
          end);

        !e
      end)
  end

fun offset_momentum bodies =
  let
    val px = ref 0.0
    val py = ref 0.0
    val pz = ref 0.0
    val b0 = Array.sub (bodies, 0)
  in
    Util.for (0, Array.length bodies) (fn i =>
      let
        val b = Array.sub (bodies, i)
      in
        px := !px + #vx b * #mass b;
        py := !py + #vy b * #mass b;
        pz := !pz + #vz b * #mass b
      end);
    Array.update (bodies, 0,
      { x = #x b0
      , y = #y b0
      , z = #z b0
      , vx = ~ (!px) / solar_mass
      , vy = ~ (!py) / solar_mass
      , vz = ~ (!pz) / solar_mass
      , mass = #mass b0
      })
  end

val seed = Random.rand (42, 15210)
fun randFloat _ bound =
  bound * (Random.randReal seed)

(* fun randFloat seed bound =
  bound * (Real.fromInt (Util.hash seed mod 1000000000) / 1000000000.0) *)

val bodies =
  Array.tabulate (num_bodies, fn i =>
    let
      val seed = 7*i
    in
      { x = randFloat seed 10.0
      , y = randFloat (seed+1) 10.0
      , z = randFloat (seed+2) 10.0
      , vx = randFloat (seed+3) 5.0 * days_per_year
      , vy = randFloat (seed+4) 4.0 * days_per_year
      , vz = randFloat (seed+5) 5.0 * days_per_year
      , mass = randFloat (seed+6) 10.0 * solar_mass
      }
    end)

val _ = offset_momentum bodies
val _ = print ("initial energy: " ^ Real.toString (energy bodies) ^ "\n")

val _ = Benchmark.run "running simulation" (fn _ =>
  Util.for (0, n) (fn _ => advance bodies 0.01))

val _ = print ("final energy: " ^ Real.toString (energy bodies) ^ "\n")

