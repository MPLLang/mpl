structure CLA = CommandLineArgs

val _ = print ("hello\n")

val num_domains = Concurrency.numberOfProcessors

val n = CLA.parseInt "n" 500
val num_bodies = CLA.parseInt "num_bodies" 1024

val pi = 3.141592653589793
val solar_mass = 4.0 * pi * pi
val days_per_year = 365.24

type planet =
  { x: real ref,  y: real ref,  z: real ref
  , vx: real ref, vy: real ref, vz: real ref
  , mass: real
  }

(** manually packed. size = 7 * num bodies *)
type bodies = real array
fun get_x bodies i = Array.sub (bodies, 7*i)
fun get_y bodies i = Array.sub (bodies, 7*i + 1)
fun get_z bodies i = Array.sub (bodies, 7*i + 2)
fun get_vx bodies i = Array.sub (bodies, 7*i + 3)
fun get_vy bodies i = Array.sub (bodies, 7*i + 4)
fun get_vz bodies i = Array.sub (bodies, 7*i + 5)
fun get_mass bodies i = Array.sub (bodies, 7*i + 6)
fun set_x bodies i x = Array.update (bodies, 7*i, x)
fun set_y bodies i x = Array.update (bodies, 7*i + 1, x)
fun set_z bodies i x = Array.update (bodies, 7*i + 2, x)
fun set_vx bodies i x = Array.update (bodies, 7*i + 3, x)
fun set_vy bodies i x = Array.update (bodies, 7*i + 4, x)
fun set_vz bodies i x = Array.update (bodies, 7*i + 5, x)
fun set_mass bodies i x = Array.update (bodies, 7*i + 6, x)


fun advance bodies dt =
  let
  in
    ForkJoin.parfor 1 (0, num_bodies) (fn i =>
      let
        val (vx, vy, vz) = (get_vx bodies i, get_vy bodies i, get_vz bodies i)
        val (vx, vy, vz) =
          Util.loop (0, num_bodies) (vx, vy, vz) (fn ((vx, vy, vz), j) =>
            let
              (* val b' = Array.sub (bodies, j) *)
            in
              if i <> j then
                let
                  val dx = get_x bodies i - get_x bodies j
                  val dy = get_y bodies i - get_y bodies j
                  val dz = get_z bodies i - get_z bodies j
                  val dist2 = dx * dx + dy * dy + dz * dz
                  val mag = dt / (dist2 * Math.sqrt(dist2))
                in
                  ( vx - dx * get_mass bodies j * mag
                  , vy - dy * get_mass bodies j * mag
                  , vz - dz * get_mass bodies j * mag
                  )
                end
              else (vx, vy, vz)
            end);
      in
        set_vx bodies i vx;
        set_vy bodies i vy;
        set_vz bodies i vz
      end);

    Util.for (0, num_bodies) (fn i =>
      let
        (* val b = Array.sub (bodies, i) *)
      in
        set_x bodies i (get_x bodies i + dt * get_vx bodies i);
        set_y bodies i (get_y bodies i + dt * get_vy bodies i);
        set_z bodies i (get_z bodies i + dt * get_vz bodies i)
      end)
  end

fun energy bodies =
  let
  in
    SeqBasis.reduce 1 op+ 0.0 (0, num_bodies) (fn i =>
      let
        (* val b = Array.sub (bodies, i) *)
        val e = ref 0.0
      in
        e := !e + 0.5 * get_mass bodies i *
          (get_vx bodies i * get_vx bodies i +
           get_vy bodies i * get_vy bodies i +
           get_vz bodies i * get_vz bodies i);

        Util.for (i+1, num_bodies) (fn j =>
          let
            (* val b' = Array.sub (bodies, j) *)
            val dx = get_x bodies i - get_x bodies j
            val dy = get_y bodies i - get_y bodies j
            val dz = get_z bodies i - get_z bodies j
            val distance = Math.sqrt (dx * dx + dy * dy + dz * dz)
          in
            e := !e - (get_mass bodies i * get_mass bodies j) / distance
          end);

        !e
      end)
  end

fun offset_momentum bodies =
  let
    val px = ref 0.0
    val py = ref 0.0
    val pz = ref 0.0
  in
    Util.for (0, num_bodies) (fn i =>
      let
        (* val b = Array.sub (bodies, i) *)
      in
        px := !px + get_vx bodies i * get_mass bodies i;
        py := !py + get_vy bodies i * get_mass bodies i;
        pz := !pz + get_vz bodies i * get_mass bodies i
      end);
    set_vx bodies 0 (~ (!px) / solar_mass);
    set_vy bodies 0 (~ (!py) / solar_mass);
    set_vz bodies 0 (~ (!pz) / solar_mass)
  end

val seed = Random.rand (42, 15210)
fun randFloat bound =
  bound * (Random.randReal seed)

val _ = print ("initializing bodies...\n")

val bodies = Array.array (num_bodies * 7, 0.0);
val _ =
  Util.for (0, num_bodies) (fn i =>
    ( set_x bodies i (randFloat 10.0)
    ; set_y bodies i (randFloat 10.0)
    ; set_z bodies i (randFloat 10.0)
    ; set_vx bodies i (randFloat 5.0 * days_per_year)
    ; set_vy bodies i (randFloat 4.0 * days_per_year)
    ; set_vz bodies i (randFloat 5.0 * days_per_year)
    ; set_mass bodies i (randFloat 10.0 * solar_mass)
    )
  )

val _ = print ("offset momentum...\n");
val _ = offset_momentum bodies

val _ = print ("calculating initial energy...\n");
val _ = print ("initial energy: " ^ Real.toString (energy bodies) ^ "\n")

val _ = Benchmark.run "running simulation" (fn _ =>
  Util.for (0, n) (fn _ => advance bodies 0.01))

val _ = print ("final energy: " ^ Real.toString (energy bodies) ^ "\n")

