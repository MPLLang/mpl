structure MLton =
   struct
      open MLton

      structure Platform =
         struct
            structure Arch =
               struct
                  type t = string
                  val host = "amd64"
                  val toString = fn s => s
               end
            structure OS =
               struct
                  type t = string
                  val host = "linux"
                  val toString = fn s => s
               end
         end
   end
