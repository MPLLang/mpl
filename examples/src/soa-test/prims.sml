(* direct access to sequence literals, which the basis library otherwise only
 * uses with zero elements *)
structure SoaLit =
struct
  val vec3: (int * (real * bool)) * (int * (real * bool)) * (int * (real * bool))
            -> (int * (real * bool)) MPL.VectorSoa.vector =
    _prim "Vector_vectorSoa": (int * (real * bool)) * (int * (real * bool)) * (int * (real * bool)) -> (int * (real * bool)) MPL.VectorSoa.vector;
  val vec3aos: (int * (real * bool)) * (int * (real * bool)) * (int * (real * bool))
            -> (int * (real * bool)) MPL.VectorAos.vector =
    _prim "Vector_vectorAos": (int * (real * bool)) * (int * (real * bool)) * (int * (real * bool)) -> (int * (real * bool)) MPL.VectorAos.vector;
end
