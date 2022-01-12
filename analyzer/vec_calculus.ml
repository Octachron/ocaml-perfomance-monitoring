
module Stable_average(V:Vec.t) = struct
  let map_and_compute f c =
    let _, s = Seq.fold_left (fun (n,mn) x ->
        let n = n + 1 in
        n, V.( mn + (f x - mn) /. float n )
      ) (0, V.zero) c
    in
    s

  let compute c =
    let _, s = Seq.fold_left (fun (n,mn) x ->
        let n = n + 1 in
        n, V.( mn + (x - mn) /. float n )
      ) (0, V.zero) c
    in
    s
end


module Convex(V:Vec.t) = struct
  include V
  let distance_2 x y =
    let d = V.(x - y) in
    V.(d|*|d)
  module Stable = Stable_average(V)
  let isobarycenter = Stable.compute
end
