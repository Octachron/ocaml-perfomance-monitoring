module Db = Types.Db

module Generator(Alts:Array_like.t)(Sl:Stat.slice)(S:module type of Stat.Balanced(Alts)(Sl)) = struct
  let compare ~ref ~alternatives db =
    let ref_times = Db.find ref db in
    let times = Alts.map (fun alt -> Db.find alt db) alternatives in
    S.simplify ref_times times
end

module Mean_projectors
    (Alts:Array_like.t)
    (S:module type of Stat.Balanced(Alts)(Stat.SliceTy))
    (I:module type of Plot_projectors.Indexed(Alts)) = struct
  type t = S.simplified Types.input
  module Sl = Stat.SliceTy
  module I = Plot_projectors.Indexed(Alts)
  let epsilon = 1e-6

  let imean proj (x : t) =
    let ty = x.data.Sl.%(proj) in
    let denom = ty.ref.mean.center in
    if denom < epsilon then
      None
    else
      Some (Alts.map (fun (tym:Stat.summary) -> tym.mean.center /. denom, tym.mean.width /. denom) ty.main)

  let simple proj (x: t) =
    Some (Alts.map (fun (x:Stat.summary) -> x.mean.center) x.data.Sl.%(proj).main)

  let proj kind name title f = I.( Any { info = {kind; name; title}; f } )
  let all =
    [
      proj Yes "mean"       "Relative change in average typechecking time" (imean Ty);
      proj Yes "other"      "Relative change in average time spent outside of typeching" (imean Nonty);
      proj Yes "total"      "Relative change in average total compilation time" (imean Total);
      proj No "profile"     "Ratio of average typechecking time compared to average total compilation time" (simple Ratio);
    ]

end

module Min_projectors
    (Alts:Array_like.t)
    (S:module type of Stat.Balanced(Alts)(Stat.SliceTy))
    (I:module type of Plot_projectors.Indexed(Alts)) =
struct
  type t = S.simplified Types.input
  module Sl = Stat.SliceTy
  let epsilon = 1e-6
  let ratio proj (x : t) =  Some (Alts.map (fun (x:Stat.summary) -> x.min)  x.data.Sl.%(proj).main)
  let min proj (x: t) =
    let y = x.data.Sl.%(proj) in
    let denom = y.ref.min in
    if denom < epsilon then None
    else  Some (Alts.map (fun (x:Stat.summary) -> x.min /. denom) y.main)

  let proj name title f = I.( Any { info = {kind=No; name; title}; f } )
  let all =
    [
      proj "min"         "Relative change in minimal typechecking time" (min Ty);
      proj "min_other"   "Relative change in minimal time spent outside of typeching" (min Nonty);
      proj "min_total"   "Relative change in minimal total compilation time" (min Total);
      proj "min_profile" "Ratio of minimal typechecking time compared to minimal total compilation time" (ratio Ratio);
    ]
end



module Kmeans
    (Alts:Array_like.t)
    (S:module type of Stat.Balanced(Alts)(Stat.SliceTy)) =
struct
  module Embedding0 = Array_like.Triple(Alts)(Alts)(Alts)
  module Embedding = struct include Embedding0 include Array_like.Expand(Embedding0) end

  module Inner = struct
    type t = S.simplified Types.input
    let compare (x:t) (y:t) = compare x y
    let proj ({ data = {Stat.SliceTy.ty;nonty;_}; _ } : t ) =
      Embedding.cat
        (Alts.map (fun (tyx:Stat.summary) -> tyx.mean.center /. ty.ref.mean.center) ty.main)
        (Alts.map (fun (tyx:Stat.summary) -> tyx.mean.width /. ty.ref.mean.center) ty.main)
        (Alts.map (fun (nontyx:Stat.summary) -> nontyx.mean.width /. nonty.ref.mean.center) nonty.main)
  end

  module C = Vec_calculus.Convex(Array_like.As_vec(Vec.Float)(Embedding))
  module Kmean = Stat.Kmeans(C)(Inner)
  let kmeans epsilon m =
    Random.self_init ();
    let groups =
      Kmean.compute ~k:10 ~fuel:1_000 ~epsilon
        (Stat.By_files.cardinal m)
        (Seq.map (fun (key,data)-> { Types.key;data}) @@ Stat.By_files.to_seq m)
    in
    let split g =
      Kmean.Group.fold (fun {key;data} m -> Stat.By_files.add key data m) g.Kmean.group Stat.By_files.empty
    in
    let split_and_save i x =
      let name = Fmt.str "group_%d.data" i in
      Fmt.pf Fmt.stderr "center: %a with %d points@." C.pp x.Kmean.center (Kmean.Points.cardinal x.points);
      let data = split x in
      S.save name data
    in
    Array.sort (fun y x -> Stdlib.compare (Kmean.Points.cardinal x.Kmean.points) (Kmean.Points.cardinal y.Kmean.points) ) groups;
    Array.iteri split_and_save groups


end


