module Db = Types.Db

module Make(Alts:Array_like.t) = struct
  module S = Stat.Balanced(Alts)
  module Embedding0 = Array_like.Triple(Alts)(Alts)(Alts)
  module Embedding = struct include Embedding0 include Array_like.Expand(Embedding0) end

  module Inner = struct
    type t = S.simplified Types.input
    let compare (x:t) (y:t) = compare x y
    let proj ({ data = {S.ty;nonty;_}; _ } : t ) =
      Embedding.cat
        (Alts.map (fun (tyx:Stat.summary) -> tyx.mean.center /. ty.ref.mean.center) ty.main)
        (Alts.map (fun (tyx:Stat.summary) -> tyx.mean.width /. ty.ref.mean.center) ty.main)
        (Alts.map (fun (nontyx:Stat.summary) -> nontyx.mean.width /. nonty.ref.mean.center) nonty.main)
  end
  include Inner
  let comparison ~ref ~alternatives db =
    let ref_times = Db.find ref db in
    let times = Alts.map (fun alt -> Db.find alt db) alternatives in
    S.simplify ref_times times

  module C = Stat.Convex_from_vec(Array_like.As_vec(Stat.Float_as_vec)(Embedding))
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

  module Projectors = struct
    open Plot_projectors
    let epsilon = 1e-6

    let mean (x : t) =
      let ty = x.data.ty in
      let denom = ty.ref.mean.center in
      if denom < epsilon then
        None
      else
        Some (Seq.map (fun (tym:Stat.summary) -> tym.mean.center /. denom, tym.mean.width /. denom) (Alts.to_seq ty.main))

    let other (x : t) =
      let nonty = x.data.nonty in
      let denom = nonty.ref.mean.center in
      if denom < epsilon then None
      else
        Some (Seq.map (fun (n:Stat.summary) -> n.mean.center /. denom, n.mean.width /. denom) @@ Alts.to_seq nonty.main)

    let total (x:t) =
      let r = x.data in
      let denom = r.total.ref.mean.center in
      if denom < epsilon then None
      else
        Some (Seq.map (fun (t:Stat.summary)->
            t.mean.center /. denom, t.mean.width /. denom
          ) @@ Alts.to_seq  r.total.main
          )


    let ratio (x: t) = Some (Seq.map (fun (x:Stat.summary) -> x.mean.center) @@ Alts.to_seq x.data.ratio.main)
    let min_ratio (x : t) =  Some (Seq.map (fun (x:Stat.summary) -> x.min) @@ Alts.to_seq  x.data.ratio.main)
    let min (x: t) =
      let ty = x.data.ty in
      let denom = ty.ref.min in
      if denom < epsilon then None
      else  Some (Seq.map (fun (ty:Stat.summary) -> ty.min /. denom) @@ Alts.to_seq  ty.main)

    let min_other (x:t) =
      let nonty = x.data.nonty in
      let denom = nonty.ref.min in
      if denom < epsilon then None
      else Some (Seq.map (fun (nt:Stat.summary) -> nt.min /. denom) @@ Alts.to_seq  nonty.main)

    let min_total (x:t) =
      let total = x.data.total in
      let denom = total.ref.min in
      if denom < epsilon then None
      else Some (Seq.map (fun (t:Stat.summary) -> t.min /. denom)  @@ Alts.to_seq total.main)
    let proj kind name title f = Any { info = {kind; name; title}; f }
    let all =
      [
        proj Yes "mean"       "Relative change in average typechecking time" mean;
        proj Yes "other"      "Relative change in average time spent outside of typeching" other;
        proj Yes "total"      "Relative change in average total compilation time" total;
        proj No "profile"     "Ratio of average typechecking time compared to average total compilation time" ratio;
        proj No "min"         "Relative change in minimal typechecking time" min;
        proj No "min_other"   "Relative change in minimal time spent outside of typeching" min_other;
        proj No "min_total"   "Relative change in minimal total compilation time" min_total;
        proj No "min_profile" "Ratio of minimal typechecking time compared to minimal total compilation time" min_ratio;
      ]
  end



end


