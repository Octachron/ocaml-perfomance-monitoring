module Db = Map.Make(String)

let read_log_entry db (x:Data.typechecking_stat) =
  let switch_stat =
    match Db.find x.switch db with
    | x -> x
    | exception Not_found -> Stat.Ls.empty
  in
  Db.add x.switch (Stat.Ls.add x switch_stat) db

let read_log log_seq =
  Seq.fold_left read_log_entry Db.empty log_seq


module C = Stat.Convex_from_vec(Stat.R3)
module Input = struct
  type t = Stat.Key.t * Stat.simplified
  let compare (x:t) (y:t) = compare x y
  let proj (_, {Stat.ty;nonty;_} : t ) =
    {
      Stat.R3.x = log (ty.main.mean.center /. ty.ref.mean.center);
      y = ty.main.mean.width /. ty.ref.mean.center;
      z = nonty.main.mean.width /. nonty.ref.mean.center
    }
end
module Kmean = Stat.Kmeans(C)(Input)
module Seq_average=Stat.Stable_average(Stat.Float_as_vec)(struct type 'a t = 'a Seq.t let fold = Seq.fold_left end)

let comparison ~before ~after db =
    let ref_times = Db.find before db in
    let times = Db.find after db in
    Stat.simplify ref_times times

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"


let split n l =
  let rec split left n right =
  if n = 0 then left, right else
    match right with
    | [] -> left, right
    | a :: right -> split (a::left) (n-1) right
  in
  split [] n l

let _reject_outliers proj excl l =
  let l = List.sort (fun x y -> compare (proj y) (proj x)) l in
  let _ ,r = split excl l in
  r

let () =
  let log_name = "longer_complex.log" in
  let log_seq = Log.read log_name in
  let log = read_log log_seq in
  let m = comparison ~before ~after log in
  let epsilon = 1e-6 in
  Stat.save  "by_files.data" m;
  let m = Stat.M.filter (fun _k {Stat.ty;nonty;_} ->
      ty.ref.min > epsilon && nonty.ref.min > epsilon
    )
      m
  in
  Random.self_init ();
  let groups =
    Kmean.compute ~k:20 ~fuel:1_000 ~epsilon
      (Stat.M.cardinal m)
      (Stat.M.to_seq m)
  in
  let split g =
    Kmean.Group.fold (fun (key,p) m -> Stat.M.add key p m) g.Kmean.group Stat.M.empty
  in
  let split_and_save i x =
    let name = Fmt.str "group_%d.data" i in
    Fmt.pf Fmt.stderr "center: %a with %d points@." Stat.R3.pp x.Kmean.center (Kmean.Points.cardinal x.points);
    let data = split x in
    Stat.save name data
  in
  Array.sort (fun y x -> compare (Kmean.Points.cardinal x.Kmean.points) (Kmean.Points.cardinal y.Kmean.points) ) groups;
  Array.iteri split_and_save groups;
  let proj (_,{ty;_} : Input.t) =  ty.main.mean.center /. ty.ref.mean.center in
  let other_proj (_, {nonty; _ } : Input.t) =  nonty.main.mean.center /. nonty.ref.mean.center in
  let ratio_proj (_, r : Input.t) = r.ratio.main.mean.center in
  let min_ratio_proj (_, r : Input.t) = r.ratio.main.min in
  let total_proj (_, r : Input.t) = r.total.main.mean.center /. r.total.ref.mean.center  in
  let min_proj (_,{ty;_} : Input.t) =  ty.main.min /. ty.ref.min in
  let min_other_proj (_, {nonty; _ } : Input.t) =  nonty.main.min /. nonty.ref.min in
  let min_total_proj (_, {total; _ } : Input.t) =  total.main.min /. total.ref.min in
  let hist name proj =
    let h = Stat.histogram 20 proj (Array.of_seq @@ Stat.M.to_seq m) in
    Stat.save_histogram name proj h
  in
  let () =
    hist "hist.data" proj;
    hist "other_hist.data" other_proj;
    hist "total_hist.data" total_proj;
    hist "ratio_hist.data" ratio_proj;
    hist "min_hist.data" min_proj;
    hist "min_other_hist.data" min_other_proj;
    hist "min_total_hist.data" min_total_proj
  in
  Stat.save_quantiles "quantiles.data" proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "other_quantiles.data" other_proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "total_quantiles.data" total_proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "min_quantiles.data" min_proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "min_other_quantiles.data" min_other_proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "min_total_quantiles.data" min_total_proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_quantiles "min_ratio_quantiles.data" min_ratio_proj (Array.of_seq @@ Stat.M.to_seq m);
  let average = Seq_average.map_and_compute proj (Stat.M.to_seq m) in
  let other_average = Seq_average.map_and_compute other_proj (Stat.M.to_seq m) in
  let total_average = Seq_average.map_and_compute total_proj (Stat.M.to_seq m) in
  let min_average = Seq_average.map_and_compute min_proj (Stat.M.to_seq m) in
  let min_other_average = Seq_average.map_and_compute min_other_proj (Stat.M.to_seq m) in
  let min_total_average = Seq_average.map_and_compute min_total_proj (Stat.M.to_seq m) in
  let log_average = exp @@ Seq_average.map_and_compute (fun x -> Float.log @@ proj x) (Stat.M.to_seq m) in
  let log_other_average = exp @@ Seq_average.map_and_compute
      (fun x -> if other_proj x <= 0. then 0. else Float.log @@ other_proj x) (Stat.M.to_seq m) in
  Fmt.pr
    "@[<v>average: %g@ \
     non typechecking average: %g@ \
     total average: %g@ \
     exp (E (log Ty)): %g@ \
     exp (E (log Nonty)): %g@ \
     Min average: %g@ \
     Min total average: %g@ \
     Min average (non-typechecking): %g\
@]@."
    average
    other_average
    total_average
    log_average
    log_other_average
    min_average
    min_total_average
    min_other_average
