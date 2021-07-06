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
  type t = Stat.Key.t * Stat.Pairs.t
  let compare (x:t) (y:t) = compare x y
  let proj (_, (ty,nonty) : t ) =
    {
      Stat.R3.x = log (ty.main.center /. ty.ref.center);
      y = ty.main.width /. ty.ref.center;
      z = nonty.main.width /. nonty.ref.center
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

let reject_outliers proj q abs l =
  let l = List.sort (fun x y -> compare (proj x) (proj y)) l in
  let n = List.length l in
  let excl = max abs @@ int_of_float (float n *. q) in
  let _ ,r = split excl l in
  let l, _ = split (n- 2 * excl) r in
  l

let () =
  let log_name = "longer_complex.log" in
  let log_seq = Log.read log_name in
  let log = read_log log_seq in
  let pre l = reject_outliers (fun (x:Data.times) -> x.typechecking) 0.1 1 l in
  let log = Db.map (Stat.M.map pre) log in
  let m = comparison ~before ~after log in
  let epsilon = 1e-6 in
  Stat.save  "by_files.data" m;
  let m = Stat.M.filter (fun _k ((ty,nonty): Stat.Pairs.t) ->
      ty.ref.center > epsilon && nonty.ref.center > epsilon
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

  let h = Stat.histogram 100 Input.proj (Array.of_seq @@ Stat.M.to_seq m) in
  let proj (_,(ty,_) : Input.t) =  ty.main.center /. ty.ref.center in
  let antiproj (_,(_, nonty) : Input.t) =  nonty.main.center /. nonty.ref.center in
  Stat.save_histogram "hist.data" proj h;
  Stat.save_quantiles "quantiles.data" proj (Array.of_seq @@ Stat.M.to_seq m);
  Stat.save_histogram "antihist.data" antiproj h;
  Stat.save_quantiles "antiquantiles.data" antiproj (Array.of_seq @@ Stat.M.to_seq m);
  let average = Seq_average.map_and_compute proj (Stat.M.to_seq m) in
  let anti_average = Seq_average.map_and_compute antiproj (Stat.M.to_seq m) in
  let log_average = exp @@ Seq_average.map_and_compute (fun x -> Float.log @@ proj x) (Stat.M.to_seq m) in
  let log_anti_average = exp @@ Seq_average.map_and_compute
      (fun x -> if antiproj x <= 0. then 0. else Float.log @@ antiproj x) (Stat.M.to_seq m) in
  Fmt.pr
    "@[<v>average: %g@ \
     non typechecking average: %g@ \
     exp (E (log Ty)): %g@ \
     exp (E (log Nonty)): %g
@]@."
    average
    anti_average
    log_average
    log_anti_average
