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

let comparison ~before ~after db =
    let ref_times = Db.find before db in
    let times = Db.find after db in
    Stat.simplify ref_times times

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let () =
  let log_name = "longer_complex.log" in
  let log_seq = Log.read log_name in
  let log = read_log log_seq in
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
  Array.iteri split_and_save groups
