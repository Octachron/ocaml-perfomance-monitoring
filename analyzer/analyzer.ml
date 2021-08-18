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

let log_files: string list ref = ref []
let data_dir = ref None

let set r = Arg.String (fun x -> r := Some x)
let add_item l = Arg.String (fun x -> l := x :: !l)
let anon x = log_files := x :: !log_files

let log =  "-log" , add_item log_files, "read data from file"
let dir =  "-output-dir", set data_dir, "output dir"

let read_logs logs =
  List.fold_left (fun s x -> Seq.append (Log.read x) s) (fun () -> Seq.Nil) logs

let out_name x = match !data_dir with
  | None -> x
  | Some dir -> Filename.concat dir x


module Projectors = struct
  type named = { name:string; f: Input.t -> float }
  let mean (_,{ty;_} : Input.t) =  ty.main.mean.center /. ty.ref.mean.center
  let other (_, {nonty; _ } : Input.t) =  nonty.main.mean.center /. nonty.ref.mean.center
  let ratio (_, r : Input.t) = r.ratio.main.mean.center
  let min_ratio (_, r : Input.t) = r.ratio.main.min
  let total (_, r : Input.t) = r.total.main.mean.center /. r.total.ref.mean.center
  let min (_,{ty;_} : Input.t) =  ty.main.min /. ty.ref.min
  let min_other (_, {nonty; _ } : Input.t) =  nonty.main.min /. nonty.ref.min
  let min_total (_, {total; _ } : Input.t) =  total.main.min /. total.ref.min
  let all =
    [
      {name="mean"; f=mean} ;
      {name="other"; f=other};
      {name="total"; f=total};
      {name="profile"; f=ratio};
      {name="min"; f=min};
      {name="min_other"; f=min_other};
      {name="min_total"; f= min_total};
      {name="min_profile"; f=min_ratio};
    ]


  let logarithmics =
    let log p x =
      let r = p x in
      if r <= 0. then 0. else Float.log r
    in
    List.map (fun np -> { name = "log_" ^ np.name; f = log np.f }) all

end
open Projectors

let () =
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  let m = comparison ~before ~after log in
  let epsilon = 1e-6 in
  Stat.save (out_name "by_files.data") m;
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
  let hist proj =
    let name = out_name (Fmt.str "%s_hist.data" proj.name) in
    let h = Stat.histogram 20 proj.f (Array.of_seq @@ Stat.M.to_seq m) in
    Stat.save_histogram name proj.f h
  in
  let () = List.iter hist Projectors.all in
  let quantiles np =
    let name = out_name (Fmt.str "%s_quantiles.data" np.name) in
    Stat.save_quantiles name np.f (Array.of_seq @@ Stat.M.to_seq m) in
  List.iter quantiles Projectors.all;
  let report_average np =
    let average = Seq_average.map_and_compute np.f (Stat.M.to_seq m) in
    Fmt.pr "average %s: %g@." np.name average
  in
  let report_geometric_average np =
    let average = Seq_average.map_and_compute np.f (Stat.M.to_seq m) in
    Fmt.pr "geometric average %s: %g@." np.name (exp average)
  in
  List.iter report_average Projectors.all;
  List.iter report_geometric_average Projectors.logarithmics
