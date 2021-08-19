module Db = Map.Make(String)

let read_log_entry db (x:Data.file Data.typechecking_stat) =
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
  type t = Stat.File_key.t * Stat.simplified
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

let epsilon = 1e-6

module Projectors = struct
  type _ with_std =
    | No: float with_std
    | Yes: (float * float) with_std

  type 'a named = { kind:'a with_std; name:string; f: Input.t -> 'a option }
  type any = Any: 'a named -> any  [@@unboxed]
  let mean (_,{ty;_} : Input.t) =
    let denom = ty.ref.mean.center in
    if denom < epsilon then
      None
    else
      Some (ty.main.mean.center /. denom, ty.main.mean.width /. denom)
  let other (_, {nonty; _ } : Input.t) =
    let denom = nonty.ref.mean.center in
    if denom < epsilon then None
      else
        Some (nonty.main.mean.center /. denom, nonty.main.mean.width /. denom)
  let total (_, r : Input.t) =
    let denom = r.total.ref.mean.center in
    if denom < epsilon then None
      else
      Some (r.total.main.mean.center /. denom, r.total.main.mean.width /. denom)


  let ratio (_, r : Input.t) = Some r.ratio.main.mean.center
  let min_ratio (_, r : Input.t) = Some r.ratio.main.min
  let min (_,{ty;_} : Input.t) =
    let denom = ty.ref.min in
    if denom < epsilon then None
    else  Some (ty.main.min /. denom)

  let min_other (_, {nonty; _ } : Input.t) =
    let denom = nonty.ref.min in
    if denom < epsilon then None
    else Some (nonty.main.min /. denom)
  let min_total (_, {total; _ } : Input.t) =
    let denom = total.ref.min in
    if denom < epsilon then None
    else Some (total.main.min /. denom)
  let all =
    [
      Any {kind=Yes; name="mean"; f=mean} ;
      Any {kind=Yes; name="other"; f=other};
      Any {kind=Yes; name="total"; f=total};
      Any {kind=No; name="profile"; f=ratio};
      Any {kind=No; name="min"; f=min};
      Any {kind=No; name="min_other"; f=min_other};
      Any {kind=No; name="min_total"; f= min_total};
      Any {kind=No; name="min_profile"; f=min_ratio};
    ]


  let remove_std (Any p) =
    let f (type a) (p: a named) x: float option =
      match p.kind, p.f x with
      | No, x -> x
      | Yes, None -> None
      | Yes, Some (r,_) -> Some r
    in
    {kind=No; name = p.name; f = f p}

  let ln anp =
    let ln f x = Option.bind (f x) (fun x -> if x <= 0. then None else Some (Float.log x)) in
    let s = remove_std anp in
    { kind=No; name = "log_" ^ s.name; f = ln s.f }

end
open Projectors

let by_pkg_data seq ppf =
  let by_pkg = Stat.By_pkg.empty in
  let add key _data m =
    let key = key.Data.pkg in
    let value =
      match Stat.By_pkg.find_opt key m with
      | None -> 1
      | Some x -> 1 + x
    in
    Stat.By_pkg.add key value m
  in
  let m = Seq.fold_left (fun m (key,data) -> add key data m) by_pkg seq in
  let pp ppf (key,x,s) = Fmt.pf ppf "%s %d %d@." key x s in
  let rec cumulative s seq () = match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons( (key,x), r ) ->
      let s = x + s in
      Seq.Cons( (key,x,s), cumulative s r )
  in
  Seq.iter (pp ppf) (cumulative 0 @@ Stat.By_pkg.to_seq m)

let kmeans epsilon m =
  Random.self_init ();
  let groups =
    Kmean.compute ~k:10 ~fuel:1_000 ~epsilon
      (Stat.By_files.cardinal m)
      (Stat.By_files.to_seq m)
  in
  let split g =
    Kmean.Group.fold (fun (key,p) m -> Stat.By_files.add key p m) g.Kmean.group Stat.By_files.empty
  in
  let split_and_save i x =
    let name = Fmt.str "group_%d.data" i in
    Fmt.pf Fmt.stderr "center: %a with %d points@." Stat.R3.pp x.Kmean.center (Kmean.Points.cardinal x.points);
    let data = split x in
    Stat.save name data
  in
  Array.sort (fun y x -> compare (Kmean.Points.cardinal x.Kmean.points) (Kmean.Points.cardinal y.Kmean.points) ) groups;
  Array.iteri split_and_save groups



let () =
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  let m = comparison ~before ~after log in
  let m = Stat.By_files.filter (fun _k {Stat.ty;nonty; _} -> ty.ref.min > epsilon && nonty.ref.min > epsilon && nonty.main.min > epsilon )
      m
  in
  Stat.save (out_name "by_files.data") m;
  let points = Stat.By_files.to_seq m in
  let hist_and_quantiles proj =
    let points = Seq.filter_map proj.f points in
    let ordered_points = Stat.order_statistic points in
    let h = Stat.histogram 20 ordered_points in
    let name = out_name (Fmt.str "%s_hist.data" proj.name) in
    Stat.save_histogram name h;
    Stat.save_quantiles (out_name (Fmt.str "%s_quantiles.data" proj.name)) ordered_points;
    Stat.save_quantile_table proj.name (out_name ((Fmt.str "%s_quantile_table.md") proj.name)) ordered_points
  in
  let () = List.iter hist_and_quantiles (List.map remove_std Projectors.all) in
  let report_average ppf np =
    let np = remove_std np in
    let average = Seq_average.compute (Seq.filter_map np.f points) in
    Fmt.pf ppf "average %s: %g@." np.name average
  in
  let report_geometric_average ppf np =
    let np = Projectors.ln np in
    let average = Seq_average.compute (Seq.filter_map np.f points) in
    Fmt.pf ppf "geometric average %s: %g@." np.name (exp average)
  in
  Stat.to_filename (out_name "averages.data") (fun ppf ->
      List.iter (report_average ppf) Projectors.all;
      List.iter (report_geometric_average ppf) Projectors.all
    );
  Stat.to_filename (out_name "pkgs.data") (by_pkg_data (Stat.By_files.to_seq m))
