module Db = Map.Make(String)

module Observable_reader(O:Stat.observable with type sample = Data.entry_type) = struct
  let read db (x:Data.entry_type) =
    let switch = match x with
      | Compilation_profile x -> x.origin.switch
      | File_size x -> x.origin.switch
    in
    let switch_stat =
      match Db.find switch db with
      | x -> x
      | exception Not_found -> O.empty
    in
    Db.add switch (O.add x switch_stat) db
end

module Time_reader = Observable_reader(Stat.Ls)

let read_log log_seq =
  Seq.fold_left Time_reader.read Db.empty log_seq


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

let out_name fmt = match !data_dir with
  | None -> Fmt.str fmt
  | Some dir -> Fmt.kstr (fun x -> Filename.concat dir x) fmt

let epsilon = 1e-6

module Projectors = struct
  type _ with_std =
    | No: float with_std
    | Yes: (float * float) with_std

  type 'a named = { kind:'a with_std; name:string; title:string; f: Input.t -> 'a option }
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
      Any {kind=Yes; name="mean"; title="Relative change in average typechecking time"; f=mean} ;
      Any {kind=Yes; name="other"; title="Relative change in average time spent outside of typeching"; f=other};
      Any {kind=Yes; name="total"; title="Relative change in average total compilation time"; f=total};
      Any {kind=No; name="profile"; title="Ratio of average typechecking time compared to average total compilation time"; f=ratio};
      Any {kind=No; name="min"; title="Relative change in minimal typechecking time"; f=min};
      Any {kind=No; name="min_other"; title="Relative change in minimal time spent outside of typeching"; f=min_other};
      Any {kind=No; name="min_total"; title="Relative change in minimal total compilation time"; f= min_total};
      Any {kind=No; name="min_profile"; title="Ratio of minimal typechecking time compared to minimal total compilation time"; f=min_ratio};
    ]


  let remove_std (Any p) =
    let f (type a) (p: a named) x: float option =
      match p.kind, p.f x with
      | No, x -> x
      | Yes, None -> None
      | Yes, Some (r,_) -> Some r
    in
    {kind=No; name = p.name; title =""; f = f p}

  let ln anp =
    let ln f x = Option.bind (f x) (fun x -> if x <= 0. then None else Some (Float.log x)) in
    let s = remove_std anp in
    { kind=No; name = "log_" ^ s.name; title=""; f = ln s.f }

end
open Projectors

let by_pkg_data seq =
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
  let rec cumulative s seq () = match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons( (key,x), r ) ->
      let s = x + s in
      Seq.Cons( (key,x,s), cumulative s r )
  in
  Array.of_seq (cumulative 0 @@ Stat.By_pkg.to_seq m)

let global_by_pkg_data seq ppf =
  let a = by_pkg_data seq in
  let pp ppf (key,x,s) = Fmt.pf ppf "%s %d %d@." key x s in
  Array.iter (pp ppf) a

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


let cloud_plot (type a) (ymin,ymax) pkgs (proj: a named) ppf =
  let tics ppf (pkg,n,pos) =  Fmt.pf ppf "%S %d" pkg (pos - n/2) in
  let comma ppf () = Fmt.pf ppf "," in
  let x2tics = Fmt.array (fun ppf (_pkg,_n,pos) -> Fmt.int ppf pos)  ~sep:comma in
  let xtics = Fmt.seq tics ~sep:comma in
  let large_pkgs = Seq.filter (fun (_,n,_) -> n >= 30) @@ Array.to_seq pkgs in
  let plot ppf (proj: a named) =
    match proj.kind with
    | Yes -> Fmt.pf ppf {|plot "%s_cloud.data" u 0:2:3 w yerrorbars pt 0 title ""|} proj.name
    | No -> Fmt.pf ppf {|plot "%s_cloud.data" u 0:2 w p pt 0 title ""|} proj.name
  in
  let yrange ppf =
    if proj.name ="profile" then
      Fmt.pf ppf {|set yrange [0:1]|}
    else
      Fmt.pf ppf {|set yrange [%g:%g]|} (max 0.5 ymin) (Float.min 2.0 ymax)
  in
  Fmt.pf ppf
{|set title "%s"
set xtics rotate by -45
set output "%s_ratio.svg"
set term svg

set grid x2tics
set x2tics (%a) format "" scale 0

set xtics (%a) format "" scale 0

set ylabel "after/before"
set xlabel "files"

set bars 0
set grid y my
%t

%a|}
proj.title proj.name
x2tics pkgs
xtics large_pkgs
yrange
plot proj

let cloud (type a) m (proj:a named) =
  let points =
    let filter (s,_ as x) = Option.map (fun x -> s, x) (proj.f x)  in
    Seq.filter_map filter (Stat.By_files.to_seq m)
  in
  let range (mn,mx) (_,(x:a)) : float * float = match proj.kind, x with
    | No, x -> (Float.min x mn, Float.max x mx)
    | Yes, (x,_) -> (Float.min x mn, Float.max x mx)
  in
  let range = Seq.fold_left range (1./.0., -1./.0.) points in
  let file_name = out_name "%s_cloud.data" proj.name in
  let pp_entry ppf (key,(x:a)) = match proj.kind, x with
    | No, x -> Fmt.pf ppf "%a %g@," Stat.pp_full_key key x
    | Yes, (mu,sigma) -> Fmt.pf ppf "%a %g %g@," Stat.pp_full_key key mu sigma
  in
  let pp_entries ppf = Seq.iter (pp_entry ppf) points in
  let pp_header ppf = match proj.kind with
    | Yes -> Fmt.pf ppf "File %s_mean %s_std@," proj.name proj.name
    | No -> Fmt.pf ppf "File %s_mean@," proj.name
    in
    Stat.to_filename file_name (fun ppf ->
        Fmt.pf ppf "@[<v>%t%t@]@." pp_header pp_entries
      );
    let pkgs = by_pkg_data points in
    Stat.to_filename (out_name "%s_cloud.plot" proj.name) (cloud_plot range pkgs proj)

let cloud' m (Any proj) = cloud m proj


let with_key f (key,_ as x) =
  Option.map (fun value -> {Stat.Q.key; value}) (f x)

let () =
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  let m = comparison ~before ~after log in
  List.iter (cloud' m) Projectors.all;
  let m = Stat.By_files.filter (fun _k {Stat.ty;nonty; _} -> ty.ref.min > epsilon && nonty.ref.min > epsilon && nonty.main.min > epsilon )
      m
  in

  Stat.save (out_name "by_files.data") m;
  let points = Stat.By_files.to_seq m in
  let hist_and_quantiles proj =
    let points = Seq.filter_map (with_key proj.f) points in
    let ordered_points = Stat.order_statistic points in
    let h = Stat.histogram 20 ordered_points in
    let name = out_name  "%s_hist.data" proj.name in
    Stat.save_histogram name h;
    Stat.save_quantiles (out_name "%s_quantiles.data" proj.name) ordered_points;
    Stat.save_quantile_table proj.name (out_name "%s_quantile_table.md" proj.name) ordered_points
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
  Stat.to_filename (out_name "pkgs.data") (global_by_pkg_data (Stat.By_files.to_seq m))
