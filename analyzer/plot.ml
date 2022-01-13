open Plot_projectors

let cloud_plot (type a) (ymin,ymax) pkgs (proj: a info) ppf =
  let tics ppf (pkg,n,pos) =  Fmt.pf ppf "%S %d" pkg (pos - n/2) in
  let comma ppf () = Fmt.pf ppf "," in
  let x2tics = Fmt.array (fun ppf (_pkg,_n,pos) -> Fmt.int ppf pos)  ~sep:comma in
  let xtics = Fmt.seq tics ~sep:comma in
  let large_pkgs = Seq.filter (fun (_,n,_) -> n >= 30) @@ Array.to_seq pkgs in
  let plot ppf (proj: a info) =
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


let cloud dir (type a i) m (proj:(a,i Types.input) gen) =
  let points =
    let filter x = Option.map (fun data -> { x with Types.data}) (proj.f x)  in
    Seq.filter_map filter (Seq.map Types.input @@ Stat.By_files.to_seq m)
  in
  let range (mn,mx) (x:a) : float * float = match proj.info.kind, x with
    | No, x -> (Float.min x mn, Float.max x mx)
    | Yes, (x,_) -> (Float.min x mn, Float.max x mx)
  in
  let prange (mn,mx) s = Seq.fold_left range (mn,mx) s.Types.data in
  let range = Seq.fold_left prange (1./.0., -1./.0.) points in
  let file_name = Io.out_name ?dir "%s_cloud.data" proj.info.name in
  let pp_seq pp ppf x = Fmt.(seq ~sep:Fmt.sp pp) ppf x in
  let pp_entry ppf {Types.key; data=(x:a Seq.t)} = match proj.info.kind, x with
    | No, x -> Fmt.pf ppf "@[<h>%a %a@]@," Stat.pp_full_key key (pp_seq Fmt.float) x
    | Yes, x -> Fmt.pf ppf "@[<h>%a %a@]@," Stat.pp_full_key key (pp_seq Fmt.(pair float float)) x
  in
  let pp_entries ppf = Seq.iter (pp_entry ppf) points in
  let info = proj.info in
  let pp_header ppf = match proj.info.kind with
    | Yes -> Fmt.pf ppf "File %s_mean %s_std@," info.name info.name
    | No -> Fmt.pf ppf "File %s@," info.name
    in
    Stat.to_filename file_name (fun ppf ->
        Fmt.pf ppf "@[<v>%t%t@]@." pp_header pp_entries
      );
    let pkgs = By_pkg_aggregation.count points in
    Stat.to_filename (Io.out_name ?dir "%s_cloud.plot" info.name) (cloud_plot range pkgs info)

let cloud' dir m (Any proj) = cloud dir m proj
