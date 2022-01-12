open Analyzer

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

module After = Array_like.Expanded(struct
  type 'a t = 'a
  type index = unit
  let dim _ = 1
  let to_seq x = Seq.return x
  let indices = Seq.return ()
  let (.%()) x () = x
  let init f = f ()
end)


module Analysis = Timings.Make(After)
open Analysis

module P = Projectors.I

let epsilon = Analysis.Projectors.epsilon

module Seq_average=Stat.Stable_average(Vec.Float)(struct type 'a t = 'a Seq.t let fold = Seq.fold_left end)


let hist_and_quantiles dir points (proj: _ Analysis.Projectors.I.t) =
  let info = proj.info in
  let points = Seq.filter_map (fun x ->  Option.map (fun data -> { x with Types.data }) (proj.f x)) points in
  let ordered_points = Stat.order_statistic points in
  let h = Stat.histogram 20 ordered_points in
  let name = Io.out_name ?dir "%s_hist.data" info.name in
  Stat.save_histogram name h;
  Stat.save_quantiles (Io.out_name ?dir "%s_quantiles.data" info.name) ordered_points;
  Stat.save_quantile_table info.name (Io.out_name ?dir "%s_quantile_table.md" info.name) ordered_points


let time_analysis dir log =
   let m = comparison ~ref:before ~alternatives:after log in
  List.iter (fun (P.Any x) -> Plot.cloud dir  m (P.gen x)) Projectors.all;
  let m = Stat.By_files.filter (fun _k {Analysis.S.ty;nonty; _} -> ty.ref.min > epsilon && nonty.ref.min > epsilon && nonty.main.min > epsilon )
      m
  in
  Analysis.S.save (Io.out_name ?dir "by_files.data") m;
  let points = Seq.map Types.input @@ Types.By_files.to_seq m in
  let () = List.iter
      (hist_and_quantiles dir points)
      (List.map P.remove_std Projectors.all)
  in
  let report_average ppf np =
    let np = P.remove_std np in
    let average = Seq_average.compute @@ (Seq.filter_map np.f points) in
    Fmt.pf ppf "average %s: %g@." np.info.name average
  in
  let report_geometric_average ppf np =
    let np = P.ln np  in
    let average = Seq_average.compute @@ (Seq.filter_map np.f points) in
    Fmt.pf ppf "geometric average %s: %g@." np.info.name (exp average)
  in
  Stat.to_filename (Io.out_name ?dir "averages.data") (fun ppf ->
      List.iter (report_average ppf) Projectors.all;
      List.iter (report_geometric_average ppf) Projectors.all
    );
  Stat.to_filename (Io.out_name ?dir "pkgs.data")
    (By_pkg_aggregation.global_count (Seq.map Types.input @@ Types.By_files.to_seq m))


let () =
  let open Cli in
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  time_analysis !data_dir log
