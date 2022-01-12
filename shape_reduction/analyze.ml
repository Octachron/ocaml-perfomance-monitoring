open Alts
open Analyzer

module V = struct
  type 'a t = { initial:'a; call_by_need:'a }
  type index = Initial | Call_by_need
  let dim _ = 2
  let to_seq x = List.to_seq [x.initial;x.call_by_need]
  let indices = List.to_seq [Initial; Call_by_need]
  let (.%()) x = function
    | Initial -> x.initial
    | Call_by_need -> x.call_by_need
  let init f = { initial = f Initial; call_by_need = f Call_by_need }
end

module Variants = Array_like.Expanded(V)


module Analysis = Timings.Make(Variants)
open Analysis

module P = Projectors.I

let epsilon = Analysis.Projectors.epsilon

module Seq_average=Vec_calculus.Stable_average(Vec.Float)

let dispatch f s = Variants.init (fun index -> f (Seq.map  (Types.Input.map (fun x -> x.Variants.%(index))) s ) )

let dispatch' f s = Variants.init (fun index -> f (Seq.map  (fun x -> x.Variants.%(index)) s ) )


let (.%()) = Variants.(.%())
let alternatives = { V.initial; call_by_need }


let hist_and_quantiles dir points (proj: _ Analysis.Projectors.I.t) =
  let info = proj.info in
  let points = Seq.filter_map (fun x ->  Option.map (fun data -> { x with Types.data }) (proj.f x)) points in
  let ordered_points = dispatch Stat.order_statistic points in
  let h = Variants.map (Stat.histogram 20) ordered_points in
  let name v = Io.out_name ?dir "%s_%s_hist.data" (alternatives.%(v)) info.name in
  Variants.iteri (fun i -> Stat.save_histogram (name i)) h;
  Variants.iteri (fun i ordered_points ->
      Stat.save_quantiles (Io.out_name ?dir "%s_%s_quantiles.data" alternatives.%(i) info.name) ordered_points;
      Stat.save_quantile_table info.name (Io.out_name ?dir "%s_%s_quantile_table.md" alternatives.%(i) info.name) ordered_points
    )
    ordered_points


let time_analysis dir log =
   let m = comparison ~ref:reverted ~alternatives log in
  List.iter (fun (P.Any x) -> Plot.cloud dir  m (P.gen x)) Projectors.all;
  let m = Stat.By_files.filter (fun _k {Analysis.S.ty;nonty; _} ->
      ty.ref.min > epsilon && nonty.ref.min > epsilon &&
      Variants.for_all (fun (nt:Stat.summary) -> nt.min > epsilon ) nonty.main
    ) m
  in
  Analysis.S.save (Io.out_name ?dir "by_files.data") m;
  let points = Seq.map Types.input @@ Types.By_files.to_seq m in
  let () = List.iter
      (hist_and_quantiles dir points)
      (List.map P.remove_std Projectors.all)
  in
  let report_average ppf np =
    let np = P.remove_std np in
    let average = dispatch' Seq_average.compute @@ Seq.filter_map np.f points in
    Fmt.pf ppf "average %s: %a@." np.info.name Variants.(pp Fmt.float) average
  in
  let report_geometric_average ppf np =
    let np = P.ln np  in
    let average = dispatch' Seq_average.compute @@ (Seq.filter_map np.f points) in
    Fmt.pf ppf "geometric average %s: %a@." np.info.name (Variants.pp Fmt.float) (Variants.map exp average)
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
