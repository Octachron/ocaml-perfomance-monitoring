open Analyzer

let before = "5.3.0+occur_rec_profiling"
let after = "5.3.0+occur_rec_marking"

module After = Array_like.Expanded(struct
  type 'a t = 'a
  type index = unit
  let dim _ = 1
  let to_seq x = Seq.return x
  let indices = Seq.return ()
  let (.%()) x () = x
  let init f = f ()
end)

module Obs = struct
  module Core = struct
    type index = Total | Ty | Occur | Ty_over_Total | Occur_over_Ty
    type 'a t = { total:'a; ty:'a; occur:'a; ty_total:'a; occ_ty:'a}
    let dim _ = 5
    let (.%()) x = function
      | Total -> x.total
      | Ty -> x.ty
      | Occur -> x.occur
      | Ty_over_Total -> x.ty_total
      | Occur_over_Ty -> x.occ_ty
    let init f = {
      total = f Total;
      ty = f Ty;
      occur = f Occur;
      ty_total = f Ty_over_Total;
      occ_ty = f Occur_over_Ty
    }
    let indices = Array.to_seq [|
        Total; Ty; Occur; Ty_over_Total; Occur_over_Ty
      |]
    let to_seq x = Array.to_seq [|x.total; x.ty;  x.occur; x.ty_total; x.occ_ty|]
  end
  include Core
  include Array_like.Expand(Core)
  let gen =
    let total = List.map Data.total in
    let ty = List.map Data.typing in
    let occur_rec = "occur_rec" in
    let occur = List.map (fun x -> x.Data.?(occur_rec)) in
    let ty_total = List.map (fun x -> Data.typing x /. Data.total x) in
    let occ_ty = List.map (fun x ->  x.Data.?(occur_rec) /. Data.typing x) in
    {total;ty;occur;ty_total;occ_ty}

  let main x = x.occur

end


module Time_info = Stat.Balanced(After)(Obs)
module Gen = Timings.Generator(After)(Obs)(Time_info)

module P = Plot_projectors.Indexed(After)

let epsilon = 1e-6

module A = After
module Projs = struct
module S = Time_info
type t = S.simplified Types.input
let mean proj (x : t) =
  let ty = x.data.Obs.%(proj) in
  let denom = ty.ref.mean.center in
  if denom < epsilon then
    None
  else
    Some (A.map (fun (tym:Stat.summary) -> tym.mean.center /. denom, tym.mean.width /. denom) ty.main)

let ratio_mean proj (x: t) =
  Some (A.map (fun (x:Stat.summary) -> x.mean.center) x.data.Obs.%(proj).main)

let ratio_min proj (x : t) =
  Some (A.map (fun (x:Stat.summary) -> x.min) x.data.Obs.%(proj).main)
let min proj (x: t) =
  let y = x.data.Obs.%(proj) in
  let denom = y.ref.min in
  if denom < epsilon then None
  else  Some (A.map (fun (x:Stat.summary) -> x.min /. denom) y.main)


module I = Plot_projectors.Indexed(After)

let proj kind name title f = I.( Any { info = {kind; name; title}; f } )


let all =
[

  proj Yes "total" "Relative change in total compilation time"
    (mean Total);
  proj Yes "typing" "Relative change in average typechecking time"
    (mean Ty);
  proj Yes "occur" "Relative change in average occur_rec time"
    (mean Occur);
  proj No "typing_total" "Ratio of average typechecking time compared to average total time"
    (ratio_mean Ty_over_Total);
  proj No "occur_typing" "Ratio of average occur time compared to average typing time"
    (ratio_mean Occur_over_Ty);

  proj No "min_total" "Relative change in minimal compilation time"
    (min Total);
  proj No "min_typing" "Relative change in minimal typechecking time"
    (min Ty);
  proj No "occur" "Relative change in average occur_rec time"
    (min Occur);
  proj No "min_typing_total" "Ratio of average typechecking time compared to average total time"
    (ratio_min Ty_over_Total);
  proj No "min_occur_typing" "Ratio of average occur time compared to average typing time"
   (ratio_min Occur_over_Ty);
]
end

let projs = Projs.all


module Seq_average=Vec_calculus.Stable_average(Vec.Float)

let hist_and_quantiles dir points (proj: _ P.t) =
  let info = proj.info in
  let points = Seq.filter_map (fun x ->  Option.map (fun data -> { x with Types.data }) (proj.f x)) points in
  let ordered_points = Stat.order_statistic points in
  let h = Stat.histogram 20 ordered_points in
  let name = Io.out_name ?dir "%s_hist.data" info.name in
  Stat.save_histogram name h;
  Stat.save_quantiles (Io.out_name ?dir "%s_quantiles.data" info.name) ordered_points;
  Stat.save_quantile_table info.name (Io.out_name ?dir "%s_quantile_table.md" info.name) ordered_points


let time_analysis dir log =
  let m = Gen.compare ~ref:before ~alternatives:after log in
  Format.eprintf "%d files@." (Types.By_files.cardinal m);
  List.iter (fun (P.Any x) -> Plot.cloud dir  m (P.gen x)) projs;
  let m = Stat.By_files.filter (fun _k ({ty; _}: Time_info.simplified) ->
      ty.ref.min > epsilon )
      m
  in
  Time_info.save (Io.out_name ?dir "by_files.data") m;
  let points = Seq.map Types.input @@ Types.By_files.to_seq m in
  let () = List.iter
      (hist_and_quantiles dir points)
      (List.map P.remove_std projs)
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
      List.iter (report_average ppf) projs;
      List.iter (report_geometric_average ppf) projs
    );
  Stat.to_filename (Io.out_name ?dir "pkgs.data")
    (By_pkg_aggregation.global_count (Seq.map Types.input @@ Types.By_files.to_seq m))


let () =
  let open Cli in
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  Format.eprintf "%d switches@." (Types.By_pkg.cardinal log);
  time_analysis !data_dir (Types.By_pkg.map (fun x -> x.Stat.R.times) log)
