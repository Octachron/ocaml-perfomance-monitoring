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
module Vec_variants = Array_like.As_vec(Vec.Float)(Variants)

module Time_info = Stat.Balanced(Variants)
module Size_info = Time_info
module Gen = Timings.Generator(Variants)(Time_info)

module P = Plot_projectors.Indexed(Variants)

module Min_proj = Timings.Min_projectors(Variants)(Time_info)(P)

let epsilon = Min_proj.epsilon

module Seq_average=Vec_calculus.Stable_average(Vec.Float)

let dispatch f s = Variants.init (fun index -> f (Seq.map  (Types.Input.map (fun x -> x.Variants.%(index))) s ) )

let dispatch' f s = Variants.init (fun index -> f (Seq.map  (fun x -> x.Variants.%(index)) s ) )

let (.%()) = Variants.(.%())
let regroup s =
  let n = Seq.fold_left Stdlib.min Int.max_int  (Seq.map Array.length @@ Variants.to_seq s) in
  Array.init n (fun i -> Variants.init (fun x -> s.%(x).(i))  ) 

let alternatives = { V.initial; call_by_need }
let names = { V.initial = "initial_bugfix"; call_by_need="strong_call_by_need" }

let hist_and_quantiles dir points (proj: _ P.t) =
  let info = proj.info in
  let points = Seq.filter_map (fun x ->  Option.map (fun data -> { x with Types.data }) (proj.f x)) points in
  let ordered_points = dispatch Stat.order_statistic points in
  let h = Variants.map (Stat.histogram 20) ordered_points in
  let name v = Io.out_name ?dir "%s_%s_hist.data" (names.%(v)) info.name in
  Variants.iteri (fun i -> Stat.save_histogram (name i)) h;
  Variants.iteri (fun i ordered_points ->
      Stat.save_quantiles (Io.out_name ?dir "%s_%s_quantiles.data" names.%(i) info.name) ordered_points;
      Stat.save_quantile_table info.name (Io.out_name ?dir "%s_%s_quantile_table.md" names.%(i) info.name) ordered_points
    )
    ordered_points

module Mq = Stat.Multi_quantiles(Variants)

let multi_quantiles dir points (proj: _ P.t) =
  let info = proj.info in
  let points = Seq.filter_map (fun x ->  Option.map (fun data -> { x with Types.data }) (proj.f x)) points in
  let extract (Stat.Ordered x) = x in
  let ordered_points = Variants.map extract @@ dispatch Stat.order_statistic points in
  let ordered_points = Mq.shuffle @@ regroup ordered_points in
  Mq.save_quantiles (Io.out_name ?dir "%s_quantiles.data" info.name) ordered_points;
  Mq.save_quantile_table names
    [0.01; 0.1; 0.25; 0.5; 0.75;0.9;0.99;0.999;0.9999]
    (Io.out_name ?dir "%s_quantile_table.md" info.name) ordered_points


let time_analysis dir log =
   let m = Gen.compare ~ref:reverted ~alternatives log in
  List.iter (fun (P.Any x) -> Plot.cloud dir m (P.gen x)) Min_proj.all;
  let m = Stat.By_files.filter (fun _k ({ty;nonty; _}:Time_info.simplified) ->
      ty.ref.min > epsilon && nonty.ref.min > epsilon &&
      Variants.for_all (fun (nt:Stat.summary) -> nt.min > epsilon ) nonty.main
    ) m
  in
  Time_info.save (Io.out_name ?dir "timing_by_files.data") m;
  let points = Seq.map Types.input @@ Types.By_files.to_seq m in
  let projs = Min_proj.all in
  let () = List.iter
      (multi_quantiles dir points)
      (List.map P.remove_std projs)
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
      List.iter (report_average ppf) projs;
      List.iter (report_geometric_average ppf) projs
    );
  Stat.to_filename (Io.out_name ?dir "pkgs.data")
    (By_pkg_aggregation.global_count (Seq.map Types.input @@ Types.By_files.to_seq m))

module Size_analysis = struct
  module By_files = Types.By_files

  type elt = float Size_info.t
  type t = elt Types.input

  let read ~ref ~alts = By_files.fold (fun key ref_size m ->
      match Variants.map (By_files.find key) alts with
      | exception Not_found -> m
      | all_sizes ->
         let simplify l = List.fold_left min Float.infinity l in
          let data = { Stat.ref = simplify ref_size; main = Variants.map simplify all_sizes } in
          By_files.add key data m
    ) ref By_files.empty


  let absolute =
    P.Any { P.info={kind=No; name="absolute_size"; title="Absolute file size" };
                 f = (fun (x:t) -> Some x.data.main)
    }

  let relative =
    P.Any { P.info={kind=No; name="relative_size"; title="Relative file size" };
      f = (fun (x:t) -> Some Vec_variants.(x.data.main /. x.data.ref) )
    }

  let plot_variants = [ absolute; relative ]

  let save_size_entry fmt key (data:elt) =
      Fmt.pf fmt "@[<h>%a %g %a@]@."
        Stat.pp_full_key key
        data.ref
        (Variants.pp Fmt.float) data.main

  let save filename m = Stat.to_filename filename (fun fmt ->
      Fmt.pf fmt "@[<h>File Filesize:reverted Filesize:minimal_bugfix Filesize:strong_call_by_need@]@.";
      By_files.iter (save_size_entry fmt) m
    )

  let start dir log =
    let ref = Types.Db.find reverted log in
    let alts = Variants.map (fun n -> Types.Db.find n log) alternatives in
    let m = read ~ref ~alts in
    List.iter (fun (P.Any x) -> Plot.cloud dir m (P.gen x)) plot_variants;
    save (Io.out_name ?dir "size_by_files.data") m;
    let points = Seq.map Types.input @@ Types.By_files.to_seq m in
    let () = List.iter
        (multi_quantiles dir points)
        (List.map P.remove_std plot_variants)
    in
    ()

end


let () =
  let open Cli in
  Arg.parse [log;dir] anon "analyzer -output-dir dir -log log1 -log log2 log3";
  let log_seq = read_logs !log_files in
  let log = read_log log_seq in
  time_analysis !data_dir (Types.By_pkg.map (fun x -> x.Stat.R.times) log);
  Size_analysis.start !data_dir  (Types.By_pkg.map (fun x -> x.Stat.R.sizes) log)
