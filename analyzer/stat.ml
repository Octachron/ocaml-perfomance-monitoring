module type observable = sig
  type sample
  type t
  val empty: t
  val singleton: sample -> t
  val add: sample -> t -> t
end

module By_files = Types.By_files
module File_key = Types.File_key

module Time_stat(O:observable with type sample = Data.times) = struct
  type sample = Data.entry_type
  type t = O.t By_files.t
  let empty = By_files.empty
  let add x m =
    match x with
    | Data.File_size _ -> m
    | Data.Compilation_profile {origin; key; value=observation} ->
      let key = {Data.pkg=origin.pkg; name=key} in
      match By_files.find key m with
      | exception Not_found -> By_files.add key (O.singleton observation) m
      | observable -> By_files.add key O.(add observation observable) m

  let add_list ls o = List.fold_left (fun acc x -> add x acc) o ls
  let singleton x = add x empty

end

module Filesize_stat(O:observable with type sample = float) = struct
  type sample = Data.entry_type
  type t = O.t By_files.t
  let empty = By_files.empty
  let add x  m =
    match x with
    | Data.Compilation_profile _
    | Data.File_size { value={kind=Cmi|Cmo|Cmx;_}; _ }
      -> m
    | Data.File_size {origin; key; value={kind=Cmt|Cmti; size}} ->
      let observation = float_of_int size in
      let key = {Data.pkg=origin.pkg; name=key} in
      match By_files.find key m with
      | exception Not_found -> By_files.add key (O.singleton observation) m
      | observable -> By_files.add key O.(add observation observable) m

  let add_list ls o = List.fold_left (fun acc x -> add x acc) o ls
  let singleton x = add x empty

end


module List_observable = struct
  let singleton x = [x]
  let add x l = x :: l
  let empty = []
end

module Time_list = struct
  type sample = Data.times
  type t = sample list
  include List_observable
end

module File_list = struct
  type sample = float
  type t = float list
  include List_observable
end


module Ls = Time_stat(Time_list)
module Fs = Filesize_stat(File_list)

module Float_stable_average = Vec_calculus.Stable_average(Vec.Float)

let stable_average = Float_stable_average.compute
let map_stable_average = Float_stable_average.map_and_compute


let average = stable_average
let variance average = map_stable_average (fun x -> let diff = x -. average in diff *. diff )

type interval = { center:float; width:float }
type summary = { min:float; mean:interval }

let interval_average l =
  let n = List.length l in
  let mu = average (List.to_seq l) in
  let sigma_2 = variance mu (List.to_seq l) in
  let factor = (* should depend on the number of sample *) 2. in
  let width = factor *. sqrt (sigma_2 /. float n) in
  {center=mu; width}

let min l = match l with
  | [] -> 0.
  | a :: q -> List.fold_left min a q

let summarize l = { min = min l; mean = interval_average l }

let pretty_interval ppf x =
  Fmt.pf ppf "%g±%g" x.center x.width

let pp_interval ppf x =
  Fmt.pf ppf "%g %g" x.center x.width

let pp_summary ppf x =
  Fmt.pf ppf "%g %a" x.min pp_interval x.mean

let print times =
  By_files.iter (fun {pkg;name} times ->
      let mean = interval_average (List.map (fun x -> x.Data.typechecking) times) in
      Fmt.pr "%s/%s:average:%a(%a)@." pkg name pretty_interval mean Fmt.(Dump.list Data.pp_times) times
    )   times


let epsilon = 1e-6
let() = Random.self_init ()


let bootstrap array () =
  array.(Random.int (Array.length array))

let bootstrap_ratio x y () =
  bootstrap x () /. bootstrap y ()

let bootstrap_ratio_samples x y =
  let x, y = Array.of_list x, Array.of_list y in
  let n = max (Array.length x) (Array.length y) in
  Array.to_list @@ Array.init n (fun _ -> bootstrap_ratio x y ())

let print_raw_entry name ppf (key:File_key.t) times =
  let space ppf () = Fmt.pf ppf " " in
  let list = Fmt.(list ~sep:space Data.pp_times) in
  Fmt.pf ppf "%s %s %s %a@." name key.pkg key.name list times

let to_filename filename f =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  f fmt;
  Fmt.flush fmt ();
  close_out chan


let save_raw name stat =
  let name ="raw_"^ name ^ ".data" in
  to_filename name (fun fmt -> By_files.iter (print_raw_entry name fmt) stat)

module Slice0 = struct
  type index = Ty | Nonty | Total | Ratio
  type 'a t = { ty:'a; nonty:'a; total:'a; ratio:'a}
  let dim _ = 4
  let (.%()) x = function
    | Nonty -> x.nonty
    | Ty -> x.ty
    | Total -> x.total
    | Ratio -> x.ratio
  let init f = { nonty = f Nonty; ty = f Ty; total = f Total; ratio = f Ratio }
  let indices = Array.to_seq [|Ty; Nonty; Total; Ratio |]
  let to_seq x = Array.to_seq [|x.ty; x.nonty; x.total; x.ratio|]
end

module Slice = struct include Slice0 include Array_like.Expand(Slice0) end

let gen =
  let nonty =  List.map (fun (x:Data.times) -> x.total -. x.typechecking) in
  let ty = List.map (fun x -> x.Data.typechecking) in
  let total = List.map (fun x -> x.Data.total) in
  let ratio = List.map (fun x -> x.Data.typechecking/. x.total) in
  Slice.{nonty;total;ratio;ty}




type ('r,'alts) balanced = { main:'alts; ref:'r }
let pp_balanced pp_ref pp_alts ppf x =
  Fmt.pf ppf "%a %a"
    pp_ref x.ref
    pp_alts x.main

let pp_full_key ppf (key:File_key.t) = Fmt.pf ppf "%s:%s" key.pkg key.name


module Balanced(A:Array_like.t) = struct
  type 'a t = ('a, 'a A.t) balanced
  let map f { ref; main } = { main = A.map f main; ref = f ref }

  type simplified = summary t Slice.t

  let simplify ref alts = By_files.fold (fun key ref_times m ->
      match A.map (By_files.find key) alts with
      | exception Not_found -> m
      | all_times ->
        if A.exists (fun times -> List.length times < 2) all_times
        &&  List.length ref_times < 2 then
          m
        else
          let map_summary x f = map (fun x -> summarize (f x)) x in
          let data = { ref = ref_times; main = all_times } in
          let data = Slice.map (map_summary data) gen in
          if A.for_all ( fun x -> x.min > epsilon) data.ty.main  then
            By_files.add key data m
          else
            m
    ) ref By_files.empty

  let space ppf () = Fmt.pf ppf " "

  let save_entry fmt pp_key key slice =
    let pp = pp_balanced pp_summary (A.pp pp_summary) in
    Fmt.pf fmt "%a %a@."
      pp_key key
      (Slice.pp pp) slice


let save filename m = to_filename filename (fun fmt ->
    Fmt.pf fmt
      "File \
       Typechecking_ref_min Typechecking_ref_mean Typechecking_ref_std \
       Typechecking_main_min Typechecking_main_mean Typechecking_main_std \
       Other_ref_min Other_ref_mean Other_ref_std \
       Other_main_min Other_main_mean Other_main_std \
       Total_ref_min Total_ref_mean Total_ref_std \
       Total_main_min Total_main_mean Total_main_std \
       Ratio_ref_min Ratio_ref_mean Ratio_ref_std \
       Ratio_main_min Ratio_main_mean Ratio_main_std@."
    ;
      By_files.iter (save_entry fmt pp_full_key) m
  )

end


module type Convex_space = sig
  type t
  val compare: t -> t -> int
  val distance_2: t -> t -> float
  val isobarycenter: t Seq.t -> t
  val pp: Format.formatter -> t -> unit
end

module type Input_space = sig
  type t
  type target
  val compare: t -> t -> int
  val proj: t -> target
end


module Kmeans(P:Convex_space)(I: Input_space with type target := P.t) = struct

  module Points = Set.Make(P)
  module Group = Set.Make(I)

  type center_data = { center: P.t; points: Points.t; group: Group.t }

  let add_point centers x =
    let find_min point centers =
      let _, candidate, _ =
        Array.fold_left (fun (distance, candidate, current) x ->
            let d = P.distance_2 x.center point in
            if d < distance then
              (d,current, current+1)
            else
              (distance, candidate, current + 1)
          ) (Float.infinity, 0, 0) centers
      in
      candidate
    in
    let y = I.proj x in
    let m = find_min y centers in
    let nearest = centers.(m) in
    centers.(m) <- {
      center = nearest.center;
      points = Points.add y nearest.points;
      group= Group.add x nearest.group
    }

  let init center = { center; points = Points.empty; group = Group.empty }

  let update_centers iter epsilon centers =
    iter (add_point centers);
    Array.mapi (fun k x ->
        let new_center = P.isobarycenter (Points.to_seq x.points) in
        let dist = P.distance_2 new_center x.center in
        begin if dist > epsilon then
          centers.(k) <- init new_center
          else
            centers.(k) <- { centers.(k) with center = new_center }
        end;
        dist
      )
      centers

  exception Convergence_failure

  let rec fix_point fuel epsilon iter centers =
    if fuel = 0 then raise Convergence_failure
    else
      let dists = update_centers iter epsilon centers in
      if Array.for_all (fun d -> d < epsilon) dists then
        centers
      else
        fix_point (fuel - 1) epsilon iter centers


  let rand_choose_k n seq k =
    let indices = Array.init k (fun _ -> Random.int n) in
    let () = Array.sort compare indices in
    let rec search found k indices pos seq =
      if k = Array.length indices then Array.of_list found
      else
        match seq () with
        | Seq.Nil -> Array.of_list found
        | Seq.Cons(x, seq) ->
          if indices.(k) = pos then
            search (init (I.proj x)::found) (k+1) indices (pos + 1) seq
        else
          search found k indices (pos + 1) seq
    in
    search [] 0 indices 0 seq

  let compute ~k ~fuel ~epsilon n seq =
    let centers = rand_choose_k n seq k in
    fix_point fuel epsilon (fun f -> Seq.iter f seq) centers

end


module Interval_as_vec = struct
  type t = interval
  let ( + ) x y = { center= x.center +. y.center; width = x.width +. y.width }
  let ( - ) x y = { center = x.center -. y.center; width = x.width -. y.width }
  let (|*|) x y = x.center *. y.center +. x.width *. y.width
  let ( *. ) l x = { center = l *. x.center; width = l *. x.width }
  let ( /. ) x l = { center = x.center /. l; width = x.width /. l }
  let zero = { center = 0.; width = 0. }
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp = pretty_interval
end


module Summary_as_vec = struct
  type t = summary
  let ( + ) x y = { min= x.min +. y.min; mean = Interval_as_vec.(x.mean + y.mean) }
  let ( - ) x y =   { min= x.min -. y.min; mean = Interval_as_vec.(x.mean - y.mean) }
  let (|*|) x y = x.min *. y.min +. Interval_as_vec.(x.mean|*|y.mean)
  let ( *. ) l x = { min= l *. x.min; mean = Interval_as_vec.(l *. x.mean) }
  let ( /. ) x l = { min= x.min /. l; mean = Interval_as_vec.(x.mean /. l) }
  let zero = { min=0.; mean = Interval_as_vec.zero}
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp = pp_summary
end

module Balanced_as_vec(V:Vec.t)(A:Array_like.t)= struct
  type t = (V.t, V.t A.t) balanced
  open V

  let ( + ) x y = { main = A.map2 (+) x.main y.main; ref = x.ref + y.ref }
  let ( - ) x y = { main = A.map2 (-) x.main y.main; ref = x.ref - y.ref }
  let (|*|) x y = A.fold_2 V.(|*|) (+.) 0. x.main y.main +. V.(y.ref|*|y.ref)
  let ( *. ) l x = { main = A.map ( ( *. ) l ) x.main; ref = l *. x.ref }
  let ( /. ) x l = { main = A.map (fun x -> x /. l) x.main; ref = x.ref /. l }
  let zero = { main = A.init (fun _ -> V.zero); ref = V.zero }
  let compare x y =
    let res = A.compare ~cmp:V.compare x.main y.main in
    if res = 0 then V.compare x.ref y.ref
    else res
  let pp ppf x = Fmt.pf ppf "%a(%a)" (A.pp V.pp) x.main V.pp x.ref
end

module Pair(X:Vec.t)(Y:Vec.t) = struct
  type t = X.t * Y.t
  let ( + ) (x,u) (y,v) = X.( x + y ), Y.(u + v)
  let ( - ) (x,u) (y,v) = X.( x - y ), Y.(u - v)
  let (|*|) (x,u) (y,v) = X.(x|*|y) +. Y.(u|*|v)
  let zero = X.zero, Y.zero
  let ( *. ) l (x,y) = X.(l *. x), Y.(l *. y)
  let ( /. ) (x,y) l = X.(x /. l), Y.(y/.l)
  let compare (x1,y1) (x2,y2) =
    let res = X.compare x1 x2 in
    if res = 0 then Y.compare y1 y2
    else res
  let pp ppf (x,y) = Fmt.pf ppf "(%a,%a)" X.pp x Y.pp y
end


module Summary_b(Alts:Array_like.t)= Balanced_as_vec(Summary_as_vec)(Alts)
module Pairs(Alts:Array_like.t) = Pair(Summary_b(Alts))(Summary_b(Alts))



module Q = struct
  type t = float Types.input
  let value q = q.Types.data
  let compare x y = compare (value x) (value y)
  let key x = x.Types.key
  let pp ppf q = Fmt.pf ppf "%g %a" (value q) pp_full_key (key q)
end
type ordered = Ordered of Q.t array [@@unboxed]


let order_statistic all =
  let all = Array.of_seq all in
  let () = Array.sort Q.compare all in
  Ordered all

let quantile (Ordered ordered_points) i = ordered_points.( int_of_float @@ i *. float (Array.length ordered_points)  )

let quantiles_table name ordered_points ppf =
  let row ppf i = Fmt.pf ppf "| %g%% | %g |@," (100. *. i) (Q.value @@ quantile ordered_points i) in
  Fmt.pf ppf "@[<v>\
              | %% |  %s quantiles      |@,\
              |---|--------------------|@,"
    name
  ;
  List.iter (row ppf) [0.01; 0.1; 0.25; 0.5; 0.75;0.9;0.99;0.999];
  Fmt.pf ppf "@]@."


let quantiles_data (Ordered ordered_points) ppf =
  Array.iteri
    (fun i x -> Fmt.pf ppf "%g %g %a@."
        (Q.value x)
        ((100. *. float i) /. float (Array.length ordered_points))
        pp_full_key (Q.key x)
    )
    ordered_points

let histogram nbins (Ordered ordered_points) =
  let ordered_points = Array.map Q.value ordered_points in
  let npoints = Array.length ordered_points in
  let size =  npoints / nbins in
  let rest = npoints - nbins * size in
  let sizes = Array.init (nbins + 1) (fun i -> if i = 0 then 0 else if i < rest then size + 1 else size) in
  let () = Array.iteri (fun i x -> if i > 0 then sizes.(i) <- x + sizes.(i-1)) sizes in
  Array.init nbins (fun n -> Array.sub ordered_points sizes.(n) (sizes.(n+1)-sizes.(n)))

let pp_histogram h ppf =
  let print_cell ppf a =
    let min =  a.(0) in
    let max = a.(Array.length a - 1) in
    let s = Array.length a in
    let density = float s /.  (max -. min) in
    let density = if density > Float.max_float then Float.max_float /. 100. else density in
    Fmt.pf ppf "%g %g %g@."  min max density
  in
  Array.iter (print_cell ppf) h

let save_histogram name h = to_filename name (pp_histogram  h)
let save_quantiles name ordered = to_filename name (quantiles_data ordered)
let save_quantile_table name filename ordered = to_filename filename (quantiles_table name ordered)
