module type observable = sig
  type sample
  type t
  val empty: t
  val singleton: sample -> t
  val add: sample -> t -> t
end

module Key = struct
  type t = { pkg: string; subpart: string }
  let compare: t -> t -> int = Stdlib.compare
end

module M = Map.Make(Key)
module Pkg = Map.Make(String)



module Stat(O:observable with type sample = Data.times) = struct
  let empty = M.empty
  let add {Data.switch=_; pkg;subpart;time;total_time} m =
    let key = { Key.pkg; subpart } in
    let observation : Data.times = { typechecking=time; total=total_time } in
    match M.find key m with
    | exception Not_found -> M.add key (O.singleton observation) m
    | observable -> M.add key O.(add observation observable) m

  let add_list ls o = List.fold_left (fun acc x -> add x acc) o ls
end

module List_observable = struct
  type sample = Data.times
  type t = sample list
  let singleton x = [x]
  let add x l = x :: l
  let empty = []
end

module Ls = Stat(List_observable)


module type Vec = sig
  type t
  val zero: t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val (  *. ): float -> t -> t
  val (  /. ): t -> float -> t
  val ( |*| ): t -> t -> float
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end


module type Fold = sig
  type 'a t
  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

end

module Stable_average(V:Vec)(C:Fold) = struct
  let map_and_compute f c =
    let _, s = C.fold (fun (n,mn) x ->
        let n = n + 1 in
        n, V.( mn + (f x - mn) /. float n )
      ) (0, V.zero) c
    in
    s

  let compute c =
    let _, s = C.fold (fun (n,mn) x ->
        let n = n + 1 in
        n, V.( mn + (x - mn) /. float n )
      ) (0, V.zero) c
    in
    s

end

module Float_as_vec = struct
  let ( *. ) = ( *. )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( /. ) = ( /. )
  let (|*|) x y = x *. y
  let zero = 0.
  type t = float
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp = Fmt.float
end

module Float_stable_average = Stable_average(Float_as_vec)(struct type 'a t = 'a List.t let fold = List.fold_left end)

let stable_average = Float_stable_average.compute
let map_stable_average = Float_stable_average.map_and_compute


let average = stable_average
let variance average = map_stable_average (fun x -> let diff = x -. average in diff *. diff )

type interval = { center:float; width:float }
type summary = { min:float; mean:interval }

let interval_average l =
  let n = List.length l in
  let mu = average l in
  let sigma_2 = variance mu l in
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
  M.iter (fun {pkg;subpart} times ->
      let mean = interval_average (List.map (fun x -> x.Data.typechecking) times) in
      Fmt.pr "%s/%s:average:%a(%a)@." pkg subpart pretty_interval mean Fmt.(Dump.list Data.pp_times) times
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

let print_raw_entry name ppf (key:Key.t) times =
  let space ppf () = Fmt.pf ppf " " in
  let list = Fmt.(list ~sep:space Data.pp_times) in
  Fmt.pf ppf "%s %s %s %a@." name key.pkg key.subpart list times

let to_filename filename f =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  f fmt;
  Fmt.flush fmt ();
  close_out chan


let save_raw name stat =
  let name ="raw_"^ name ^ ".data" in
  to_filename name (fun fmt -> M.iter (print_raw_entry name fmt) stat)


type 'a balanced = { main:'a; ref:'a }

let nonty =  List.map (fun (x:Data.times) -> x.total -. x.typechecking)
let ty = List.map (fun x -> x.Data.typechecking)
let total = List.map (fun x -> x.Data.total)
let ratio = List.map (fun x -> x.Data.typechecking/. x.total)

let map f { ref; main } = { main = f main; ref = f ref }

type simplified = { ty: (summary balanced as 'a); nonty:'a; total:'a; ratio:'a }

let simplify ref main = M.fold (fun key times m ->
    match M.find key ref with
    | exception Not_found -> m
    | ref_times ->
      if List.length times < 2 &&  List.length ref_times < 2 then
        m
      else
        let map_summary f x = map (fun x -> summarize (f x)) x in
        let data = { ref = ref_times; main = times } in
        let ty = map_summary ty data in
        let nonty = map_summary nonty data in
        let ratio = map_summary ratio data in
        let total = map_summary total data in
        if ty.main.min > epsilon then
          M.add key {ty; nonty; total; ratio} m
        else
          m
  ) main M.empty

let pp_balanced pp ppf x = Fmt.pf ppf "%a %a" pp x.ref pp x.main
let save_entry fmt pp_key key {ty; nonty; total; ratio } =
  let pp = pp_balanced pp_summary in
  Fmt.pf fmt "%a %a %a %a %a@."
    pp_key key
    pp ty
    pp nonty
    pp total
    pp ratio

let pp_full_key ppf (key:Key.t) = Fmt.pf ppf "%s:%s" key.pkg key.subpart


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
       Ratio_main_min Ratio_main_mean Ratio_main_std"
    ;
      M.iter (save_entry fmt pp_full_key) m
  )

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


module Balanced_as_vec(V:Vec) = struct
  type t = V.t balanced
  open V
  let ( + ) x y = { main = x.main + y.main; ref = x.ref + y.ref }
  let ( - ) x y = { main = x.main - y.main; ref = x.ref - y.ref }
  let (|*|) x y = V.(x.main|*|y.main) +. V.(y.ref|*|y.ref)
  let ( *. ) l x = { main = l *. x.main; ref = l *. x.ref }
  let ( /. ) x l = { main = x.main /. l; ref = x.ref /. l }
  let zero = { main = V.zero; ref = V.zero }
  let compare x y =
    let res = V.compare x.main y.main in
    if res = 0 then V.compare x.ref y.ref
    else res
  let pp ppf x = Fmt.pf ppf "%a(%a)" V.pp x.main V.pp x.ref
end

module Pair(X:Vec)(Y:Vec) = struct
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

module R3 = struct
  type scalar = float
  type t = {x:scalar; y:scalar; z:scalar}
  let ( + ) u v= { x=u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }
  let ( - ) u v= { x=u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }
  let zero = { x =0.; y = 0.; z = 0. }
  let (|*|) u v = u.x *. v.x +. u.y *. v.y +. u.z *. v.z
  let ( *. ) l u = { x = l *. u.x; y = l *. u.y; z = l *. u.z }
  let ( /.  ) u l = { x = u.x /. l; y = u.y /. l; z = u.z /. l }
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp ppf v = Fmt.pf ppf "(%g %g %g)" v.x v.y v.z
end

module Summary_b = Balanced_as_vec(Summary_as_vec)
module Pairs = Pair(Summary_b)(Summary_b)

module Convex_from_vec(V:Vec) = struct
  include V
  let distance_2 x y =
    let d = V.(x - y) in
    V.(d|*|d)
  module Stable = Stable_average(V)(struct
      type 'a t = 'a Seq.t
      let fold = Seq.fold_left
    end)
  let isobarycenter = Stable.compute
end


let quantiles proj all ppf =
  let () = Array.sort (fun x y -> compare (proj x) (proj y)) all in
  let n = Array.length all in
  let quantile i = proj all.( int_of_float @@ i *. float n  ) in
  Fmt.pr "Quantiles: 1%% :%g; 10%%: %g -> 25%%: %g -> 50%%: %g -> 75%%: %g -> 90%%: %g -> 99%%: %g -> 99.9%%: %g @."
    (quantile 0.01)
    (quantile 0.1)
    (quantile 0.25)
    (quantile 0.5)
    (quantile 0.75)
    (quantile 0.9)
    (quantile 0.99)
    (quantile 0.999)
  ;
  Array.iteri (fun i x -> Fmt.pf ppf "%g %g@." (proj x) ((100. *. float i) /. float n)) all

let histogram nbins proj all =
  let () = Array.sort (fun x y -> compare (proj x) (proj y)) all in
  let npoints = Array.length all in
  let size =  npoints / nbins in
  let rest = npoints - nbins * size in
  let sizes = Array.init (nbins + 1) (fun i -> if i = 0 then 0 else if i < rest then size + 1 else size) in
  let () = Array.iteri (fun i x -> if i > 0 then sizes.(i) <- x + sizes.(i-1)) sizes in
  Array.init nbins (fun n -> Array.sub all sizes.(n) (sizes.(n+1)-sizes.(n)))

let pp_histogram proj h ppf =
  let print_cell ppf a =
    let min = proj a.(0) in
    let max = proj a.(Array.length a - 1) in
    let s = Array.length a in
    let density = float s /.  (max -. min) in
    let density = if density > Float.max_float then Float.max_float /. 100. else density in
    Fmt.pf ppf "%g %g %g@."  min max density
  in
  Array.iter (print_cell ppf) h

 let save_histogram name proj h = to_filename name (pp_histogram proj h)
 let save_quantiles name proj all = to_filename name (quantiles proj all)
