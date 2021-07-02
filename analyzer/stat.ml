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
end


module type Fold = sig
  type 'a t
  val fold: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a

end

module Stable_average(V:Vec)(C:Fold) = struct
  let compute f c =
    let _, s = C.fold (fun (n,mn) x ->
        let n = n + 1 in
        n, V.( mn + (f x - mn) /. float n )
      ) (0, V.zero) c
    in
    s
end

module Float_as_vec = struct
  let ( *. ) = ( *. )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( /. ) = ( /. )
  let zero = 0.
  type t = float
end

module Float_stable_average = Stable_average(Float_as_vec)(struct type 'a t = 'a List.t let fold = List.fold_left end)

let stable_average = Float_stable_average.compute

let average = stable_average Fun.id
let variance average = stable_average (fun x -> let diff = x -. average in diff *. diff )

type interval = { center:float; width:float }

let interval_average l =
  let n = List.length l in
  let mu = average l in
  let sigma_2 = variance mu l in
  let factor = (* should depend on the number of sample *) 2. in
  let width = factor *. sqrt (sigma_2 /. float n) in
  {center=mu; width}

let pretty_interval ppf x =
  Fmt.pf ppf "%g±%g" x.center x.width

let pp_interval ppf x =
  Fmt.pf ppf "%g %g" x.center x.width

let print times =
  M.iter (fun {pkg;subpart} times ->
      let i = interval_average (List.map (fun x -> x.Data.typechecking) times) in
      Fmt.pr "%s/%s:average:%a(%a)@." pkg subpart pretty_interval i Fmt.(Dump.list Data.pp_times) times
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

let save_raw name stat =
  let out = open_out ("raw_"^ name ^ ".data") in
  let fmt = Format.formatter_of_out_channel out in
  M.iter (print_raw_entry name fmt) stat;
  close_out out


type 'a balanced_interval = { main:'a; ref:'a }

let nonty =  List.map (fun (x:Data.times) -> x.total -. x.typechecking)
let ty = List.map (fun x -> x.Data.typechecking)


let simplify ref main = M.fold (fun key times m ->
    match M.find key ref with
    | exception Not_found -> m
    | ref_times ->
      if List.length times < 2 &&  List.length ref_times < 2 then
        m
      else

        let ty =
          { main = interval_average (ty times);
            ref  = interval_average (ty ref_times)
          }
        in
        let nonty = { ref = interval_average (nonty ref_times);
                      main = interval_average (nonty times)
                    }
        in
        if ty.main.center +. ty.main.width > epsilon then
          M.add key (ty, nonty) m
        else
          m
  ) main M.empty

let save_entry fmt pp_key key (ty, nonty) =
          Fmt.pf fmt "%a %a %a %a %a@."
            pp_key key
            pp_interval ty.ref
            pp_interval ty.main
            pp_interval nonty.ref
            pp_interval nonty.main

let pp_full_key ppf (key:Key.t) = Fmt.pf ppf "%s:%s" key.pkg key.subpart

let save filename m =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  M.iter (save_entry fmt pp_full_key) m;
  Fmt.flush fmt ();
  close_out chan


module type Convex_space = sig
  type t
  val compare: t -> t -> int
  val distance: t -> t -> float
  val isobarycenter: int -> t Seq.t -> t
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
            let d = P.distance x.center point in
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
      group=Group.add x nearest.group
    }

  let init center = { center; points = Points.empty; group = Group.empty }

  let update_centers iter centers =
    iter (add_point centers);
    Array.mapi (fun k x ->
        let new_center = P.isobarycenter (Points.cardinal x.points) (Points.to_seq x.points) in
        let dist = P.distance new_center x.center in
        centers.(k) <- init new_center;
        dist
      )
      centers

  exception Convergence_failure

  let rec fix_point fuel epsilon iter centers =
    if fuel = 0 then raise Convergence_failure
    else
      let dists = update_centers iter centers in
      if Array.for_all (fun d -> d < epsilon) dists then
        centers
      else
        fix_point (fuel - 1) epsilon iter centers


  let rand_choose_k n seq k =
    let indices = Array.init k (fun _ -> Random.int n ) in
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

  let compute ~k ~fuel epsilon n seq =
    let centers = rand_choose_k n seq k in
    fix_point fuel epsilon (fun f -> Seq.iter f seq) centers

end
