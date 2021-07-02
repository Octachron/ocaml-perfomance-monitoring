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

let stable_average f l =
  let _, s = List.fold_left (fun (n,mn) x ->  n + 1,  mn +. (f x -. mn) /. float (n + 1) ) (0, 0.) l in
  s

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

let save_entry fmt pp_key ref key times =
  match M.find key ref with
  | exception Not_found -> ()
  | ref_times ->
    if List.length times > 1 &&  List.length ref_times > 1 then
      begin
        let nonty =  List.map (fun (x:Data.times) -> x.total -. x.typechecking) in
        let ty = List.map (fun x -> x.Data.typechecking) in
        let ty_ref = interval_average (ty ref_times) in
        let ty = interval_average (ty times) in
        let nonty_ref = interval_average (nonty ref_times) in
        let nonty = interval_average (nonty times) in
        if ty.center +. ty.width > epsilon then
          Fmt.pf fmt "%a %a %a %a %a@."
            pp_key key
            pp_interval ty_ref
            pp_interval ty
            pp_interval nonty_ref
            pp_interval nonty
      end

let pp_full_key ppf (key:Key.t) = Fmt.pf ppf "%s:%s" key.pkg key.subpart

let save filename ~ref_times ~times =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  M.iter (save_entry fmt pp_full_key ref_times) times;
  Fmt.flush fmt ();
  close_out chan
