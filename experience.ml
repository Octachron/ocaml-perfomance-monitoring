

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"


let cmd fmt = Format.kasprintf Sys.command fmt

let output_dir name = "/tmp/ocaml-profile-" ^ name

let uuid name =
  let rec loop n =
    let guess = output_dir (name ^ string_of_int n) in
    if not (Sys.file_exists guess) then guess
    else loop (n+1)
   in
  let guess = output_dir name in
  if not (Sys.file_exists guess) then guess else
  loop 2


type typechecking_stat = { pkg: string; subpart: string; time:float; total_time:float }

let typechecking_times pkg l  =
  let typechecking {Parse.name; time; children} =
    match List.find_opt (fun child -> child.Parse.name = "typing") children with
    | None -> { pkg; subpart=name;  time=0.; total_time=time}
    | Some child -> { pkg; subpart=name; time=child.time; total_time = time}
  in
  List.map typechecking l


type times = { typechecking: float; total: float }

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
module Stat(O:observable with type sample = times) = struct
  let empty = M.empty
  let add {pkg;subpart;time;total_time} m =
    let key = { Key.pkg; subpart } in
    let observation : times = { typechecking=time; total=total_time } in
    match M.find key m with
    | exception Not_found -> M.add key (O.singleton observation) m
    | observable -> M.add key O.(add observation observable) m

  let add_list ls o = List.fold_left (fun acc x -> add x acc) o ls
end



let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt

let pkg_dir ~switch ~pkg =
   uuid @@ (switch ^ "-" ^ pkg)

let set_switch switch ppf = Fmt.pf ppf "eval $(opam env --set-switch --switch=%S)" switch

let execute ~dir ~switch ~pkg =
  putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir;
  putenv_fmt "OPAMJOBS" "1";
  cmd "(%t && opam reinstall --yes %s)" (set_switch switch) pkg

let rec is_prefix_until prefix s len pos =
  pos >= len ||
  (
    prefix.[pos] = s.[pos]
    && is_prefix_until prefix s len (pos + 1)
  )

let is_prefix ~prefix s = is_prefix_until prefix s (String.length prefix) 0



let pp_times ppf {typechecking; total} =
  Fmt.pf ppf "%.3gs/%.3gs" typechecking total

(*

let () =
Fmt.pr "@[<v>Typechecking time:@ %a@]@."
  Fmt.(l  ist ~sep:(fun ppf () -> Fmt.pf ppf "@ ") pp_time) times;


let typing_total, full_total = List.fold_left (fun (tty, ttot) (_,ty,tot) ->
      tty +. ty, ttot +. tot) (0.,0.) times
  in
  let concentration = List.fold_left (fun ct (_,ty,tot) -> max ct (ty /. tot) ) 0. times
*)


module List_observable = struct
  type sample = times
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

let interval_average l =
  let n = List.length l in
  let mu = average l in
  let sigma_2 = variance mu l in
  let factor = (* should depend on the number of sample *) 2. in
  let width = factor *. sqrt (sigma_2 /. float n) in
  mu, width


let read_result ~pkg ~dir stats =
  let files = Array.to_list @@ Sys.readdir dir in
  let read_file filename =
    if is_prefix ~prefix:"profile" (Filename.basename filename) then
      Some (Parse.profile (Filename.concat dir filename))
    else
      None
  in
  let profiles = List.filter_map read_file files in
  List.fold_left (fun stat x -> Ls.add_list (typechecking_times pkg x) stat) stats profiles
(*
  Fmt.pr "@[Total: %.3g/%.3g, concentation: %g @]@." typing_total full_total concentration
*)


let print times =
  M.iter (fun {pkg;subpart} times ->
      let mu, width = interval_average (List.map (fun x -> x.typechecking) times) in
      Fmt.pr "%s/%s:average:%g±%g(%a)@." pkg subpart mu width Fmt.(Dump.list pp_times) times
    )   times


let epsilon = 1e-6

let save filename times =
  let chan = open_out filename in
  let fmt = Format.formatter_of_out_channel chan in
  M.iter (fun {pkg;subpart} times ->
      let mu, width = interval_average (List.map (fun x -> x.typechecking) times) in
      if mu +. width > epsilon then
        Fmt.pf fmt "%s:%s %g %g@." pkg subpart mu width
    ) times;
  Fmt.flush fmt ();
  close_out chan



let (<!>) n err =
  if n = 0 then () else (err Fmt.stderr ; exit n)


let sample ~switch ~pkg stats =
  let dir = pkg_dir ~switch:after ~pkg in
  execute ~switch ~pkg ~dir <!> Format.dprintf "Failed to install %s" pkg;
  let stats = read_result ~dir ~pkg stats in
  stats

let rec multisample n ~switch ~pkg stats =
  if n = 0 then stats else
   multisample (n-1) ~pkg ~switch (sample ~switch ~pkg stats)


let remove_pkg ~switch pkg =
  cmd "(%t && opam remove --yes %s)" (set_switch switch) pkg
  <!> Format.dprintf "Failed to remove %S" pkg

let pkg_line n ~switch pkgs stats =
  List.iter (remove_pkg ~switch) (List.rev pkgs);
  List.fold_left  (fun stats pkg ->
      multisample n ~switch ~pkg stats
    ) stats pkgs

let () =
  let line = [ "ocamlfind"; "num"; "zarith" ] in
  let switch = after in
  let stats = pkg_line ~switch 5 line Ls.empty in
  save "basic.data"  stats;
  print stats
