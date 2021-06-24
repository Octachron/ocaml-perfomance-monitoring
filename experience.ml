

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

let execute ~dir ~switch ~pkg =
  putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir;
  cmd "(eval $(opam env --set-switch --switch=%S)  && opam reinstall %s)"
    switch pkg

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
      Fmt.pr "%s/%s:%a@." pkg subpart Fmt.(Dump.list pp_times) times
    )   times

let sample ~pkg stats =
  let dir = pkg_dir ~switch:after ~pkg in
  let return = execute ~switch:after ~pkg ~dir in
  if return <> 0 then exit return
  else
  let stats = read_result ~dir ~pkg stats in
  stats

let rec multisample n ~pkg stats =
  if n = 0 then stats else
   multisample (n-1) ~pkg (sample ~pkg stats)

let remove_pkg pkg = cmd "opam remove --yes %s" pkg

let () =
  let stats = multisample 2 ~pkg:"dune" Ls.empty in
  print stats
