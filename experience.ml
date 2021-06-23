

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

module Key = struct
  type t = string
  let compare: t -> t -> int = Stdlib.compare
end

module Map = Map.Make(Key)

type entry = { name:string; time:float; children: entry list}

module Parse = struct

  let rec indentation l pos =
    if pos < String.length l && l.[pos] = ' ' then
      indentation l (pos+1)
    else
      pos
  let indentation l = indentation l 0

  let read_lines filename =
    let chan = open_in filename in
    let rec read_lines l =
      match input_line chan with
      | exception End_of_file -> List.rev l
      | newline -> read_lines (newline::l)
    in
    let r = read_lines [] in
    close_in chan;
    r


  let scan_entry s = Scanf.sscanf s " %fs %s" (fun time name -> name, time )

  let rec parse_group parent_indent l =
    match l with
    | [] -> [], []
    | line :: rest as full ->
      let indent = indentation line in
      if indent <= parent_indent then
        [], full
      else
        let name, time = scan_entry line in
        let children, rest = parse_group indent rest in
        let entry = { name; time; children } in
        let entries, rest =parse_group parent_indent rest in
        entry :: entries, rest

  let profile filename =
    let lines = read_lines filename in
    let group, rest = parse_group (-1) lines in
    assert (rest = []);
    group

end


let typechecking_times l  =
  let typechecking {name; time; children} =
    match List.find_opt (fun child -> child.name = "typing") children with
    | None -> name, 0., time
    | Some child -> name, child.time, time
  in
  List.map typechecking l

let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt

let pkg_dir ~switch ~pkg =
   uuid @@ output_dir (switch ^ "-" ^ pkg)

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

let rec fold_other l = match l with
  | a :: { name = "other"; time; children = [] } :: q ->
    fold_other ({ a with time = a.time +. time } :: q)
   | a :: q -> a :: fold_other q
   | [] -> []

let read_result ~dir =
  let files = Array.to_list @@ Sys.readdir dir in
  let read_file filename =
    if is_prefix ~prefix:"profile" (Filename.basename filename) then
      Some (Parse.profile (Filename.concat dir filename))
    else
       None
  in
  let profiles = List.filter_map read_file files in
  let profiles = List.map fold_other profiles in
  let times = List.concat_map typechecking_times profiles in
  let pp_time ppf (name,typechecking,total) =
    Fmt.pf ppf "%s: %.3gs/%.3gs" name typechecking total
  in
  Fmt.pr "@[<v>Typechecking time:@ %a@]@."
    Fmt.(list ~sep:(fun ppf () -> Fmt.pf ppf "@ ") pp_time) times;
  let typing_total, full_total = List.fold_left (fun (tty, ttot) (_,ty,tot) ->
      tty +. ty, ttot +. tot) (0.,0.) times
  in
  let n, concentration = List.fold_left (fun (n,ct) (_,ty,tot) ->
      1 + n, ct +. (ty /. tot) ** 2.) (0,0.) times
  in
  Fmt.pr "@[Total: %.3g/%.3g, concentation: %g @]@." typing_total full_total (concentration /. float n)


let () =
  let dir = pkg_dir ~switch:after ~pkg:"dune" in
  let return = execute ~switch:after ~pkg:"dune" ~dir in
  if return <> 0 then exit return
  else read_result ~dir
