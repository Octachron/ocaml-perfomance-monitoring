let cmd fmt = Format.kasprintf Sys.command fmt

let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt

let set_switch switch ppf = Fmt.pf ppf "eval $(opam env --set-switch --switch=%S)" switch
let with_switch ~switch fmt =
  cmd ("(%t &&" ^^ fmt ^^ ")") (set_switch switch)

let rec with_retry ~msg ~retry f =
  match f (), retry with
  | 0, _ | _, 0 -> 0
  | _, retry ->
    Fmt.(pf stderr) "Error during %t. Retrying %d times.@." msg retry;
    with_retry ~msg ~retry:(retry - 1) f

let reinstall ~retry ~switch ~pkg =
  with_retry
    ~msg:(Format.dprintf "reinstallation of %s"  (Pkg.full pkg))
    ~retry
    (fun () -> with_switch ~switch "opam reinstall --no-depexts -b --yes %s" (Pkg.name pkg))

let install ~retry  ~switch ~pkgs =
  with_retry ~retry
    ~msg:(Format.dprintf "reinstallation of %a"  (Fmt.(list string))  (List.map Pkg.full pkgs))
    (fun () -> with_switch ~switch "opam install --no-depexts -b --yes %s" (String.concat " " @@ List.map Pkg.name pkgs))

let opam_var ~switch ~pkg var =
  let inp, _ = Unix.open_process (Format.asprintf "(%t && opam var %s:%s)" (set_switch switch) (Pkg.name pkg) var) in
  input_line inp


let execute ~retry ~dir ~switch ~pkg =
  putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir;
  putenv_fmt "OPAMJOBS" "1";
  reinstall ~retry ~switch ~pkg



let (<!>) n err =
  if n = 0 then () else (err Fmt.stderr ; exit n)


let remove_pkg ~switch pkgs =
  cmd "(%t && opam remove --no-depexts --yes %s)" (set_switch switch) (String.concat " " @@ List.map Pkg.name pkgs)
  <!> Format.dprintf "Failed to remove %a" (Fmt.list Fmt.Dump.string) (List.map Pkg.full pkgs)
