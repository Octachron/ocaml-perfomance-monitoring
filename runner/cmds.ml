let cmd fmt = Format.kasprintf Sys.command fmt

let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt

module Pkg = struct
  let name (x,_y) = x
  let full (x,y) = Format.asprintf "%s.%s" x y
end

let set_switch switch ppf = Fmt.pf ppf "eval $(opam env --set-switch --switch=%S)" switch
let with_switch ~switch fmt =
  cmd ("(%t &&" ^^ fmt ^^ ")") (set_switch switch)

let reinstall ~switch ~pkg =
  with_switch ~switch "opam reinstall -b --yes %s" (Pkg.name pkg)

let install ~switch ~pkg =
  with_switch ~switch "opam install -b --yes %s" (Pkg.full pkg)

let opam_var ~switch ~pkg var =
  let inp, _ = Unix.open_process (Format.asprintf "(%t && opam var %s:%s)" (set_switch switch) (Pkg.name pkg) var) in
  input_line inp


let execute ~dir ~switch ~pkg =
  putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir;
  putenv_fmt "OPAMJOBS" "1";
  reinstall ~switch ~pkg

let (<!>) n err =
  if n = 0 then () else (err Fmt.stderr ; exit n)


let remove_pkg ~switch pkgs =
  cmd "(%t && opam remove --yes %s)" (set_switch switch) (String.concat " " @@ List.map Pkg.name pkgs)
  <!> Format.dprintf "Failed to remove %a" (Fmt.list Fmt.Dump.string) (List.map Pkg.full pkgs)
