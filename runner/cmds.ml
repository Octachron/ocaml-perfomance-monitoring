let cmd fmt = Format.kasprintf Sys.command fmt

let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt


let set_switch switch ppf = Fmt.pf ppf "eval $(opam env --set-switch --switch=%S)" switch

let reinstall ~switch ~pkg =
  cmd "(%t && opam reinstall --yes %s)" (set_switch switch) pkg

let install ~switch ~pkg =
  cmd "(%t && opam install --yes %s)" (set_switch switch) pkg

let execute ~dir ~switch ~pkg =
  putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir;
  putenv_fmt "OPAMJOBS" "1";
  reinstall ~switch ~pkg

let (<!>) n err =
  if n = 0 then () else (err Fmt.stderr ; exit n)


let remove_pkg ~switch pkg =
  cmd "(%t && opam remove --yes %s)" (set_switch switch) pkg
  <!> Format.dprintf "Failed to remove %S" pkg
