
let from_branch name = Format.asprintf "Octachron-ocaml-%s" name

let reverted = from_branch "shape_reverted"
let initial = from_branch "shape_pr10796_minimal"
let call_by_need = from_branch "shape_10825_strong_call_by_need"


let simple = [
  "ocamlfind", "1.9.1";
  "num", "1.4"
]

let irmin =
  [("conf-pkg-config", "2"); ("seq", "base"); ("ocamlfind", "1.9.1");
   ("base-bytes", "base"); ("ocamlbuild", "0.14.0"); ("uchar", "0.0.2");
   ("dune", "2.9.1"); ("stdlib-shims", "0.3.0"); ("bigarray-compat", "1.0.0");
   ("csexp", "1.5.1"); ("mmap", "1.1.0"); ("result", "1.5");
   ("topkg", "1.0.4"); ("base64", "3.5.0"); ("stringext", "1.6.0");
   ("bigstringaf", "0.8.0"); ("cstruct", "6.0.1"); ("cppo", "1.6.8");
   ("ocaml-syntax-shims", "1.0.0"); ("ocplib-endian", "1.2");
   ("uutf", "1.0.2"); ("eqaf", "0.8"); ("angstrom", "0.15.0");
   ("dune-configurator", "2.9.1"); ("fmt", "0.9.0"); ("astring", "0.8.5");
   ("ocamlgraph", "2.0.0"); ("jsonm", "1.0.1"); ("digestif", "1.1.0");
   ("uri", "4.2.0"); ("lwt", "5.5.0"); ("logs", "0.7.0"); ("irmin", "2.2.0")]



let () = Runner.run
~n:2
~switches:[reverted;initial;call_by_need]
~context:[]
~pkgs:irmin
~log:Sys.argv.(1)