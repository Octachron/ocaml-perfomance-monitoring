
let from_branch name = Format.asprintf "Octachron-ocaml-%s" name

let reverted = from_branch "shape_reverted"

let simple = [ "ocamlfind"; "num" ]

let () = Runner.run
~n:2
~switches:[reverted]
~context:[]
~pkgs:simple
~log:"shape_test"
