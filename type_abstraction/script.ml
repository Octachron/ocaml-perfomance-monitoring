
let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let simple = List.map OpamFormula.atom_of_string [ "ocamlfind"; "num"; "zarith" ]
let hard =
  [ "ocamlfind"; "num"; "zarith"; "seq"; "containers"; "coq"; "dune"; "re";
    "ocamlbuild"; "uchar"; "topkg"; "uutf"; "tyxml";
    "sexplib0"; "base"
  ]

let () = Runner.run
 ~log:"type_abstraction"
 ~n:4
 ~switches:[before;after]
 ~context:[]
 ~pkgs:simple
