
let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let simple =[
  "ocamlfind", "1.9.1";
  "num", "1.4";
  "zarith", "1.12"
]
let hard =
  [ "ocamlfind"; "num"; "zarith"; "seq"; "containers"; "coq"; "dune"; "re";
    "ocamlbuild"; "uchar"; "topkg"; "uutf"; "tyxml";
    "sexplib0"; "base"
  ]

let () = Runner.run
 ~log:"type_abstraction"
 ~slices:["typing"]
 ~n:4
 ~retry:3
 ~switches:[before;after]
 ~context:[]
 ~pkgs:simple
 ~status_file:"ta.json"
