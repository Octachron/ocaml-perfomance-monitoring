let output_dir name = "/tmp/" ^ name

let uuid name =
  let rec loop n =
    let guess = output_dir (name ^ string_of_int n) in
    if not (Sys.file_exists guess) then guess
    else loop (n+1)
   in
  let guess = output_dir name in
  if not (Sys.file_exists guess) then guess else
  loop 2

let pkg_dir ~switch ~pkg =
   uuid @@ (switch ^ "-" ^ pkg)


let rec is_prefix_until prefix s len pos =
  pos >= len ||
  (
    prefix.[pos] = s.[pos]
    && is_prefix_until prefix s len (pos + 1)
  )

let is_prefix ~prefix s = is_prefix_until prefix s (String.length prefix) 0

let read_result  ~switch ~pkg ~dir =
  let files = Array.to_list @@ Sys.readdir dir in
  let read_file filename =
    if is_prefix ~prefix:"profile" (Filename.basename filename) then
      Some (Parse.profile (Filename.concat dir filename))
    else
      None
  in
  files
  |> List.to_seq
  |> Seq.filter_map read_file
  |> Seq.map (Data.typechecking_times ~switch ~pkg)

let (<!>) = Cmds.(<!>)

let sample ~log ~switch ~pkg =
  let dir = pkg_dir ~switch ~pkg in
  let () =  Sys.mkdir dir 0o777 in
  Cmds.execute ~switch ~pkg ~dir <!> Format.dprintf "Failed to install %s" pkg;
  Seq.iter (Log.write_many log) (read_result ~switch ~dir ~pkg)

let rec multisample n ~log ~switch ~pkg =
  if n = 0 then () else
    begin
      sample ~log ~switch ~pkg;
      multisample (n-1) ~log  ~pkg ~switch
    end

let pkg_line n ~log ~switch pkgs  =
  List.iter (Cmds.remove_pkg ~switch) (List.rev pkgs);
  List.iter  (fun pkg ->
      multisample n ~log ~switch ~pkg
    ) pkgs

let install_context ~switches ~pkgs = List.iter (fun switch ->
    List.iter (fun pkg -> Cmds.install ~switch ~pkg <!> Format.dprintf "Installation failure: %s/%s" switch pkg) pkgs
  ) switches

let start ~n ~switches ~log ~context ~pkgs =
  let () = install_context ~switches ~pkgs:context in
  let experiment switch = pkg_line ~switch ~log n pkgs in
  List.iter experiment switches

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let simple = [ "ocamlfind"; "num"; "zarith" ]
let hard =
  [ "ocamlfind"; "num"; "zarith"; "seq"; "containers"; "coq"; "dune"; "re";
    "ocamlbuild"; "uchar"; "topkg"; "uutf"; "tyxml";
    "sexplib0"; "base"
  ]

let with_file filename f =
  let x = open_out filename in
  let ppf = Format.formatter_of_out_channel x in
  Fun.protect (fun () -> f ppf)
    ~finally:(fun () -> close_out x)

let () =
  let log = "log" in
  with_file log (fun log ->
      start ~log
        ~n:4
        ~switches:[before;after]
        ~context:[]
        ~pkgs:simple
    )
