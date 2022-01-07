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

let pp_pkg ppf (name,version) = match version with
  | None -> Format.fprintf ppf "%s" (OpamPackage.Name.to_string name)
  | Some v -> Format.fprintf ppf "%s.%s" (OpamPackage.Name.to_string name) (OpamPackage.Version.to_string v)


type 'a switch = 'a Cmds.switch = { switch:OpamSwitch.t; state:'a OpamStateTypes.switch_state }


let pkg_dir ~switch ~pkg =
  let pkg_name = OpamPackage.Name.to_string (fst pkg) in
  let switch_name = OpamSwitch.to_string switch.switch in
   uuid @@ (switch_name ^ "-" ^ pkg_name)

let pkg_name (name,_v) = OpamPackage.Name.to_string name


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
  let switch_name=OpamSwitch.to_string switch.switch in
  files
  |> List.to_seq
  |> Seq.filter_map read_file
  |> Seq.map (Data.typechecking_times ~switch:switch_name ~pkg:(pkg_name pkg))

let (<!>) = Cmds.(<!>)

let sample ~log ~switch ~pkg =
  let dir = pkg_dir ~switch ~pkg in
  let () =  Sys.mkdir dir 0o777 in
  let switch = Cmds.execute ~switch ~pkg ~dir in
  Seq.iter (Log.write_many log) (read_result ~switch ~dir ~pkg);
  switch

let rec multisample n ~log ~switch ~pkg =
  if n = 0 then switch else
    begin
      let switch = sample ~log ~switch ~pkg in
      multisample (n-1) ~log  ~pkg ~switch
    end

let pkg_line n ~log ~switch pkgs  =
  let switch = List.fold_left (fun switch pkg -> Cmds.remove ~pkg ~switch) switch (List.rev pkgs) in
  List.fold_left  (fun switch pkg ->
      multisample n ~log ~switch ~pkg
    ) switch pkgs

let install_context ~switch ~pkgs =
  List.fold_left (fun switch pkg -> Cmds.install ~switch ~pkg) switch pkgs

let start ~n ~switches ~log ~context ~pkgs =
  let experiment switch =
    Cmds.with_switch switch (fun switch ->
        let switch = install_context ~switch ~pkgs:context in
        ignore @@ pkg_line ~switch ~log n pkgs
      )
  in
  List.iter experiment switches

let with_file filename f =
  let x = open_out filename in
  let ppf = Format.formatter_of_out_channel x in
  Fun.protect (fun () -> f ppf)
    ~finally:(fun () -> close_out x)

let run ~n ~log ~switches ~context ~pkgs =
  Cmds.setup_dump_dir "/tmp/shape_test_2";
  OpamClientConfig.opam_init ~keep_build_dir:true ~yes:(Some true) ~jobs:(lazy 1) ();
  with_file log (fun log ->
      start ~log
        ~n
        ~switches
        ~context
        ~pkgs
    )
