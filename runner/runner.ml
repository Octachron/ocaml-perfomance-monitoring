module Pkg = Cmds.Pkg

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
   uuid @@ (switch ^ "-" ^ Pkg.full pkg)


let rec is_prefix_until prefix s len pos =
  pos >= len ||
  (
    prefix.[pos] = s.[pos]
    && is_prefix_until prefix s len (pos + 1)
  )

let is_prefix ~prefix s = is_prefix_until prefix s (String.length prefix) 0


let file_size ~pkg ~switch ~filename kind s =
  Data.File_size {
    origin={pkg;switch};
    key=filename;
    value = { Data.kind; size= (Unix.stat s).st_size}
  }

let rec crawl_dir ~switch ~pkg dir =
  Seq.concat @@ Seq.map (crawl_file ~switch ~pkg dir) @@ Array.to_seq (Sys.readdir dir)
and crawl_file ~switch ~pkg dir filename =
  let path = Filename.concat dir filename in
  if Sys.is_directory path then
    crawl_dir ~switch ~pkg path
  else
    match Filename.extension filename with
    | ".cmt" -> Seq.return (file_size ~filename ~pkg ~switch Cmt path)
    | ".cmo" -> Seq.return (file_size ~filename ~pkg ~switch Cmo path)
    | ".cmx" -> Seq.return (file_size ~filename  ~pkg ~switch  Cmx path)
    | ".cmi" -> Seq.return (file_size ~filename  ~pkg ~switch Cmi path)
    | ".cmti" -> Seq.return (file_size ~filename  ~pkg ~switch Cmti path)
    | _ -> Seq.empty



let read_result ~build_dir  ~switch ~pkg ~dir =
  let files = Array.to_list @@ Sys.readdir dir in
  let read_file filename =
    if is_prefix ~prefix:"profile" (Filename.basename filename) then
      Some (Parse.profile (Filename.concat dir filename))
    else
      None
  in
  let split_timings l = Seq.map (fun x -> Data.Compilation_profile x) (List.to_seq l) in
  let timings =
    files
    |> List.to_seq
    |> Seq.filter_map read_file
    |> Seq.map (Data.typechecking_times ~switch ~pkg)
    |> Seq.concat_map split_timings
  in
  let sizes =
    crawl_dir ~switch ~pkg build_dir
  in
  Seq.append timings sizes

let (<!>) = Cmds.(<!>)

let sample ~log ~switch ~pkg =
  let dir = pkg_dir ~switch ~pkg in
  let () =  Sys.mkdir dir 0o777 in
  Cmds.execute ~switch ~pkg ~dir <!> Format.dprintf "Failed to install %s" (Pkg.full pkg);
  let build_dir = Cmds.opam_var ~switch ~pkg "build" in
  Seq.iter (Log.write_entry log) (read_result ~switch ~build_dir ~dir ~pkg:(Pkg.full pkg))

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
    List.iter (fun pkg ->
        Cmds.install ~switch ~pkg
        <!> Format.dprintf "Installation failure: %s/%s" switch (Pkg.full pkg)
      ) pkgs
  ) switches

let start ~n ~switches ~log ~context ~pkgs =
  let () = install_context ~switches ~pkgs:context in
  let experiment switch = pkg_line ~switch ~log n pkgs in
  List.iter experiment switches

let with_file filename f =
  let x = open_out filename in
  let ppf = Format.formatter_of_out_channel x in
  Fun.protect (fun () -> f ppf)
    ~finally:(fun () -> close_out x)

let run ~n ~log ~switches ~context ~pkgs =
  with_file log (fun log ->
      start ~log
        ~n
        ~switches
        ~context
        ~pkgs
    )

