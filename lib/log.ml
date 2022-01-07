let profile_fmt: _ format6  = "Profile %s %s %s %g %g"
let filesize_fmt: _ format6 = "Filesize %s %s %s %s %d"


let write_entry ppf = function
  | Data.Compilation_profile x ->
    Fmt.pf ppf profile_fmt x.origin.switch x.origin.pkg x.key x.value.typechecking x.value.total;
    Fmt.pf ppf "@."
  | Data.File_size x ->
    Fmt.pf ppf filesize_fmt x.origin.switch x.origin.pkg x.key (Data.file_type_extension x.value.kind) x.value.size;
    Fmt.pf ppf "@."


let scan_entry s =
  try Scanf.sscanf s profile_fmt (fun switch pkg name typechecking total ->
      Data.Compilation_profile { Data.origin={switch; pkg }; key = name; value={typechecking;total} }
    )
  with Scanf.Scan_failure  _ ->
    Scanf.sscanf s filesize_fmt (fun switch pkg name kind size ->
        let kind = match Data.classify_file_type kind with
          | Some x -> x
          | None -> failwith "format error: unknown file kind"
        in
      Data.File_size { Data.origin={switch; pkg }; key = name; value={kind;size} }
    )


let write_many ppf x = List.iter (write_entry ppf) x

let read filename =
  let x = open_in filename in
  let closed = ref false in
  let rec read () = if !closed then Seq.Nil else
      match input_line x with
      | exception End_of_file ->
        close_in x; closed:=false;
        Seq.Nil
      | s -> Seq.Cons (scan_entry s, read)
in read
