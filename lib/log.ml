let profile_fmt: _ format6  = "Profile %s %s %a"
let profile_scan: _ format6 = "Profile %s %s %s"
let filesize_fmt: _ format6 = "Filesize %s %s %s %s %d"


let write_entry ppf = function
  | Data.Compilation_profile x ->
    Fmt.pf ppf profile_fmt x.origin.switch x.origin.pkg Data.pp_times x.value;
    Fmt.pf ppf "@."
  | Data.File_size x ->
    Fmt.pf ppf filesize_fmt x.origin.switch x.origin.pkg x.key (Data.file_type_extension x.value.kind) x.value.size;
    Fmt.pf ppf "@."

let parse_column (x,y) =
  {Data.name=x; time = float_of_string y}

let rec pair = function
  | x :: y :: z -> (x,y) :: pair z
  | [s] -> Format.eprintf "Parsing unpaired name: %s@."s ; exit 2
  | [] -> []

let parse_profile s =
  let s = String.split_on_char ' ' s in
  match s with
  | "Profile" :: switch :: pkg :: (key :: t as rest) ->
    begin match t with
      | [] ->
        Some Data.(
            Compilation_profile { origin = {switch;pkg}; key; value=[]}
          )
      | _ ->
        let value = List.map parse_column (pair rest) in
        Some Data.(
            Compilation_profile { origin = {switch;pkg}; key; value}
          )
    end
  | _ -> None


let scan_entry s =
  match parse_profile s with
  | Some x -> x
  | None ->
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
