
type metadata = { switch:string; pkg:string }

type ('k,'v) point = { origin:metadata; key:'k; value:'v }

type file = { pkg:string; name:string }

type time_slice = { name:string; time:float }

let rec (.!()) l x = match l with
  | [] -> raise Not_found
  | a :: q  -> if a.name = x then a.time else q.!(x)

let typing x = x.!("typing")
let total = function
  | [] -> raise Not_found
  | h :: _ -> h.time
type times = time_slice list

type file_type =
  | Cmi
  | Cmo
  | Cmx
  | Cmt
  | Cmti


type filesize = { kind:file_type; size:int }

let rec slice_child names children =
  match names with
  | [] -> []
  | name :: rest ->
    match List.find_opt (fun child -> child.Parse.name = name) children with
    | None -> []
    | Some child ->
      { name; time=child.time } :: slice_child rest child.children


let times ~slices ~switch ~pkg l  =
  let origin = { switch; pkg } in
  let time_slice (parse:Parse.entry) =
    let value = { time=parse.time; name = parse.name } :: slice_child slices parse.children in
    { origin; key = parse.name; value }
  in
  List.map time_slice l


let pp_slice ppf s = Fmt.pf ppf "%s %.3g" s.name s.time

let pp_times ppf l =
  let sep ppf () = Fmt.pf ppf " " in
  Fmt.(list ~sep pp_slice) ppf l

let file_type_extension = function
  | Cmi -> "cmi"
  | Cmo -> "cmo"
  | Cmt -> "cmt"
  | Cmti -> "cmti"
  | Cmx -> "cmx"

let classify_file_type = function
  | "cmi" -> Some Cmi
  | "cmo" -> Some Cmo
  | "cmt" -> Some Cmt
  | "cmx" -> Some Cmx
  | "cmti" -> Some Cmti
  | _ -> None

type entry_type =
  | File_size of (string,  filesize) point
  | Compilation_profile of (string, times) point
