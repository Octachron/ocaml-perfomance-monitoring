
type metadata = { switch:string; pkg:string }

type ('k,'v) point = { origin:metadata; key:'k; value:'v }

type file = { pkg:string; name:string }
type times = { typechecking: float; total: float }

type file_type =
  | Cmi
  | Cmo
  | Cmx
  | Cmt
  | Cmti

type filesize = { kind:file_type; size:int }

let typechecking_times ~switch ~pkg l  =
  let origin = { switch; pkg } in
  let typechecking {Parse.name; time; children} =
    match List.find_opt (fun child -> child.Parse.name = "typing") children with
    | None -> { origin; key=name;  value = { typechecking=0.; total=time} }
    | Some child -> { origin; key=name; value = { typechecking=child.time; total = time} }
  in
  List.map typechecking l


let pp_times ppf {typechecking; total} =
  Fmt.pf ppf "%.3gs/%.3gs" typechecking total



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
