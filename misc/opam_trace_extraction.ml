let try_format fmt s =
  match
    Scanf.sscanf s fmt (fun n v -> n, v)
  with
  | x -> Some x
  | exception Scanf.Scan_failure _ -> None

let rec try_formats s = function
  | [] -> None
  | fmt :: rest ->
    match try_format fmt s with
    | None -> try_formats s rest
    | Some _ as r -> r

let line ch =
  try_formats (input_line ch) [
    "%_s installed %s@.%s";
    "%_s installed %s@.%s %_s"
  ]

let rec all ch pkgs =
  match line ch with
  | exception End_of_file -> List.rev pkgs
  | None -> all ch pkgs
  | Some pkg -> all ch (pkg::pkgs)

let main () =
  let log = open_in Sys.argv.(1) in
  let pkgs = all log [] in
  Fmt.pr "@[<v>%a@]@." Fmt.Dump.(list (pair string string)) pkgs

let () = main ()
