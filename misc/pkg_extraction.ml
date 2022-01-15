
let try_format fmt s =
  match
    Scanf.sscanf s fmt (fun v -> v)
  with
  | x ->
    if String.contains s '\146' then None
    else if String.contains s '\145' then Some x
    else None
  | exception Scanf.Scan_failure _ -> None



let rec try_formats s = function
  | [] -> None
  | fmt :: rest ->
    match try_format fmt s with
    | None -> try_formats s rest
    | Some _ as r -> r

let line ch =
  try_formats (input_line ch) [
    {|%s@.%_s|};
  ]

let rec all ch pkgs =
  match line ch with
  | exception End_of_file -> List.rev pkgs
  | None -> all ch pkgs
  | Some pkg -> all ch (pkg::pkgs)

let main () =
  let log = open_in Sys.argv.(1) in
  let pkgs = all log [] in
  Fmt.pr "@[<v>%a@]@." Fmt.(list string) pkgs

let () = main ()
