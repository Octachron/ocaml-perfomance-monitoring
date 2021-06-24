type entry = { name:string; time:float; children: entry list}

let rec pp_entries ppf l =
  Fmt.(Dump.list pp_entry) ppf l
and pp_entry ppf {name; time; children} =
  Fmt.pf ppf "@[{name=%s;@ time=%f;@ children=@,%a}@]"
    name time pp_entries children



let rec fold_other l = match l with
  |  a :: { name = "other"; time; children = [] } :: q ->
    fold_other ({ a with time = a.time +. time } :: q)
  | [{name="other";_} ] -> []
  | [] -> []
  | a :: q ->
     if (a.name = "other") then raise Exit;
     a :: fold_other q
let fold_other  l =
  try fold_other l with
  | Exit ->
    pp_entries Fmt.stderr l;
    exit 2


let rec indentation l pos =
  if pos < String.length l && l.[pos] = ' ' then
    indentation l (pos+1)
  else
    pos
let indentation l = indentation l 0

let read_lines filename =
  let chan = open_in filename in
  let rec read_lines l =
    match input_line chan with
    | exception End_of_file -> List.rev l
    | newline -> read_lines (newline::l)
  in
  let r = read_lines [] in
  close_in chan;
  r


let scan_entry s = Scanf.sscanf s " %fs %s" (fun time name -> name, time )

let rec parse_group parent_indent l =
  match l with
  | [] -> [], []
  | line :: rest as full ->
    let indent = indentation line in
    if indent <= parent_indent then
      [], full
    else
      let name, time = scan_entry line in
      let children, rest = parse_group indent rest in
      let entry = { name; time; children } in
      let entries, rest =parse_group parent_indent rest in
      entry :: entries, rest

let profile filename =
  let lines = read_lines filename in
  let group, rest = parse_group (-1) lines in
  assert (rest = []);
  fold_other group
