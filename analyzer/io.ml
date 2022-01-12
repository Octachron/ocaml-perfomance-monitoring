
let out_name ?dir fmt = match dir with
  | None -> Fmt.str fmt
  | Some dir -> Fmt.kstr (fun x -> Filename.concat dir x) fmt
