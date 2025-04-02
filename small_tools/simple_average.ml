module P = Parse

let rec process_profile name (p:P.entry) =
  if p.name = name then p.time
   else process_profiles name p.children
and process_profiles name ch =
  List.fold_left (fun acc x -> acc +. process_profile name x) 0. ch

let process_file name f =
  let x = Parse.profile f in
  process_profiles name x

let main () =
  let column = ref "" in
  let time = ref 0. in
  let non_zero_time = ref 0. in
  let non_zero_count = ref 0 in
  let count = ref 0 in
  let anon f =
    let p = process_file !column f in
    count := !count + 1;
    time := !time +. p;
    if p > 1e-6 then begin
      incr non_zero_count;
      non_zero_time := p +. !non_zero_time
    end
  in
  let () =
  Arg.parse
    ["-column", Arg.Set_string column, "<name> average the <name> column"]
    anon
    "simple_average -column <name> file1 ... file n"
  in
  let avg = !time /. float !count in
  let navg = !non_zero_time /. float !non_zero_count in
  Format.printf "@[Average %s time: %f/non-zero %f (%n/%d sample)@]@."
    !column avg  navg !count !non_zero_count

let () = main ()
