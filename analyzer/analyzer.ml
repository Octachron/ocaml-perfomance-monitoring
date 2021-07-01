let typechecking_proj = List.map (fun x -> x.Data.typechecking)
let nontypechecking_proj = List.map (fun (x:Data.times) -> x.total -. x.typechecking)

module Db = Map.Make(String)

let read_log_entry db (x:Data.typechecking_stat) =
  let switch_stat =
    match Db.find x.switch db with
    | x -> x
    | exception Not_found -> Stat.Ls.empty
  in
  Db.add x.switch (Stat.Ls.add x switch_stat) db

let read_log log_seq =
  Seq.fold_left read_log_entry Db.empty log_seq

let comparison ~before ~after db =
    let ref_times = Db.find before db in
    let times = Db.find after db in
    Stat.save typechecking_proj "ratio.data"  ~ref_times ~times;
    Stat.save nontypechecking_proj "witness.data"  ~ref_times ~times

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let () =
  let log_name = "log" in
  let log_seq = Log.read log_name in
  let log = read_log log_seq in
  comparison ~before ~after log
