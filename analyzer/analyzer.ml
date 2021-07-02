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
    let m = Stat.simplify ref_times times in
    Stat.save  "by_files.data"  m

let before = "Octachron-ocaml-before-pr10337+dump-dir"
let after = "Octachron-ocaml-pr10337+dump-dir"

let () =
  let log_name = "complex.log" in
  let log_seq = Log.read log_name in
  let log = read_log log_seq in
  comparison ~before ~after log
