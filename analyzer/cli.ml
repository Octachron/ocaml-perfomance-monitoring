

module Observable_reader(O:Stat.observable with type sample = Data.entry_type) = struct
  let read db (x:Data.entry_type) =
    let switch = match x with
      | Compilation_profile x -> x.origin.switch
      | File_size x -> x.origin.switch
    in
    let switch_stat =
      match Types.Db.find switch db with
      | x -> x
      | exception Not_found -> O.empty
    in
    Types.Db.add switch (O.add x switch_stat) db
end


module Time_reader = Observable_reader(Stat.Ls)
module Size_reader = Observable_reader(Stat.Fs)


let read_log log_seq =
  Seq.fold_left Time_reader.read Types.Db.empty log_seq



let split n l =
  let rec split left n right =
  if n = 0 then left, right else
    match right with
    | [] -> left, right
    | a :: right -> split (a::left) (n-1) right
  in
  split [] n l

let _reject_outliers proj excl l =
  let l = List.sort (fun x y -> compare (proj y) (proj x)) l in
  let _ ,r = split excl l in
  r

let log_files: string list ref = ref []
let data_dir = ref None

let set r = Arg.String (fun x -> r := Some x)
let add_item l = Arg.String (fun x -> l := x :: !l)
let anon x = log_files := x :: !log_files

let log =  "-log" , add_item log_files, "read data from file"
let dir =  "-output-dir", set data_dir, "output dir"

let read_logs logs =
  List.fold_left (fun s x -> Seq.append (Log.read x) s) (fun () -> Seq.Nil) logs
