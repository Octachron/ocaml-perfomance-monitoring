let fmt: _ format6 = "%s %s %s %g %g"

let write_entry ppf (x:Data.typechecking_stat) =
  Fmt.pf ppf fmt x.switch x.pkg x.subpart x.time x.total_time;
  Fmt.pf ppf "@."

let scan_entry s =
  Scanf.sscanf s fmt (fun switch pkg subpart time total_time -> { Data.switch; pkg; subpart; time; total_time })

let write_many ppf x = List.iter (write_entry ppf) x

let read filename =
  let x = open_in filename in
  let closed = ref false in
  let rec read () = if !closed then Seq.Nil else
      match input_line x with
      | exception End_of_file ->
        close_in x; closed:=false;
        Seq.Nil
      | s -> Seq.Cons (scan_entry s, read)
in read
