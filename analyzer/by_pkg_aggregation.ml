module By_pkg = Types.By_pkg

let count seq =
  let by_pkg = By_pkg.empty in
  let add key _data m =
    let key = key.Data.pkg in
    let value =
      match By_pkg.find_opt key m with
      | None -> 1
      | Some x -> 1 + x
    in
    By_pkg.add key value m
  in
  let m = Seq.fold_left (fun m {Types.key;data} -> add key data m) by_pkg seq in
  let rec cumulative s seq () = match seq () with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons( (key,x), r ) ->
      let s = x + s in
      Seq.Cons( (key,x,s), cumulative s r )
  in
  Array.of_seq (cumulative 0 @@ By_pkg.to_seq m)

let global_count seq ppf =
  let a = count seq in
  let pp ppf (key,x,s) = Fmt.pf ppf "%s %d %d@." key x s in
  Array.iter (pp ppf) a
