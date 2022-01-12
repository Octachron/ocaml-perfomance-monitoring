type _ with_std =
  | No: float with_std
  | Yes: (float * float) with_std


type 'a info = { kind:'a with_std; name:string; title:string }

type ('a,'i) named = { info:'a info; f: 'i -> 'a Seq.t option }
type 'i any = Any: ('a,'i) named -> 'i any  [@@unboxed]


let remove_std (Any p) =
  let f (type a i) (p: (a,i) named) x: float Seq.t option =
    match p.info.kind, p.f x with
    | No, x -> x
    | Yes, None -> None
    | Yes, Some m -> Some (Seq.map fst m)
  in
  { info = { kind=No; name = p.info.name; title =""}; f = f p}


let rec seq_exists pred s = match s () with
  | Seq.Nil -> false
  | Seq.Cons(x,r) -> pred x || seq_exists pred r

let ln anp =
  let ln f x = Option.bind (f x) (fun x ->
      if seq_exists (fun x -> x <= 0.) x then None else Some (Seq.map Float.log x)
    ) in
  let s = remove_std anp in
  let info = { kind=No; name = "log_" ^ s.info.name; title=""} in
   {info; f = ln s.f }
