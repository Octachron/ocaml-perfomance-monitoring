module type core = sig
  type 'a t
  type index
  val dim: _ t -> int
  val (.%()): 'a t -> index -> 'a
  val init: (index -> 'a) -> 'a t
  val indices: index Seq.t
  val to_seq: 'a t -> 'a Seq.t
end

module type t = sig
  include core
  val map: ('a -> 'b) -> 'a t -> 'b t
  val exists: ('a -> bool) -> 'a t -> bool
  val for_all: ('a -> bool) -> 'a t -> bool
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> ' c t
  val fold_2: ('a -> 'b -> 'x ) -> ('acc -> 'x -> 'acc) -> 'acc -> 'a t -> 'b t -> 'acc
  val compare: cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
  val pp: 'a Fmt.t -> 'a t Fmt.t
end


module Add(X:core)(Y:core) = struct
  type 'a t = { left: 'a X.t; right: 'a Y.t}
  type index = Left of X.index | Right of Y.index
  let dim x = X.dim x.left + Y.dim x.right
  let (.%()) x = function
    | Left i -> x.left.X.%(i)
    | Right i -> x.right.Y.%(i)
  let init f =
    { left = X.init (fun i -> f (Left i)); right = Y.init (fun i -> f (Right i))}
  let indices =
    Seq.append
      (Seq.map (fun i -> Left i) X.indices)
      (Seq.map (fun i -> Right i) Y.indices)
  let to_seq x = Seq.append (X.to_seq x.left) (Y.to_seq x.right)
  let (+) left right = { left; right}
end


module Triple(X:core)(Y:core)(Z:core) = struct
  type 'a t = { x: 'a X.t; y: 'a Y.t; z: 'a Z.t}
  type index = X of X.index | Y of Y.index | Z of Z.index
  let dim x = X.dim x.x + Y.dim x.y + Z.dim x.z
  let (.%()) x = function
    | X i -> x.x.X.%(i)
    | Y i -> x.y.Y.%(i)
    | Z i -> x.z.Z.%(i)

  let init f =
    { x = X.init (fun i -> f (X i));
      y = Y.init (fun i -> f (Y i));
      z = Z.init (fun i -> f (Z i))
    }
  let indices =
    Seq.append
      (Seq.map (fun i -> X i) X.indices)
    @@
    Seq.append
      (Seq.map (fun i -> Y i) Y.indices)
      (Seq.map (fun i -> Z i) Z.indices)

  let to_seq x = Seq.append (X.to_seq x.x) @@ Seq.append (Y.to_seq x.y) (Z.to_seq x.z)
  let cat x y z = { x; y; z }
end

module Expand(A:core)
= struct
  open A
  let map f a = init (fun i -> f a.%(i) )
  let exists pred a =
    let rec search indices = match indices () with
      | Seq.Nil -> false
      | Seq.Cons(x,rest) -> pred a.%(x) || search rest
    in
    search indices
  let for_all pred a =
    let rec search indices = match indices () with
      | Seq.Nil -> true
      | Seq.Cons(x,rest) -> pred a.%(x) && search rest
    in
    search indices
  let to_seq a = Seq.map (fun i -> a.%(i)) indices


  let rec combine f s1 s2 () = match s1 (), s2 () with
    | Seq.Nil, _ | _, Seq.Nil -> Seq.Nil
    | Seq.Cons(x,s1), Seq.Cons(y,s2) -> Seq.Cons(f x y, combine f s1 s2)

  let fold_2 zip r acc x y =
    Seq.fold_left (fun acc x -> r acc x) acc (combine zip (to_seq x) (to_seq y))

  let compare ~cmp x y =
    let rec compare cmp x y indices = match indices () with
      | Seq.Nil -> 0
      | Seq.Cons(pos, indices) ->
        let r = cmp x.%(pos) y.%(pos) in
        if r = 0 then compare cmp x y indices else r
    in
    compare cmp x y A.indices
  let map2 f x y= A.init (fun index -> f x.%(index) y.%(index) )
  let pp pp ppf x =
    let space ppf () = Fmt.pf ppf " " in
    Fmt.(seq ~sep:space pp) ppf (to_seq x)
end

module As_vec(V:Vec.t)(A:t): Vec.t with type t = V.t A.t =
struct
  open V
  type t = V.t A.t
  let ( + ) x y = A.map2 (+) x y
  let ( - ) x y = A.map2 (-) x y
  let (|*|)  x y = A.fold_2 V.(|*|) (+.) 0. x y
  let ( *. ) l x = A.map ( ( *. ) l ) x
  let ( /. ) x l = A.map (fun x -> x /. l) x
  let zero = A.init (fun _ -> V.zero)
  let compare x y =
    A.compare ~cmp:V.compare x y 
  let pp ppf x = A.pp V.pp ppf x
end
