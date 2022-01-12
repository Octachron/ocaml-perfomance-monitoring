module type t = sig
  type t
  val zero: t
  val ( + ): t -> t -> t
  val ( - ): t -> t -> t
  val (  *. ): float -> t -> t
  val (  /. ): t -> float -> t
  val ( |*| ): t -> t -> float
  val compare: t -> t -> int
  val pp: Format.formatter -> t -> unit
end

module Float = struct
  let ( *. ) = ( *. )
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( /. ) = ( /. )
  let (|*|) x y = x *. y
  let zero = 0.
  type t = float
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp = Fmt.float
end
