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
