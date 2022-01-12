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


module R3 = struct
  type scalar = float
  type t = {x:scalar; y:scalar; z:scalar}
  let ( + ) u v= { x=u.x +. v.x; y = u.y +. v.y; z = u.z +. v.z }
  let ( - ) u v= { x=u.x -. v.x; y = u.y -. v.y; z = u.z -. v.z }
  let zero = { x =0.; y = 0.; z = 0. }
  let (|*|) u v = u.x *. v.x +. u.y *. v.y +. u.z *. v.z
  let ( *. ) l u = { x = l *. u.x; y = l *. u.y; z = l *. u.z }
  let ( /.  ) u l = { x = u.x /. l; y = u.y /. l; z = u.z /. l }
  let compare (x:t) (y:t) = Stdlib.compare x y
  let pp ppf v = Fmt.pf ppf "(%g %g %g)" v.x v.y v.z
end
