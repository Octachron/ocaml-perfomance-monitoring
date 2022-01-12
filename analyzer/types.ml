module File_key = struct
  type t = Data.file = { pkg: string; name: string }
  let compare: t -> t -> int = Stdlib.compare
end
module By_files = Map.Make(File_key)
module By_pkg = Map.Make(String)

module Db = Map.Make(String)

module Input = struct
  type 'a t = { key:File_key.t; data: 'a}
  let map f x  = { x with data = f x.data }
  let make (key,data) = { key;data }
end

type 'a input = 'a Input.t = { key:File_key.t; data: 'a}
let input = Input.make
