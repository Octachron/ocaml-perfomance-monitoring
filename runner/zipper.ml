
include struct
  open Ppx_yojson_conv_lib.Yojson_conv
module Pkg = struct
  type t = Pkg.t = { name:string; version:string }
  [@@deriving yojson]
end

type switch = string
[@@deriving yojson]


type sample = { switch:string; pkg:Pkg.t; size:int }
[@@deriving yojson]

type ('a,'b) zipper = {
  sampled:'a list;
  pending:'b;
  todo:'a list;
}
  [@@deriving yojson]


type t = {
  switches: switch list;
  retry:int;
  log:string;
  with_filesize:bool;
  slices: string list;
  pkgs: (Pkg.t,pkg) zipper
}
and pkg = (switch, variant) zipper
and variant = { sampled:int; sample:sample }
[@@deriving yojson]
end

type ('a,'b) status =
  | Ongoing of 'a
  | Done of 'b

module Variant = struct
  let pkg x = x.sample.pkg
  let switch x = x.sample.switch
  let sample_size x = x.sample.size
  let sample x = x.sample
  let start sample = { sampled=0; sample }
  let iter f z = f z.sample
  let next z =
    if z.sampled = z.sample.size then
      Done z.sample
    else
      Ongoing { z with sampled = z.sampled + 1 }
end

module Pkgz = struct
  let pkg x = Variant.pkg x.pending
  let switch x = Variant.switch x.pending
  let sample_size x = Variant.sample_size x.pending
  let sample x = Variant.sample x.pending
  let start  ~switches ~sample_size pkg =
    match switches with
    | [] -> assert false
    | switch :: todo ->
      {
        sampled=[];
        pending = Variant.start { switch; size=sample_size; pkg  } ;
        todo;
      }
  let iter f z = Option.iter (Variant.iter f) z.pending

  let rec next (z:pkg) =
    match Variant.next z.pending with
    | Ongoing pending ->
      Ongoing { z with pending }
    | Done new_sample ->
      let pkg = pkg z in
      match z.todo with
      | [] -> Done pkg
      | switch :: todo ->
        let sampled = new_sample.switch :: z.sampled in
        let pending = Variant.start  {size=sample_size z; switch; pkg} in
        next { pending; todo; sampled }

end


module Main = struct

  let pkg x = Pkgz.pkg x.pkgs.pending
  let switch x = Pkgz.switch x.pkgs.pending
  let switches z = z.switches
  let slices z = z.slices
  let sample_size x = Pkgz.sample_size x.pkgs.pending
  let sample x = Pkgz.sample x.pkgs.pending

  let iter f z =
    Option.iter (Pkgz.iter f) z.pending

  let start ~retry ~log ~with_filesize ~slices ~switches ~pkgs ~sample_size =
    match pkgs with
    | [] -> assert false
    | pkg :: todo ->
      let pkgs = {
        sampled = [];
        pending = Pkgz.start ~switches ~sample_size pkg;
        todo;
      }
      in
      { pkgs; switches; with_filesize; retry; log; slices }

  let rec next (z:t) =
    let pkgs = z.pkgs in
    match Pkgz.next pkgs.pending with
    | Ongoing pending ->
      let pkgs = { pkgs with pending } in
      Ongoing { z with pkgs }
    | Done new_sample ->
      match pkgs.todo with
      | [] -> Done (new_sample :: pkgs.sampled)
      | pending :: todo ->
        let pending =
          Pkgz.start ~sample_size:(sample_size z) ~switches:z.switches pending
        in
        let pkgs = {
          sampled = new_sample :: pkgs.sampled;
          pending;
          todo;
        } in
        next {
          z with pkgs
        }

  let pkgs z = z.pkgs.sampled @ pkg z :: z.pkgs.todo
end

let switches = Main.switches
let pkgs = Main.pkgs
let slices = Main.slices
let with_filesize z = z.with_filesize

let status_file ~file (z:t) =
  Yojson.Safe.to_file file (yojson_of_t z)


let start = Main.start

let tracked_iter ~status_file:file f z  =
  let apply z =
    status_file ~file z;
    f (Main.sample z)
  in
  let rec next z =
    apply z;
    match Main.next z with
    | Done _ -> ()
    | Ongoing z -> next z
  in
  next z
