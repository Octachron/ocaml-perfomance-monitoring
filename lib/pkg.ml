type t = {name:string; version:string}
let name pkg = pkg.name
let full pkg = Format.asprintf "%s.%s" pkg.name pkg.version
let make name version = {name;version}
