
let from_branch name = Format.asprintf "Octachron-ocaml-%s" name

let reverted = from_branch "shape_reverted"
let initial = from_branch "shape_pr10796_minimal"
let call_by_need = from_branch "shape_pr10825_strong_call_by_need"

let ref = reverted
let alts = [initial;call_by_need]
let all = ref :: alts
