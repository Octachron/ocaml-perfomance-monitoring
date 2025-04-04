
let before = "5.3.0+occur_rec_profiling"
let after = "5.3.0+occur_rec_marking"

let simple =[
  "ocamlfind", "1.9.1";
  "num", "1.4";
  "zarith", "1.12"
]
let hard =
  [
"ocamlbuild", "0.16.1";
"uchar", "0.0.2";
"topkg", "1.0.8";
"uutf", "1.0.4";
"react", "1.2.2";
"dune", "3.17.2";
"trie", "1.0.0";
"result", "1.5";
"either", "1.0.0";
"csexp", "1.5.2";
"ocaml_intrinsics_kernel", "v0.17.1";
"sexplib0", "v0.17.0";
"mew", "0.1.0";
"xdg", "3.17.2";
"cppo", "1.8.0";
"mew_vi", "0.5.0";
"ocplib-endian", "1.2";
"dune-configurator", "3.17.2";
"containers", "3.15";
"lwt", "5.9.1";
"lwt_react", "1.2.0";
"logs", "0.8.0";
"base", "v0.17.1";
"uucp", "16.0.0";
"uuseg", "16.0.0";
"zed", "3.2.3";
"lambda-term", "3.3.2";
"utop", "2.15.0-1";
 "seq", "base";
  "re", "1.12.0";
  "tyxml", "4.6.0";
"fieldslib","v0.17.0";
"jane-street-headers","v0.17.0";
"ppx_derivers","1.2.1";
"stdlib-shims","0.3.0";
"variantslib","v0.17.0";
"stdio","v0.17.0";
"ocaml-compiler-libs","v0.17.0";
"typerep","v0.17.1";
"parsexp","v0.17.0";
"sexplib","v0.17.0";
"ppxlib","0.35.0";
"ppxlib_jane","v0.17.2";
"ppx_optcomp","v0.17.0";
"ppx_tydi","v0.17.0";
"ppx_disable_unused_warnings","v0.17.0";
"ppx_cold","v0.17.0";
"ppx_stable_witness","v0.17.0";
"ppx_here","v0.17.0";
"ppx_variants_conv","v0.17.0";
"ppx_typerep_conv","v0.17.0";
"ppx_stable","v0.17.0";
"ppx_ignore_instrumentation","v0.17.0";
"ppx_fixed_literal", "v0.17.0";
"ppx_fields_conv", "v0.17.0";
"ppx_pipebang", "v0.17.0";
"ppx_enumerate", "v0.17.0";
"ppx_globalize", "v0.17.0";
"ppx_compare", "v0.17.0";
"ppx_optional", "v0.17.0";
"ppx_sexp_conv", "v0.17.0";
"ppx_let", "v0.17.0";
"ppx_hash", "v0.17.0";
"ppx_assert", "v0.17.0";
"ppx_sexp_value", "v0.17.0";
"ppx_sexp_message", "v0.17.0";
"ppx_custom_printf", "v0.17.0";
"ppx_base", "v0.17.0";
"capitalization", "v0.17.0";
"jst-config", "v0.17.0";
"ppx_string", "v0.17.0";
"bin_prot", "v0.17.0";
"time_now", "v0.17.0";
"ppx_string_conv", "v0.17.0";
"ppx_bin_prot", "v0.17.0";
"ppx_module_timer", "v0.17.0";
"ppx_inline_test", "v0.17.0";
"ppx_bench", "v0.17.0";
"ppx_expect", "v0.17.2";
"splittable_random", "v0.17.0";
"ppx_log", "v0.17.0";
"base_quickcheck", "v0.17.0";
"ppx_jane", "v0.17.0";
"uopt", "v0.17.0";
"gel", "v0.17.0";
"int_repr", "v0.17.0";
"base_bigstring", "v0.17.0";
"ppx_diff", "v0.17.0";
"core", "v0.17.1";
"janestreet_lru_cache", "v0.17.0";
"core_kernel", "v0.17.0";
"incremental", "v0.17.0";

]
let () = Runner.run
 ~log:"occur_rec.log"
 ~n:3
 ~slices:["typing"; "occur_rec"]
 ~retry:3
 ~with_filesize:false
 ~switches:[before;after]
 ~context:[]
 ~pkgs:hard
 ~status_file:"occur_rec.status"
