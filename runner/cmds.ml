let cmd fmt = Format.kasprintf (fun s ->
    Format.printf "Running: %s@." s;
    Sys.command s) fmt

let putenv_fmt key fmt =
  Format.kasprintf (Unix.putenv key) fmt

let state () = OpamGlobalState.load `Lock_write

let () =
  OpamClientConfig.r := OpamClientConfig.(set !r) ~keep_build_dir:true  ()


let set_switch opam sw =
  OpamSwitchCommand.switch `Lock_write opam sw


type 'a switch = { switch:OpamSwitch.t; state:'a OpamStateTypes.switch_state }


let (<!>) n err =
  if n = 0 then () else (err Fmt.stderr ; exit n)


let setup_env s = cmd "eval $(opam env --switch=%s --set-switch)" s
let update_env s = cmd "eval $(opam env --switch=%s  --set-switch)" s



let with_switch switch_name f =
  let switch = OpamSwitch.of_string switch_name in
  OpamGlobalState.with_ `Lock_write (fun opam ->
      OpamRepositoryState.with_ `Lock_write opam (fun repos ->
          let state = OpamSwitchState.load `Lock_write opam repos switch in
          setup_env switch_name  <!> Format.dprintf "opam env failure";
          f { switch; state }
        )
    )

let reinstall ~switch ~pkg =
  let state = OpamClient.reinstall_t switch.state ~ask:false ~assume_built:false ~force:true [pkg] in
  { switch with state }

let remove ~switch ~pkg =
  let state = OpamClient.remove switch.state ~autoremove:false ~force:true [pkg] in
  { switch with state }


let install ~switch ~pkg =
  let state =
    let atoms = OpamSolution.sanitize_atom_list ~permissive:true switch.state [pkg] in
    OpamClient.install_t switch.state ~ask:false
    ~deps_only:false ~assume_built:false
    atoms None
  in
  { switch with state }

let setup_dump_dir dir =   putenv_fmt "OCAMLPARAM" ",_,timings=1,dump-dir=%s" dir


let execute ~dir ~switch ~pkg =
  setup_dump_dir dir;
  update_env (OpamSwitch.to_string switch.switch) <!> Format.dprintf "Environment update failure";
  reinstall ~switch ~pkg
