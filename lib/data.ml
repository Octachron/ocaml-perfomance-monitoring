type typechecking_stat = { switch:string; pkg: string; subpart: string; time:float; total_time:float }

let typechecking_times ~switch ~pkg l  =
  let typechecking {Parse.name; time; children} =
    match List.find_opt (fun child -> child.Parse.name = "typing") children with
    | None -> { switch; pkg; subpart=name;  time=0.; total_time=time}
    | Some child -> { switch; pkg; subpart=name; time=child.time; total_time = time}
  in
  List.map typechecking l


type times = { typechecking: float; total: float }

let pp_times ppf {typechecking; total} =
  Fmt.pf ppf "%.3gs/%.3gs" typechecking total
