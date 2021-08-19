
type 'k typechecking_stat = { switch:string; key:'k; time:float; total_time:float }

type file = { pkg:string; name:string }

let typechecking_times ~switch ~pkg l  =
  let typechecking {Parse.name; time; children} =
    match List.find_opt (fun child -> child.Parse.name = "typing") children with
    | None -> { switch; key={pkg; name};  time=0.; total_time=time}
    | Some child -> { switch; key={pkg;name}; time=child.time; total_time = time}
  in
  List.map typechecking l


type times = { typechecking: float; total: float }

let pp_times ppf {typechecking; total} =
  Fmt.pf ppf "%.3gs/%.3gs" typechecking total
