open OUnit2
open Scoring

let test_split() =
  let ss = split_on_commas ",dave,12,M" in
  let e n = List.nth ss n in
  assert ((List.length ss) = 4)



let ar = { name = "dave" ; sex = M ; age = Some 49 }

let parsed = line_to_athlete_row ",dave hanley,49,M,silly"
                                 (* "1,WAI CHING SOH,,M,10:44:00 AM" *)
let test1 () =
  match parsed with
    | Some(pr) -> assert (pr.name = "dave hanley");
                  assert (pr.sex = M);
                  assert (pr.age = Some 49)
    | None -> assert false


let () = test1();
         test_split()


