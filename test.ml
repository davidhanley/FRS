open OUnit2
open Scoring

let test_split() =
  let ss = split_on_commas ",dave,12,M" in
  let e n = List.nth ss n in
  assert ((List.length ss) = 4)

let ar = { name = "dave" ; sex = M ; age = Some 49 ; foreign = false}

let parsed = line_to_athlete_row ",david hanley,49,M,silly"

let test1 () =
  match parsed with
    | Some(pr) -> assert (pr.name = "david hanley");
                  assert (pr.sex = M);
                  assert (pr.age = Some 49)
    | None -> assert false

let test_gender () =
  assert ((string_to_gender_and_foreign "*M") = (true, Some M));
  assert ((string_to_gender_and_foreign "*Female") = (true, Some F));
  assert ((string_to_gender_and_foreign "f") = (false, Some F));
  assert ((string_to_gender_and_foreign "m") = (false, Some M))

let () = test1();
         test_split();
         test_gender()



