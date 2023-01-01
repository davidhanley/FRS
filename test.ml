open OUnit2
open Scoring

let test_split() =
  let ss = split_on_commas ",dave,12,M" in
  assert ((List.length ss) = 4)





let test1 () =
  let parsed = line_to_athlete_row ",dave hanley,49,M,silly" in
  match parsed with
    | pr::_ -> 
               assert (pr.name = "DAVID HANLEY");
               assert (pr.sex = M);
               assert (pr.age = Some 49)
    | [] -> assert false

let test_gender () =
  assert ((string_to_gender_and_foreign "*M") = (true, Some M));
  assert ((string_to_gender_and_foreign "*Female") = (true, Some F));
  assert ((string_to_gender_and_foreign "f") = (false, Some F));
  assert ((string_to_gender_and_foreign "m") = (false, Some M))

let test_translate () =
  let tt = load_name_translator() in
  assert ((tt "dave hanley") = "david hanley");
  assert ((tt "Soh waI ching") = "wai ching soh");
  assert ((tt "bob toews") = "bob toews")


let () = test1()  ;
         test_split();
         test_gender();
         test_translate()


