open OUnit2
open Scoring
open Num

let test_split() =
  let ss = split_on_commas ",dave,12,M" in
  assert ((List.length ss) = 4)

let test1 () =
  let parsed = line_to_athlete_row  (1,Int 100) ",dave hanley,49,M,silly"   in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "DAVID HANLEY");
               assert (pr.sex = M);
               assert (pr.age = Some 49);
               assert (pr.foreign = false)
    | [] -> assert false

let test2 () =
  let parsed = line_to_athlete_row (1,Int 200) ",Wai Ching Soh,24,M,silly" in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "WAI CHING SOH");
               assert (pr.sex = M);
               assert (pr.age = Some 24);
               assert (pr.foreign = true)
    | [] -> assert false

let test3 () =
  let parsed = line_to_athlete_row (1,Int 100) ",Soh Wai Ching,24,M,silly" in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "WAI CHING SOH");
               assert (pr.sex = M);
               assert (pr.age = Some 24);
               assert (pr.foreign = true)
    | [] -> assert false

let test_gender () =
  assert ((string_to_gender_and_foreign "*M") = Some M);
  assert ((string_to_gender_and_foreign "*Female") = Some F);
  assert ((string_to_gender_and_foreign "f") = Some F);
  assert ((string_to_gender_and_foreign "m") = Some M)

let test_translate () =
  let tt = load_name_translator() in
  assert ((tt "dave hanley") = "david hanley");
  assert ((tt "Soh waI ching") = "wai ching soh");
  assert ((tt "bob toews") = "bob toews")

let test_is_foreign () =
  assert (foreign_lookup "waI ching SoH" = true);
  assert (foreign_lookup "dave hanley" = false)

let test_take () =
  assert( take 5 [1;2] = [1;2]);
  assert( take 5 [1;2;3;4;5;6;7] = [1;2;3;4;5])


let () = test1();
         test2();
         test3();
         test_split();
         test_gender();
         test_translate();
         test_is_foreign();
         test_take()



