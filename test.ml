open OUnit2
open Scoring
open Num

let test_split() =
  let ss = split_on_commas ",dave,12,M" in
  assert ((List.length ss) = 4)

let test1 () =
  let parsed = line_to_athlete  (1,Int 100) (split_on_commas ",dave hanley,49,M,silly")   in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "DAVID HANLEY");
               assert (pr.sex = M);
               assert (pr.age = Some 49);
               assert (pr.foreign = false)
    | [] -> assert false

let test2 () =
  let parsed = line_to_athlete (1,Int 200) (split_on_commas ",Wai Ching Soh,24,M,silly") in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "SOH WAI CHING");
               assert (pr.sex = M);
               assert (pr.age = Some 24);
               assert (pr.foreign = true)
    | [] -> assert false

let test3 () =
  let parsed = line_to_athlete (1, Int 100) (split_on_commas ",Soh Wai Ching,24,M,silly") in
  match (List.of_seq parsed) with
    | pr::_ ->
               assert (pr.name = "SOH WAI CHING");
               assert (pr.sex = M);
               assert (pr.age = Some 24);
               assert (pr.foreign = true)
    | [] -> assert false

let test4 () =
  let parsed = line_to_athlete (1, Int 100) (split_on_commas ",Soh Wai Ching,24,M,silly") in
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
  assert ((tt "WAI CHInG SOH") = "SOH WAI CHING");
  assert ((tt "bob toews") = "bob toews")

let test_is_foreign () =
  assert (foreign_lookup "waI ching SoH" = true);
  assert (foreign_lookup "dave hanley" = false)

let test_date () =
  let d = string_to_date "2023-1-25" in
  assert( d.tm_year = 123)

let athletes_to_dedupe = [
  { name="dave"; sex= M; age=Some(50); foreign=false; place=1; points=(Int 100)} ;
  { name="erin"; sex= F; age=Some(53); foreign=false; place=2; points=(Int 80)} ;
  { name="prince"; sex= F; age=Some(53); foreign=false; place=3; points=(Int 50)} ;
   { name="dave"; sex= M; age=Some(50); foreign=false; place=1; points=(Int 100)}
]

let test_dedupe_athletes () =
  let filterd = dedupe_athletes (List.to_seq athletes_to_dedupe) StringSet.empty in
    assert( (Seq.length filterd) = 3)


let () = test_date();
         test1();
         test2();
         test3();
         test_split();
         test_gender();
         test_translate();
         test_is_foreign();
         test_dedupe_athletes();




