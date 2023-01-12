open Str
open Char
open Option

module StringSet = Set.Make(String)


(* string -> string list *)
let dir_contents dir =
  let rec loop result = function
    | f::fs when Sys.is_directory f ->
          Sys.readdir f
          |> Array.to_list
          |> List.map (Filename.concat f)
          |> List.append fs
          |> loop result
    | f::fs -> loop (f::result) fs
    | []    -> result
  in
    List.to_seq (List.filter (String.ends_with ~suffix:".csv") (loop [] [dir]))

type gender = M | F

type athrow = { name : string ; sex : gender ; age : int option ; foreign: bool; place: int; points: float}

let string_to_int_option str:int option =
  try
  	Some (int_of_string str)
  with
  	| _ -> None

let string_to_gender str =
  match (Char.uppercase_ascii str.[0]) with
    | 'M' -> Some(M)
    | 'F' -> Some(F)
    | _ -> None

let string_to_gender_and_foreign str =
  try
    match str.[0] with
      | '*' -> string_to_gender (Str.string_after str 1)
      | _   -> string_to_gender str
  with _ -> None

let comma_regex = (Str.regexp ",")

let split_on_commas = Str.split_delim comma_regex

let file_to_strings fn =
  let inf = open_in fn in
   let rec reader acc =
      try
        let line = input_line inf in
        reader (line::acc)
      with _ ->
        close_in_noerr inf;
        acc
    in
      List.rev (reader [])


let load_name_translator () =
  let lines = List.map split_on_commas (file_to_strings "data/translate.dat") in
  let translate_table = List.map (fun pv->Str.regexp_case_fold (List.nth pv 0),List.nth pv 1) lines in
  (fun name -> let rr = List.find_opt (fun (p,_)->Str.string_match p name 0) translate_table in
    match rr with
      | None -> name
      | Some (_,n) -> n )

let translator = load_name_translator()

let load_foreign_lookup () =
  let lines = file_to_strings "data/foreign.dat" in
  let upcased_lines = List.map String.uppercase_ascii lines in
  let stringset = StringSet.of_seq (List.to_seq upcased_lines) in
  fun str->StringSet.mem (String.uppercase_ascii str) stringset

let foreign_lookup = load_foreign_lookup()

let line_to_athlete_row (position,points) str =
  try
    let ss = split_on_commas str in
    let le idx = List.nth ss idx in
    let gender_option = string_to_gender_and_foreign (le 3) and
      a = string_to_int_option (le 2)  in
    let fixed_name = String.uppercase_ascii (translator (le 1)) in
      match gender_option with
        | Some(gender) -> List.to_seq [{name = fixed_name ; sex = gender ; age = a ; foreign = foreign_lookup(fixed_name); place = position; points = points}]
        | None     -> Seq.empty
  with _ -> Seq.empty

(*
let () = assert ((line_to_athlete_row "1,WAI CHING SOH,,M,10:44:00 AM") = Some {name = "WAI CHING SOH" ; sex = M ; age = None })
*)

type date = { y:int ; m:int ; d:int }

let string_to_date str =
  let parts = List.map int_of_string (Str.split (Str.regexp "-+") str) in
  let n i = List.nth parts i in
  { y = n 0; m = n 1 ; d = n 2 }

let date_diff d1 d2 =
  (d2.y-d1.y)*30*12 + (d2.m-d1.m)*30 + (d2.d-d1.d)

type race_header = { name:string; date:date; points:int }

let read_header name d p =
    { name = name ; date = string_to_date d ; points = int_of_string p }

let get_score_iterator base_score =
  let float_base = (float_of_int (5 * base_score)) in
  Seq.map (fun position-> (position,float_base /. (4.0 +. (float_of_int position)))) (Seq.ints 1)

let read_athletes lines base_points:athrow Seq.t =
  let points_iterator = get_score_iterator base_points in
  (Seq.concat (Seq.map2 line_to_athlete_row points_iterator (List.to_seq lines)))

type athete_packet = { athlete: athrow ; header: race_header }

let read_a_race fn =
  Printf.printf "Reading.. %s\n" fn;
  let lines = file_to_strings fn in
  match lines with
  | name::date::_::points::rest ->
    let header = read_header name date points in
    let athletes = read_athletes rest header.points in
        Seq.map (fun ath->{athlete = ath; header=header}) athletes
  | _ -> Seq.empty

let compare_athletes (a1:athrow) (a2:athrow) =
  let r = String.compare a1.name a2.name in
  if r <> 0 then r else
    match (a1.age,a2.age) with
    | (None, None) -> 0
    | (Some(_), None) -> 1
    | (None, Some(_)) -> -1
    | (Some(age1), Some(age2)) -> age1-age2

let sort_athletes (results:athete_packet list) =
   List.sort (fun a1 a2-> compare_athletes a1.athlete a2.athlete) results

let load_races_into_chunked_athletes () =
  let files = dir_contents "data" in
  let results = Seq.concat_map read_a_race files in
  let sorted_results = sort_athletes (List.of_seq results) in
  sorted_results

let age_option_to_string age_option =
  match age_option with
  | Some(age) -> string_of_int age
  | None -> "N/A"


let age_match ao1 ao2 =
 match (ao1,ao2) with
 | (None,_) -> true
 | (_,None) -> true
 | (Some(a1),Some(a2)) -> (Int.abs (a2-a1)) < 2

let ath_match ao1 ao2 =
  (age_match ao1.age ao2.age) && (ao1.name = ao2.name)

let group_athletes (alist:athete_packet list) =
 let rec grouper (lst:athete_packet list) acc out =
   match (lst,acc) with
   | ([],[]) -> out
   | ([],_) -> acc::out
   | a::rest,[] -> grouper rest [a] out
   | a::rest,aa::_ ->
     if (ath_match a.athlete aa.athlete)
        then grouper rest (a::acc) out
        else grouper rest [a] (acc::out) in
 grouper alist [] []

let float_cmp f1 f2 =
  let fd = f1 -. f2 in
  if fd < 0.0 then -1 else (if fd > 0.0 then 1 else 0)

let compare_packets (a1:athete_packet) (a2:athete_packet) = float_cmp a2.athlete.points a1.athlete.points

let rec take n lst =
  match lst with
  | [] -> []
  | f::rest -> if n < 1 then [] else f::(take (n-1) rest)

(* todo: take out middle map *)
let scored_points results =
   List.fold_left (fun x y -> x +. y.athlete.points) 0.0 (take 5 results)

type results_row = { name:string; points:float; packets: athete_packet list}

let athelte_to_to_results_row (packets:athete_packet list) =
  let sorted = List.sort compare_packets packets in
  let scored_points = scored_points sorted in
  let name = (List.hd packets).athlete.name in
  {name = name; points = scored_points; packets = sorted }

let compare_rr (r1:results_row) (r2:results_row) = float_cmp (r2.points) (r1.points)

let print_partitioned wrath =
  let results_rows = List.map athelte_to_to_results_row wrath in
  let sorted_results = List.sort compare_rr results_rows in
  List.iter (fun (r:results_row)->
      print_string "<tr>";
      Printf.printf "<td>%s %f</td>" r.name r.points;
      List.iter (fun (r:athete_packet)-> Printf.printf "<td> %s <br> %f</td>" r.header.name r.athlete.points) r.packets;
      print_string "</tr>\n" ) sorted_results

type filter = {filtertype:string; name:string; filterfunc: athete_packet list->bool}

let filter_gender gender (packets:athete_packet list) = (List.hd packets).athlete.sex = gender

let make_filter ftype name ff = { filtertype = ftype; name = name; filterfunc = ff }

let make_gender_filter = make_filter "gender"

let genderfilters = [make_gender_filter "Female" (filter_gender F); make_gender_filter "Male" (filter_gender M);
                     ]


let apply_filters filters (wrath:athete_packet list list) =
  List.map (fun filter -> List.filter filter.filterfunc wrath) filters

let () =
  let wrath = load_races_into_chunked_athletes () in
  let grouped = group_athletes wrath in
  let rtypes = apply_filters genderfilters grouped in
  let _ = List.map print_partitioned rtypes in
  ()




