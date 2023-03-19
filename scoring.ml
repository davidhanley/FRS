
open Str
open Char
open Option
open Num
open Unix

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

type athlete = { name : string ; sex : gender ; age : int option ; foreign: bool; place: int; points: num}

(* how is this not in the standard library *)
let string_to_int_option str =
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
let split_on_commas str = Str.split_delim comma_regex str |> List.map String.trim


let file_to_strings filename =
  let inf = open_in filename in
  let lines = Seq.of_dispenser (fun () ->
                                 try Some(String.trim (input_line inf))
                                 with _ -> None ) in
  List.of_seq lines




let data_directory = "TowerRunningRaceData/"

let load_name_translator () =
  let lines = List.map split_on_commas (file_to_strings (data_directory ^ "translate.dat")) in
  let translate_table = List.map (fun pv->Str.regexp_case_fold (List.nth pv 0),List.nth pv 1) lines in
  (fun name -> let translation = List.find_opt (fun (p,_)->Str.string_match p name 0) translate_table in
    match translation with
      | None -> name
      | Some (_,translated) -> translated )

let translator = load_name_translator()

let load_foreign_lookup () =
  let lines = file_to_strings (data_directory ^ "foreign.dat") in
  let upcased_lines = List.map String.uppercase_ascii lines in
  let stringset = StringSet.of_seq (List.to_seq upcased_lines) in
  fun str->StringSet.mem (String.uppercase_ascii str) stringset

let foreign_lookup = load_foreign_lookup()

let line_to_athlete (position, points) str =
  try
    let split_line = split_on_commas str in
    let column idx = List.nth split_line idx in
    let gender_option = string_to_gender_and_foreign (column 3) and
      age = string_to_int_option (column 2)  in
    let fixed_name:string = String.uppercase_ascii (translator (column 1)) in
      match gender_option with
        | Some(gender) -> List.to_seq [{name = fixed_name ; sex = gender ; age = age ; foreign = foreign_lookup fixed_name; place = position; points = points}]
        | None     -> Seq.empty
  with _ -> Seq.empty

let first_commasep str = split_on_commas str |> List.hd

let string_to_date str =
  let itSplit = first_commasep str |> Str.split (Str.regexp "-+") in
  let parts = List.map int_of_string itSplit in
  let n i = List.nth parts i in
  { tm_year = (n 0)-1900; tm_mon = n 1 ; tm_mday = n 2 ; tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ; tm_wday = 0 ; tm_yday = 0 ; tm_isdst = false }

type race_header = { race_name : string; date : tm; points : int }

let parse_header name date_string points_string =
  let d2 = string_to_date date_string and
      p = first_commasep points_string |> int_of_string  in
    { race_name = first_commasep name ; date = d2 ; points = p }

(* because we add up a lot of small numbers with a lot of decimals, don't use floats.  Scores
   are rationals, so keep them as such.  Just convert to a float at the end for printing *)
let get_score_sequence base_score =
  let score_denominator = Int ( 5 * base_score) in
  Seq.map (fun position-> (position,score_denominator // ((Int 4) +/ (Int position)))) (Seq.ints 1)

let read_athletes lines base_points =
  let points_iterator = get_score_sequence base_points in
  (Seq.concat (Seq.map2 line_to_athlete points_iterator (List.to_seq lines)))

type athlete_packet = { athlete: athlete; header: race_header }

let date_not_in_range now date  =
  let (fsecs, _) = Unix.mktime date in
  let secs = (int_of_float fsecs) in
    secs + 365 * 24 * 60 * 60 < now (* || secs > now *)

let now = (int_of_float (Unix.time ()))

let race_list_to_strings lines skip_race_for_date =
  match lines with
  | name::date::_::points::rest ->
    Printf.printf "%s\n%s\n%s\n" name date points;
    let header = parse_header name date points in
    if skip_race_for_date header.date then begin
      Printf.printf "Too old, skipping...\n";
      Seq.empty
      end
    else
      let athletes = read_athletes rest header.points in
        Seq.map (fun ath->{athlete = ath; header=header}) athletes
  | _ -> Seq.empty

let read_a_race date_not_ok filename  =
  Printf.printf "Reading.. %s\n" filename;
  let lines = file_to_strings filename in
  race_list_to_strings lines date_not_ok

let compare_athletes a1 a2 =
  let r = String.compare a1.name a2.name in
  if r <> 0 then r else
    match (a1.age, a2.age) with
    | (None, None) -> 0
    | (Some(_), None) -> 1
    | (None, Some(_)) -> -1
    | (Some(age1), Some(age2)) -> age1-age2

let sort_athletes results =
   List.sort (fun a1 a2-> compare_athletes a1.athlete a2.athlete) results

let load_races_into_chunked_athletes () =
  let files = dir_contents data_directory in
  let date_not_ok = date_not_in_range now in
  let results = Seq.concat_map (read_a_race date_not_ok) files  in
  let sorted_results = sort_athletes (List.of_seq results) in
  sorted_results

let age_option_to_string age_option =
  match age_option with
  | Some(age) -> string_of_int age
  | None -> "N/A"


let age_match ao1 ao2 =
 match (ao1,ao2) with
 | (Some(a1),Some(a2)) -> (Int.abs (a2-a1)) < 2
 | _ -> true

let ath_match ao1 ao2 =
  (age_match ao1.age ao2.age) && (ao1.name = ao2.name)

let group_athletes (alist:athlete_packet list) =
 let rec grouper (lst:athlete_packet list) acc out =
   match (lst,acc) with
   | ([],[]) -> out
   | ([],_) -> acc::out
   | a::rest,[] -> grouper rest [a] out
   | a::rest,aa::_ ->
     if (ath_match a.athlete aa.athlete)
        then grouper rest (a::acc) out
        else grouper rest [a] (acc::out) in
 grouper alist [] []


let compare_packets (a1:athlete_packet) (a2:athlete_packet) = Num.compare_num a2.athlete.points a1.athlete.points

let rec take n lst =
  match lst with
  | [] -> []
  | f::rest -> if n < 1 then [] else f::(take (n-1) rest)

let scored_points results =
   List.fold_left (fun x y -> x +/ y.athlete.points) (Int 0) (take 5 results)

type results_row = { name : string; points : num; packets : athlete_packet list; age : string}

let athlete_to_to_results_row packets =
  let sorted = List.sort compare_packets packets in
  let scored_points = scored_points sorted in
  let first = (List.hd packets) in
  let name = first.athlete.name and
      age  = first.athlete.age in
  {name = name; points = scored_points; packets = sorted ; age = age_option_to_string age}


type filter = { filtertype : string; name : string; filterfunc: athlete_packet list -> bool}
let make_filter ftype name ff = { filtertype = ftype; name = name; filterfunc = ff }


let filter_gender gender (packets:athlete_packet list) = (List.hd packets).athlete.sex = gender
let make_gender_filter = make_filter "gender"
let genderfilters = [make_gender_filter "Female" (filter_gender F); make_gender_filter "Male" (filter_gender M)]

let filter_age age_range (packets:athlete_packet list) =
  match (age_range,packets) with
  | (None,_) -> true
  | (Some(lo,hi),packet::_) -> (packet.athlete.age >= Some(lo)) && (packet.athlete.age <= Some(hi))
  | (_,_) -> false

let make_age_filter = make_filter "age"
let ranges = [None; Some(0,9); Some(10,19); Some(20,29); Some(30,39); Some(40,49); Some(50,59); Some(60,69); Some(70,79); Some (80,89); Some(90,99)]
let range_to_string r =
  match r with
    | None->"all"
    | Some(lo, hi) -> Printf.sprintf "%d-%d" lo hi
let age_filters = List.map (fun t-> make_age_filter (range_to_string t) (filter_age t)) ranges

let make_foreign_filter = make_filter "foreign"
let any_foreign (packets:athlete_packet list) = List.exists (fun packet-> packet.athlete.foreign) packets

type ftypes = ALL | US_ONLY
let ftype_to_sgtring ftype =
  match ftype with
  | ALL -> "All"
  | US_ONLY -> "US_only"
let filter_foreign ftype packets =
  if ftype = ALL || (any_foreign packets) == false then true else
  if ftype = US_ONLY then false else true
let foreign_filters = List.map (fun t->make_foreign_filter (ftype_to_sgtring t) (filter_foreign t)) [ALL;US_ONLY]


type filtered = {filters: filter list; packets: athlete_packet list list}

let apply_filters filters op =
  List.map (fun filter -> {filters = filter::op.filters;  packets = List.filter filter.filterfunc op.packets} ) filters

let compare_rr results_row_1 results_row_2 = Num.compare_num results_row_2.points results_row_1.points

let replace_filter filters replacing =
  List.map (fun filter->if (filter.filtertype) = replacing.filtertype then replacing else filter) filters

let filters_to_filename filters =
  String.cat (String.concat "-" (List.map (fun f->f.name) filters)) ".html"

let print_header_row out  filters_used (filter_row:(filter list)) =
  Printf.fprintf out "<h2> %s : </h2> " ((List.hd filter_row).filtertype);
  List.iter (fun f -> let fn = filters_to_filename (replace_filter filters_used f) in
                      Printf.fprintf out "<a href = \"%s\"> %s </a> &nbsp; \n" fn f.name) filter_row;
  Printf.fprintf out"<br>"

let print_header out filters_used =
  let hs = (String.concat ", " (List.map (fun (f:filter)->f.name) filters_used)) in
  Printf.fprintf out "<html><head><title>%s</title></head><body>" hs;
  Printf.fprintf out "<h1>%s</h1>" hs;
  List.iter (print_header_row out filters_used) [genderfilters; age_filters; foreign_filters]

let compare_header_and_rank packet1 packet2 =
  let headercmp = String.compare packet1.header.race_name packet2.header.race_name in
  if headercmp <> 0 then headercmp else  packet1.athlete.place - packet2.athlete.place

let re_score_packet packet score =
  let new_athlete = { packet.athlete with points = score } in
  { packet with athlete = new_athlete }

let maybe_update_iterator packet previous_name old_scores =
  let points = packet.header.points in
  if packet.header.race_name = previous_name then old_scores else (get_score_sequence points)

let re_score_results (packets:athlete_packet list) =
  let rec rescorer lst previous_name scores =
    match lst with
       | [] -> []
       | packet::packets ->
           let s2 = maybe_update_iterator packet previous_name scores in
           let ((pos, score), scores) = Option.get (Seq.uncons s2) in
           (re_score_packet packet score)::rescorer packets packet.header.race_name scores in
  rescorer packets "" Seq.empty


let flatten_and_sort_races (results:athlete_packet list list) =
  List.concat results |>
  List.sort compare_header_and_rank |>
  re_score_results |>
  sort_athletes |>
  group_athletes

let print_ranked_athletes filtered =
  let filename = filters_to_filename filtered.filters in
  let handle = open_out filename in
  let out = Printf.fprintf handle in
  let results_rows = List.map athlete_to_to_results_row (flatten_and_sort_races filtered.packets) in
  let sorted_results:results_row list = List.sort compare_rr results_rows in
  print_header handle filtered.filters;
  out "<table border=2>";
  List.iteri (fun i (row:results_row)-> (* why is this type not inferred *)
      out "<tr>";
      Printf.fprintf handle "<td>%d</td>" (i+1);
      Printf.fprintf handle "<td>%s <br> %s <br> %f</td>" row.name row.age (Num.float_of_num row.points);
      List.iter (fun packet-> Printf.fprintf handle "<td> %s <br> %f</td>" packet.header.race_name (Num.float_of_num packet.athlete.points)) row.packets;
      out "</tr>\n" ) sorted_results;
  out "</table>";
  close_out handle

let main() =
  let all_athletes = load_races_into_chunked_athletes () in
  let grouped = group_athletes all_athletes in
  let with_empty_filters = [{filters = []; packets = grouped}] in
  (* todo: turn the following three lines into fold_left *)
  let filtered = List.fold_left (fun acc filter-> List.concat (List.map (apply_filters filter) acc)) with_empty_filters
          [genderfilters; age_filters; foreign_filters] in
  List.iter print_ranked_athletes filtered

let () = main()


