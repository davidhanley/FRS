open Option
open Num
open Load
open Filters

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
  let date_not_ok = date_not_in_range now_msecs in
  let results = List.of_seq ( Seq.concat_map (read_a_race date_not_ok) files)  in
  let sorted_results = sort_athletes results in
  sorted_results

let age_option_to_string age_option =
  match age_option with
  | Some(age) -> string_of_int age
  | None -> "N/A"


let age_match ao1 ao2 =
 match (ao1, ao2) with
 | (Some(a1), Some(a2)) -> (Int.abs (a2-a1)) < 2
 | _ -> true

let ath_match ao1 ao2 =
  (age_match ao1.age ao2.age) && (ao1.name = ao2.name)

let group_athletes alist =
 let rec grouper lst acc out =
   match (lst,acc) with
   | ([],[]) -> out
   | ([],_) -> acc::out
   | a::rest,[] -> grouper rest [a] out
   | a::rest,aa::_ ->
     if (ath_match a.athlete aa.athlete)
        then grouper rest (a::acc) out
        else grouper rest [a] (acc::out) in
 grouper alist [] []


let compare_packets packet1 packet2 = Num.compare_num packet2.athlete.points packet1.athlete.points

let intZero = (Int 0)
let scored_points results =
   Seq.fold_left (fun x y -> x +/ y.athlete.points) intZero (Seq.take 5 (List.to_seq results))

type results_row = { name : string; points : num; packets : athlete_packet list; age : string}

let athlete_to_to_results_row packets =
  let sorted = List.sort compare_packets packets in
  let scored_points = scored_points sorted in
  let first = (List.hd packets) in
  let name = first.athlete.name and
      age  = first.athlete.age in
  {name = name; points = scored_points; packets = sorted ; age = age_option_to_string age}

let compare_rr results_row_1 results_row_2 = Num.compare_num results_row_2.points results_row_1.points

let filters_to_filename filters =
  String.cat (String.concat "-" (List.map (fun f->f.filterName) filters)) ".html"

let print_header_row out filters_used (filter_row:(filter list)) =
  Printf.fprintf out "<h2> %s : </h2> " ((List.hd filter_row).filtertype);
  List.iter (fun f -> let fn = filters_to_filename (replace_filter filters_used f) in
                      Printf.fprintf out "<a href = \"%s\"> %s </a> &nbsp; \n" fn f.filterName) filter_row;
  Printf.fprintf out"<br>"

let replace_underscore str = String.map (fun ch-> if ch='_' then ' ' else ch) str

let print_header out filters_used =
  let header_string = (String.concat ", " (List.map (fun (f:filter)->replace_underscore f.filterName) filters_used)) in
  Printf.fprintf out "<html><head><title>%s</title></head><body>" header_string;
  Printf.fprintf out "<h1>%s</h1>" header_string;
  List.iter (print_header_row out filters_used) [genderfilters; age_filters; foreign_filters]

let compare_header_and_rank packet1 packet2 =
  let headercmp = String.compare packet1.header.race_name packet2.header.race_name in
  if headercmp <> 0 then headercmp else  packet1.athlete.place - packet2.athlete.place

let re_score_packet packet score =
  let new_athlete = { packet.athlete with points = score } in
  { packet with athlete = new_athlete }

let maybe_update_score_seq packet previous_name old_scores =
  let points = packet.header.points in
  if packet.header.race_name = previous_name then old_scores else (get_score_sequence points)

let re_score_results packets =
  let rec rescorer lst previous_name scores =
    match lst with
       | [] -> []
       | packet::packets ->
           let s2 = maybe_update_score_seq packet previous_name scores in
           let ((pos, score), scores) = Option.get (Seq.uncons s2) in
           (re_score_packet packet score)::rescorer packets packet.header.race_name scores in
  rescorer packets "" Seq.empty

let flatten_and_sort_races (results:athlete_packet list list) extra =
  List.concat results |>
  List.sort compare_header_and_rank |>
  extra |>
  sort_athletes |>
  group_athletes

let print_ranked_athletes filtered =
  let filename = "content/" ^ (filters_to_filename filtered.filters) in
  let handle = open_out filename in
  let out = Printf.fprintf handle in
  let results_rows = List.map athlete_to_to_results_row (flatten_and_sort_races filtered.packets (fun x->x)) in
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

let apply_scoring filtered =
  List.map (fun filter->{filters=filter.filters; packets=flatten_and_sort_races filter.packets re_score_results}) filtered

let main() =
  load_races_into_chunked_athletes () |>
  group_athletes |>
  apply_filters_to_grouped_athlete_packets apply_scoring |>
  List.iter print_ranked_athletes

let () = main()


