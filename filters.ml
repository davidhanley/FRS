
open Load

type filter = { filtertype : string; filterName : string; filterfunc: athlete_packet list -> bool}

let make_filter ftype name ff = { filtertype = ftype; filterName = name; filterfunc = ff }


let filter_gender gender packets = (List.hd packets).athlete.sex = gender
let make_gender_filter = make_filter "Gender"
let genderfilters = [make_gender_filter "Female" (filter_gender F); make_gender_filter "Male" (filter_gender M)]

let filter_age age_range packets =
  match (age_range,packets) with
  | (None,_) -> true
  | (Some(lo,hi),packet::_) -> (packet.athlete.age >= Some(lo)) && (packet.athlete.age <= Some(hi))
  | (_,_) -> false

let make_age_filter = make_filter "Age"
let ranges = [None; Some(0,9); Some(10,19); Some(20,29); Some(30,39); Some(40,49); Some(50,59); Some(60,69); Some(70,79); Some (80,89); Some(90,99)]
let range_to_string r =
  match r with
    | None->"All_ages"
    | Some(lo, hi) -> Printf.sprintf "%d_%d" lo hi
let age_filters = List.map (fun t-> make_age_filter (range_to_string t) (filter_age t)) ranges

let make_foreign_filter = make_filter "Foreign"
let any_foreign packets = List.exists (fun packet-> packet.athlete.foreign) packets

type ftypes = ALL | US_ONLY
let ftype_to_sgtring ftype =
  match ftype with
  | ALL -> "US_and_foreign"
  | US_ONLY -> "US_only"
let filter_foreign ftype packets =
  if ftype = ALL || (any_foreign packets) == false then true else
  if ftype = US_ONLY then false else true
let foreign_filters = List.map (fun t->make_foreign_filter (ftype_to_sgtring t) (filter_foreign t)) [ALL;US_ONLY]


type filtered = {filters: filter list; packets: athlete_packet list list}

let apply_filters filters op =
  List.map (fun filter -> {filters = filter::op.filters;  packets = List.filter filter.filterfunc op.packets} ) filters

let replace_filter filters replacing =
  List.map (fun filter->if (filter.filtertype) = replacing.filtertype then replacing else filter) filters


let apply_filters_to_grouped_athlete_packets apply_scoring grouped =
  let with_empty_filters = [{filters = []; packets = grouped}] in
  (* todo: turn the following three lines into fold_left *)
  let filtered = List.fold_left (fun acc filter-> List.concat (List.map (apply_filters filter) acc)) with_empty_filters
          [genderfilters; foreign_filters] in
  let f2 = apply_scoring filtered in
  List.fold_left (fun acc filter-> List.concat (List.map (apply_filters filter) acc)) f2 [age_filters]

