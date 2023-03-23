
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
                                 try Some(List.map String.trim (split_on_commas (input_line inf)))
                                 with _ -> None ) in
  lines


let data_directory = "TowerRunningRaceData/"

let load_name_translator () =
  let lines = List.of_seq (file_to_strings (data_directory ^ "translate.dat")) in
  let translate_table = List.map (fun pv->Str.regexp_case_fold (List.nth pv 0),List.nth pv 1) lines in
  (fun name -> let translation = List.find_opt (fun (p,_)->Str.string_match p name 0) translate_table in
    match translation with
      | None -> name
      | Some (_,translated) -> translated )

let translator = load_name_translator()

let load_foreign_lookup () =
  let lines = List.map List.hd (List.of_seq (file_to_strings (data_directory ^ "foreign.dat"))) in
  let upcased_lines = List.map String.uppercase_ascii lines in
  let stringset = StringSet.of_seq (List.to_seq upcased_lines) in
  fun str->StringSet.mem (String.uppercase_ascii str) stringset

let foreign_lookup = load_foreign_lookup()

let line_to_athlete (position, points) split_line =
  try
    let column idx = List.nth split_line idx in
    let gender_option = string_to_gender_and_foreign (column 3) and
      age = string_to_int_option (column 2)  in
    let fixed_name:string = String.uppercase_ascii (translator (column 1)) in
      match gender_option with
        | Some(gender) -> List.to_seq [{name = fixed_name ; sex = gender ; age = age ; foreign = foreign_lookup fixed_name; place = position; points = points}]
        | None     -> Seq.empty
  with _ -> Seq.empty

let string_to_date str =
  let itSplit = Str.split (Str.regexp "-+") str in
  let parts = List.map int_of_string itSplit in
  let n i = List.nth parts i in
  { tm_year = (n 0)-1900; tm_mon = (n 1)-1 ; tm_mday = n 2 ; tm_sec = 0 ; tm_min = 0 ; tm_hour = 0 ; tm_wday = 0 ; tm_yday = 0 ; tm_isdst = false }

type race_header = { race_name : string; date : tm; points : int }

let parse_header name date_string points_string =
  let d2 = string_to_date date_string and
      p = int_of_string  points_string in
    { race_name = name ; date = d2 ; points = p }

(* because we add up a lot of small numbers with a lot of decimals, don't use floats.  Scores
   are rationals, so keep them as such.  Just convert to a float at the end for printing *)

let get_score_sequence base_score =
  let score_denominator = Int (5 * base_score) in
  Seq.map (fun position-> (position, score_denominator // ((Int 4) +/ (Int position)))) (Seq.ints 1)

let rec dedupe_athletes athletes set =
  match (Seq.uncons athletes) with
      None -> Seq.empty
    | Some(athlete, tail) ->
        if StringSet.mem athlete.name set
          then dedupe_athletes tail set
          else Seq.cons athlete (dedupe_athletes tail (StringSet.add athlete.name set))


let read_athletes lines base_points =
  let points_sequence = get_score_sequence base_points in
  let read_and_scored = Seq.concat (Seq.map2 line_to_athlete points_sequence lines) in
  dedupe_athletes read_and_scored StringSet.empty


type athlete_packet = { athlete: athlete; header: race_header }

let date_not_in_range now date  =
  let (fsecs, _) = Unix.mktime date in
  let race_secs = (int_of_float fsecs) in
    race_secs + 365 * 24 * 60 * 60 < now (* || secs > now *)

let now_msecs = int_of_float (Unix.time ())

let race_list_to_strings lines skip_race_for_date =
  let first_4 = List.of_seq (Seq.take 4 lines) in
  match first_4 with
  | (name::_)::(date::_)::_::(points::_)::_ ->
    Printf.printf "%s\n%s\n%s\n" name date points;
    let header = parse_header name date points in
    if skip_race_for_date header.date then begin
      Printf.printf "Too old, skipping...\n";
      Seq.empty
      end
    else
      let athletes = read_athletes lines header.points in
        Seq.map (fun ath->{athlete = ath; header=header}) athletes
  | _ -> Seq.empty

let read_a_race date_not_ok filename  =
  Printf.printf "Reading.. %s\n" filename;
  let lines = file_to_strings filename in
  race_list_to_strings lines date_not_ok
