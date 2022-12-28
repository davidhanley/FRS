open Str
open Char
open Option

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
    loop [] [dir]

type gender = M | F

type athrow = { name : string ; sex : gender ; age : int option ; foreign: bool}

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
      | '*' -> (true, string_to_gender (Str.string_after str 1))
      | _   -> (false, string_to_gender str)
  with _ -> (false, None)

let split_on_commas str = Str.split_delim (Str.regexp ",") str

let line_to_athlete_row str =
  try
    let ss = split_on_commas str in
    let le idx = List.nth ss idx in
    let (foreign,gender_option) = string_to_gender_and_foreign (le 3) and
      a = string_to_int_option (le 2)
    in
      match gender_option with
        | Some(gender) -> [{name = String.uppercase_ascii (le 1) ; sex = gender ; age = a ; foreign = foreign}]
        | None     -> []
  with _ -> []

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

let read_header lines =
  match lines with
  | name :: d :: _ :: p :: _ ->
    (* Printf.printf "name:%s date:%s points:%s.\n" name d p; *)
    let date = string_to_date d in
    let points = int_of_string p in
    { name = name ; date = date ; points = points }
  | _ -> raise Exit

let get_incer() =
  let index = ref 0 in
  let incer () = index := !index + 1; !index in
  incer

let get_score_iterator base_score =
  let incer = get_incer() in
  let float_base = (float_of_int base_score) in
  fun ()-> float_base *. 5.0 /. (4.0 +. (float_of_int (incer ())))

let read_athletes lines =
  let jaggedy = List.concat (List.map line_to_athlete_row lines) in
  let incer = get_incer() in
  List.map (fun a->(a,incer())) jaggedy

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


type athete_packet = { athlete: athrow ; position: int; header: race_header }

let read_a_race fn =
  Printf.printf "Reading.. %s\n" fn;
  let lines = file_to_strings fn in
    let header = read_header lines and
        athletes = read_athletes (List.tl (List.tl (List.tl (List.tl lines)))) in
      List.map (fun (ath,idx)->{athlete = ath; position=idx; header=header}) athletes

let compare_athletes (a1:athrow) (a2:athrow) =
  let r = String.compare a1.name a2.name in
  if r <> 0 then r else
    match (a1.age,a2.age) with
    | (None,None) -> 0
    | (Some(_),None) -> 1
    | (None,Some(_)) -> -1
    | (Some(age1),Some(age2)) -> age1-age2



let sort_athletes (results:athete_packet list) =
   List.sort (fun a1 a2-> compare_athletes a1.athlete a2.athlete) results

let load_races_into_chunked_athletes () =
  let files = dir_contents "data" in
  let results = List.concat (List.map read_a_race files) in
  let sorted_results = sort_athletes results in
  sorted_results

let age_option_to_string age_option =
  match age_option with
  | Some(age) -> string_of_int age
  | None -> "N/A"

let print_athletes (wrath:athete_packet list) =
  Printf.printf "-----------------------\n";
  List.iter (fun athl -> let ath = athl.athlete in
                              Printf.printf "%s %s %d %s\n" ath.name (age_option_to_string ath.age) athl.position athl.header.name) wrath

let print_partitioned wrath = List.map print_athletes wrath

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
   | a::rest,aa::restaa -> if (ath_match a.athlete aa.athlete) then grouper rest (a::acc) out else grouper rest [a] (acc::out) in
 grouper alist [] []

let () =
  let wrath = load_races_into_chunked_athletes () in
  print_partitioned (group_athletes wrath);
  ()




