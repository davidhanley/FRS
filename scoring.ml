
open Str
open Char

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

type athrow = { name : string ; sex : gender ; age : int option }

let ar = { name = "dave" ; sex = M ; age = Some 49 }

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

let line_to_athlete_row str =
    let ss = Str.split (Str.regexp ",") str in
    let le idx = List.nth ss idx in
    let s = string_to_gender (le 3) and
      a = string_to_int_option (le 2)
    in

      match s with
        | Some(gt) -> Some({name = (le 1) ; sex = gt ; age = a })
        | None     -> None

let () = assert ((line_to_athlete_row "1,WAI CHING SOH,,M,10:44:00 AM") = Some {name = "WAI CHING SOH" ; sex = M ; age = None })

type date = { y:int ; m:int ; d:int }

let string_to_date str =
  let parts = List.map int_of_string (Str.split (Str.regexp "-+") str) in
  let n i = List.nth parts i in
  { y = n 0; m = n 1 ; d = n 2 }

let date_diff d1 d2 =
  (d2.y-d1.y)*30*12 + (d2.m-d1.m)*30 + (d2.d-d1.d)

type race_header = { name:string; date:date; points:int }

let read_header nextline =
  let name = nextline() in
  let date = string_to_date (nextline()) in
  let _ = nextline() in
  let points = int_of_string (nextline()) in
  { name = name ; date = date ; points = points }

let rec read_rest nextline  =
  try
    let ath = line_to_athlete_row (nextline ()) in
      match ath with
        | Some(a) -> a::(read_rest nextline)
        | None    -> (read_rest nextline)
   with
      | _ -> []

type unscored_race = { header: race_header; athletes:athrow list}

let read_a_race fn =
  let ic = open_in fn in
  let nextline () = (input_line ic) in
  try
    let header = read_header nextline and
        lines = read_rest nextline in
      List.iter (fun (t:athrow) -> print_endline t.name) lines;
      flush stdout;
      close_in ic;
      [ { header = header; athletes = lines }]
  with _ ->
    close_in_noerr ic;
    []

let esbru = read_a_race "data/2022-esbru.csv";;

let foo =
  let files = dir_contents "data" in
  let races = List.flatten (List.map read_a_race files) in
  races



