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
  try
    let ss = Str.split (Str.regexp ",+") str in
    let le idx = List.nth ss idx in
    let s = string_to_gender (le 3) and
      a = string_to_int_option (le 2)
    in
      match s with
        | Some(gt) -> Some({name = (le 1) ; sex = gt ; age = a })
        | None     -> None
  with
    | _ -> None

type date = { y:int ; m:int ; d:int }

let strig_to_date str =
  let [y;m;d] = List.map int_of_string (Str.split (Str.regexp "/+") str ) in
  {y=y;m=m;d=d}


type race_header = { name:string; date:date; points:int }

(* let read_header in_chan = *)

let fread fn =
  let ic = open_in fn in
  try
    let
      line = input_line ic
    in
      print_endline line;
      flush stdout;
      close_in ic
  with e ->
    close_in_noerr ic

let foo =
  let res = dir_contents "data" in
  let _ = List.map fread res in
      ()


let () = foo

