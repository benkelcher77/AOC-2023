open String
open List
open Re

let read_file filename = 
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    split_on_char '\n' s

let rev x =
  let len = String.length x in
  String.init len (fun n -> String.get x (len - n - 1))

let process_line line =
    let regex_s = "zero|one|two|three|four|five|six|seven|eight|nine" in
    let regex = Re.Posix.compile_pat (regex_s ^ "|[0-9]") in
    let r_regex = Re.Posix.compile_pat ((rev regex_s) ^ "|[0-9]") in

    let get_num str =
        match str with
        | "0" | "zero" -> 0
        | "1" | "one" -> 1
        | "2" | "two" -> 2
        | "3" | "three" -> 3
        | "4" | "four" -> 4
        | "5" | "five" -> 5
        | "6" | "six" -> 6
        | "7" | "seven" -> 7
        | "8" | "eight" -> 8
        | "9" | "nine" -> 9
        | _ -> -1
    in

    let first_digit line = 
        let matches = Re.all regex line in
        match matches with
        | [] -> -1
        | first :: _ -> get_num (Re.Group.get first 0)
    in

    let last_digit line = 
        let matches = Re.all r_regex (rev line) in
        match matches with
        | [] -> -1
        | first :: _ -> get_num (rev (Re.Group.get first 0))
    in

    match (first_digit line), (last_digit line) with
    | -1, -1 -> 0
    | _, _ -> (first_digit line) * 10 + (last_digit line)

let rec debug_print lines = 
    match lines with
    | [] -> ()
    | first :: rest -> print_endline first; print_int (process_line first); print_endline ""; debug_print rest

let () = 
    let lines = read_file "input.txt" in
    print_int (List.fold_left ( + ) 0 (List.map process_line lines));

