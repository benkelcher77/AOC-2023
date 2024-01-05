open String
open List
open Re
open Int

let rec drop_last l = 
    match l with
    | [] | [_] -> []
    | first :: rest -> first :: (drop_last rest)

let rec debug_print_list l = 
    match l with
    | [] -> print_endline "END"
    | first :: rest -> print_string first; print_string " "; debug_print_list rest

let read_file filename = 
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    drop_last (split_on_char '\n' s)

(* Part 1 *)

let game_number line = 
    int_of_string (List.nth (split_on_char ' ' (List.hd (split_on_char ':' line))) 1)

let rounds line = 
    List.map String.trim (split_on_char ';' (List.nth (split_on_char ':' line) 1))

let rec num_of_col nwc col = 
    let regex = Re.Posix.compile_pat col in
    match nwc with
    | [] -> 0
    | first :: rest -> if (Int.equal (List.length (Re.all regex first)) 1) then int_of_string (List.hd (split_on_char ' ' first)) else num_of_col rest col

let round_to_nums_with_cols round = 
    List.map String.trim (split_on_char ',' round)

let round_valid round = 
    let nums_with_cols = round_to_nums_with_cols round in

    let num_red = num_of_col nums_with_cols "red" in
    let num_green = num_of_col nums_with_cols "green" in 
    let num_blue = num_of_col nums_with_cols "blue" in

    if (num_red > 12 || num_green > 13 || num_blue > 14) then false else true

let game_valid line = 
    List.for_all round_valid (rounds line)

(* Part 2 *)

let get_red nwc = num_of_col nwc "red"
let get_green nwc = num_of_col nwc "green"
let get_blue nwc = num_of_col nwc "blue"

let game_min_cube_set line = 
    let max_red = List.fold_left max 0 (List.map get_red (List.map round_to_nums_with_cols (rounds line))) in
    let max_green = List.fold_left max 0 (List.map get_green (List.map round_to_nums_with_cols (rounds line))) in
    let max_blue = List.fold_left max 0 (List.map get_blue (List.map round_to_nums_with_cols (rounds line))) in

    max_red * max_green * max_blue

let () = 
    let lines = read_file "input.txt" in
    print_int (List.fold_left ( + ) 0 (List.map game_number (List.filter game_valid lines)));
    print_endline "";
    print_int (List.fold_left ( + ) 0 (List.map game_min_cube_set lines))

