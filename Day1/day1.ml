open String;;
open List;;

(* Read file into a list of strings, one for each line. *)
let read_file filename = 
    let ch = open_in filename in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
    split_on_char '\n' s

let rec process_lines lines =
    let first_digit acc c =
        match c with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> c
            | _ -> acc
    in

    let last_digit c acc = 
        match c with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> c
            | _ -> acc
    in

    let process_line line = 
        int_of_string ((String.make 1 (String.fold_right last_digit line '0')) ^ (String.make 1 (String.fold_left first_digit '0' line)))
    in

    List.map process_line lines

let () = 
    let lines = read_file "input.txt" in 
    let processed = process_lines lines in
    print_int (List.fold_left ( + ) 0 processed);
    ()
