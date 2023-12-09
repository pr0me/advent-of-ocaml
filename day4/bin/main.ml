
let process_line content =
  let all_numbers = Str.split (Str.regexp "[ ]+") (List.hd (List.tl (Str.split (Str.regexp ": ") content))) in

  let parse_nums lst =
    let rec aux acc = function
      | [] -> List.rev acc
      | hd :: tl ->
        try
          let num = int_of_string hd in
          aux (num :: acc) tl
        with Failure _ ->
          List.rev acc
    in
    aux [] lst
  in

  let winners = parse_nums all_numbers in
  let entries = List.rev (parse_nums (List.rev all_numbers)) in

  ignore (List.iter (fun el -> Printf.printf "%d " (el)) winners);
  Printf.printf "\n";
  ignore (List.iter (fun el -> Printf.printf "%d " (el)) entries);
  Printf.printf "-------------------------";
  Printf.printf "\n";

  let count_matches w e =
    let length = List.length w in
    let count_match count entry =
      if List.mem entry w then
        if count > length then
          count
        else
          count + 1
      else
        count
    in
    List.fold_left count_match 0 e
  in

  let hits = count_matches winners entries in
  if hits > 0 then
    2.0 ** float_of_int (hits - 1)
  else
    0.0

let get_valid_games filename = 
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (acc + (int_of_float (process_line line)))
    | exception End_of_file -> close_in ic; acc
  in
  loop 0


let () = 
  let sum = get_valid_games "scratchcards.txt" in
  Printf.printf "Score: %d\n" sum