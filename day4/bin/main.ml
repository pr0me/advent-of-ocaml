
let process_line content =
  let all_numbers = Str.split (Str.regexp "[ ]+") (List.hd (List.tl (Str.split (Str.regexp ": ") content))) in

  let parse_nums lst =
    let rec aux acc = function
      | [] -> List.rev acc (* Reverse the accumulator to maintain the original order *)
      | hd :: tl ->
        try
          let num = int_of_string hd in
          aux (num :: acc) tl
        with Failure _ ->
          List.rev acc
    in
    aux [] lst
  in

  let sp_nums = parse_nums all_numbers in
  ignore (List.iter (fun el -> Printf.printf "%d " (el)) sp_nums);
  (* let winners = List.iter *)
  Printf.printf "\n";
  
  1

let get_valid_games filename = 
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (acc + ((process_line line)))
    | exception End_of_file -> close_in ic; acc
  in
  loop 0


let () = 
  let sum = get_valid_games "scratchcards.txt" in
  Printf.printf "Score: %d\n" sum