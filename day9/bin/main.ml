let process_line content =
  let values = Str.split (Str.regexp " ") content in
  let numerics = List.map int_of_string values in

  let offset_series =
    let rec compute_offsets acc prev = function
    | [] -> List.rev acc
    | hd :: tl ->
      let offset = hd - prev in
      compute_offsets (offset :: acc) hd tl
    in 
    match numerics with
    | [] | [_] -> []
    | hd :: tl -> compute_offsets [] hd tl
  in
  
  ignore (List.iter (fun el -> Printf.printf "%d " (el)) offset_series);
  Printf.printf "\n";

  0


let parse_file filename = 
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (acc + (process_line line))
    | exception End_of_file -> close_in ic; acc
  in
  loop 0


let () = 
  let sum = parse_file "oasis.txt" in
  Printf.printf "\nScore: %d\n" sum