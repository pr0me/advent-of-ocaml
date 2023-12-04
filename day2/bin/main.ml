let r_allowed = 12
let g_allowed = 13
let b_allowed = 14

let process_line line content =
  let r_max = ref 0 in
  let g_max = ref 0 in
  let b_max = ref 0 in
  let groups = Str.split (Str.regexp "; ") content in
  ignore (List.iter (fun group ->
    let values = Str.split (Str.regexp ", ") group in
    let get_value color = 
      try
        let color_assign = List.find (fun s -> 
          try
            ignore (Str.search_forward (Str.regexp_string color) s 0);
            true
          with Not_found -> false
        ) values in
        let color_value = List.hd (Str.split (Str.regexp " ") color_assign) in
        (* Printf.printf "Color Value: %s %s\n" color color_value; *)
        int_of_string color_value
      with Not_found -> 0
    in

    let red_value = get_value "red" in
    let green_value = get_value "green" in
    let blue_value = get_value "blue" in

    if red_value > !r_max then r_max := red_value;
    if green_value > !g_max then g_max := green_value;
    if blue_value > !b_max then b_max := blue_value;

  ) groups);

  
  
  if !r_max <= r_allowed && !g_max <= g_allowed && !b_max <= b_allowed then
    let _ = Printf.printf "[i] Adding game ID: %d\n" line in
    Printf.printf "Max Red: %d\n" !r_max;
    Printf.printf "Max Green: %d\n" !g_max;
    Printf.printf "Max Blue: %d\n" !b_max;
    line
  else 
    0

let get_valid_games filename = 
  let ic = open_in filename in
  let rec loop i acc =
    match input_line ic with
    | line -> loop (i + 1) (acc + ((process_line (i) line)))
    | exception End_of_file -> close_in ic; acc
  in
  loop 0 0

let () = 
  let sum = get_valid_games "clean_games.txt" in
  Printf.printf "Sum of valid Game IDs: %d\n" sum

