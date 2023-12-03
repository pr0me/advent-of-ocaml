let calculate_total_sum filename =
  let r_max = 12 in
  let g_max = 13 in
  let b_max = 14 in

  let rec process_lines file game_number total_sum =
    try
      let line = input_line file in
      let games = Str.split (Str.regexp "; ") line in
      let max_counts =
        List.map (fun game ->
            let counts = Str.split (Str.regexp ", ") game in
            let count_color color =
              let color_counts = List.filter (fun s -> String.contains s color.[0]) counts in
              let color_values = List.map (fun s -> int_of_string s) color_counts in
              let max_color = List.fold_left max 0 color_values in
              max_color
            in
            let max_r = count_color "red" in
            let max_g = count_color "green" in
            let max_b = count_color "blue" in
            max_r, max_g, max_b
          ) games
      in
      let all_below_max = List.for_all (fun (max_r, max_g, max_b) ->
          max_r < r_max && max_g < g_max && max_b < b_max) max_counts
      in
      let total_sum' = if all_below_max then total_sum + game_number else total_sum in
      process_lines file (game_number + 1) total_sum'
    with End_of_file ->
      total_sum
  in

  let file = open_in filename in
  let total_sum = process_lines file 1 0 in
  close_in file;
  total_sum
;;

let result = calculate_total_sum "clean_games.txt" in
Printf.printf "Total sum of game numbers: %d\n" result;

(* let _ = List.iter (Printf.printf "%s\n") counts in *)