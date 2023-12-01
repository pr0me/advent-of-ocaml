let numeral_map = [("one", 1); ("two", 2); ("three", 3); ("four", 4); ("five", 5); ("six", 6); ("seven", 7); ("eight", 8); ("nine", 9);]

let is_numeral c =
  let code = Char.code c in
  code >= 48 && code <= 57

let find_numeral_word s start =
  let s_len = String.length s in
  let rec aux i word =
    if i >= s_len then
      if List.mem_assoc word numeral_map then
        Some (List.assoc word numeral_map), i - 1
      else
        None, i - 1
    else match s.[i], word with
    | 'a'..'z', _ ->
        let new_word = word ^ String.make 1 s.[i] in
        if List.mem_assoc new_word numeral_map then
          Some (List.assoc new_word numeral_map), i + 1
        else
          aux (i + 1) new_word
    | _, "" -> aux (i + 1) ""
    | _, _ -> None, i
  in
  aux start ""
  

let find_right s =
  let rec aux i =
    if i < 0 then None
    else if is_numeral s.[i] then Some (Char.code s.[i] - Char.code '0')
    else let word, _ = find_numeral_word s i in
      if word != None then word else aux (i - 1)
  in
  aux (String.length s - 1)

let find_left s =
  let rec aux i =
    if i >= String.length s then None
    else if is_numeral s.[i] then Some (Char.code s.[i] - Char.code '0')
    else let word, _ = find_numeral_word s i in
          if word != None then word else aux (i + 1)
  in
  aux 0

let process_line line =
  match find_left line, find_right line with
  | Some left, Some right -> int_of_string (string_of_int left ^ string_of_int right)
  | _ -> 0

let sum_from_file filename =
  let ic = open_in filename in
  let rec loop acc =
    match input_line ic with
    | line -> loop (acc + process_line line)
    | exception End_of_file -> close_in ic; acc
  in
  loop 0

let () =
  let sum = sum_from_file "input.txt" in
  Printf.printf "Total sum: %d\n" sum
