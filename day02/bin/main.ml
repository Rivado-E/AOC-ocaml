let rec read_lines channel acc =
  try
    let line = input_line channel in
    read_lines channel (line :: acc)  (* Append the line to the accumulator *)
  with End_of_file ->
    close_in channel;                (* Close the channel when done reading *)
    List.rev acc;;                    (* Reverse the list to get the correct order *)

let eval round = 
    match round with
    |"A X"-> (1, 3 + 0) 
    |"A Y"-> (1, 1 + 3)
    |"A Z"-> (7, 2 + 6) 
    |"B X"-> (8, 1 + 0)
    |"B Y"-> (5, 2 + 3)
    |"B Z"-> (2, 3 + 6)
    |"C X"-> (3, 2 + 0)
    |"C Y"-> (9, 3 + 3)
    |"C Z"-> (6, 1+ 6)
    |_-> failwith "wrong input"

let rec part_one lst a b =
    match lst with
    |[] -> b
    |h :: tl -> let (x, y) =  eval h in part_one tl (x + a) (y + b)

let () =
  let in_channel = open_in "data.txt" in
  let lines = read_lines in_channel [] in
    print_int (part_one lines 0 0);; 
