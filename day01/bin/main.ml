
let rec read_lines channel acc =
  try
    let line = input_line channel in
    read_lines channel (line :: acc)  (* Append the line to the accumulator *)
  with End_of_file ->
    close_in channel;                (* Close the channel when done reading *)
    List.rev acc                     (* Reverse the list to get the correct order *)

(*
let rec find_max lst acc curr=
  match lst with
  |[] -> Int.max acc curr
  |""::tl -> find_max tl (Int.max acc curr) 0
  |h::tl -> find_max tl acc (curr + int_of_string h);; 
*)

let get_top_three lst =
  match lst with 
  |h1 :: h2 :: h3 :: _ -> print_int (h1 + h2 + h3);
  |_-> print_int 0 

let rec acc_sums lst acc curr =
  match lst with
  |[] -> curr :: acc
  |"" :: tl -> acc_sums tl (curr :: acc) 0
  |h :: tl -> acc_sums tl acc (curr + int_of_string h);;

let () =
  let in_channel = open_in "data.txt" in
  let lines = read_lines in_channel [] in
  List.sort (fun a b -> b - a) (acc_sums lines [] 0) |> get_top_three;;
  (* print_int (find_max lines 0 0);; *)
  (* List.iter print_endline lines; *)    (* Print the lines to the standard output *)


