let rec read_lines channel acc =
    try
        let line = input_line channel in read_lines channel (line :: acc)
    with End_of_file -> 
        close_in channel;
        List.rev acc;;
(*
let explode s =
    let size = String.length s in
let rec exp i (l1, l2) = if i < 0 then (l1, l2) else if i >= (size / 2) then exp (i - 1) (s.[i] :: l1, l2) else exp(i - 1) (l1, s.[i] :: l2) in
    exp (size - 1) ([], []);;
*)

let explode s =
  let size = String.length s in
  let rec exp i (l1, l2) = if i < 0 then (l1, l2) else if i >= (size / 2) then exp (i - 1) (s.[i] :: l1, l2) else exp(i - 1) (l1, s.[i] :: l2) in
  exp (size - 1) ([], []);;

let do_the_thing s = let lst1, lst2 = explode s in List.filter (fun a -> List.exists (fun e -> e = a) lst2) lst1;; 

let eval c = 
    match c with
    |'a':: _ -> 1
    |'b':: _ -> 2
    |'c' ::_-> 3
    |'d'::_-> 4
    |'e'::_-> 5
    |'f'::_-> 6
    |'g'::_-> 7
    |'h'::_-> 8
    |'i'::_-> 9
    |'j'::_-> 10
    |'k'::_-> 11
    |'l'::_-> 12
    |'m'::_-> 13
    |'n'::_-> 14
    |'o'::_-> 15
    |'p'::_-> 16
    |'q'::_-> 17
    |'r'::_-> 18
    |'s'::_-> 19
    |'t'::_-> 20
    |'u'::_-> 21
    |'v'::_-> 22
    |'w'::_-> 23
    |'x'::_-> 24
    |'y'::_-> 25
    |'z'::_-> 26
    |'A'::_-> 27
    |'B'::_-> 28
    |'C' ::_-> 29
    |'D' ::_-> 30
    |'E' ::_-> 31
    |'F' ::_-> 32
    |'G' ::_-> 33
    |'H' ::_-> 34
    |'I' ::_-> 35
    |'J' ::_-> 36
    |'K' ::_-> 37
    |'L' ::_-> 38
    |'M' ::_-> 39
    |'N' ::_-> 40
    |'O' ::_-> 41
    |'P' ::_-> 42
    |'Q' ::_-> 43
    |'R' ::_-> 44
    |'S' ::_-> 45
    |'T' ::_-> 46
    |'U' ::_-> 47
    |'V' ::_-> 48
    |'W' ::_-> 49
    |'X' ::_-> 50
    |'Y' ::_-> 51
    |'Z' ::_-> 52
    |_-> 0

let () = 
    let in_channel = open_in "data.txt" in
    let lines = read_lines in_channel [] in
    lines |> List.map do_the_thing |> List.fold_left (fun acc a -> acc + eval a) 0 |> string_of_int |> print_endline;;
    
    (* List.iter print_endline lines; *)    
