(* Kattis, Kornislav *)

exception Impossible

let read_int_list () =
  let line = read_line () in
  let ints = Str.split (Str.regexp " ") line in
  List.map int_of_string ints

let () =
  let nums = read_int_list () in
  match List.sort compare nums with
  | [a; _; c; _] -> print_int (a * c)
  | _ -> raise Impossible
