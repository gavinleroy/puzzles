(* Kattis - quick brown fox *)

let is_alpha = function
  'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

let diff l1 l2 =
  List.filter (fun x -> not (List.mem x l2)) l1

let str_to_list s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let (^$) c s = s ^ Char.escaped c (* append *)
let ($^) c s = Char.escaped c ^ s (* prepend *)

let rec str_of_list = function
  | [] -> ""
  | s :: cs -> s $^ (str_of_list cs)

let alphabet () =
  "abcdefghijklmnopqrstuvwxyz"
  |> str_to_list

let () =
  let t = read_int () in
  for _ = 1 to t do
    let line = read_line () in
    let str = str_to_list line
              |> List.filter is_alpha
              |> List.map Char.lowercase_ascii
              |> List.sort_uniq Char.compare in
    let res = diff (alphabet ()) str in
    print_endline (match res with
        | [] -> "pangram"
        | ss -> "missing " ^ (str_of_list ss))
  done
