(* Kattis, Karte *)

exception Impossible
exception Dup

let suit_num suit =
  if suit='P' then 0
  else if suit='K' then 1
  else if suit='H' then 2
  else if suit='T' then 3
  else raise Impossible

let get_num c1 c2 =
  let n1 = int_of_char c1 - 48 in
  let n2 = int_of_char c2  - 48 in
  n1 * 10 + n2

let conv (suit : char) c1 c2  =
  let num = get_num c1 c2 in
  num + 13 * suit_num suit

let solve inp =
  let seen = Array.make 200 false in
  let counts = Array.make 4 13 in
  let f = fun i c ->
    if (i mod 3)<>0 then ()
    else
      (counts.(suit_num c) <- counts.(suit_num c)-1;
       let seen_id = conv c inp.[i+1] inp.[i+2] in
       if seen.(seen_id) then
         raise Dup
       else seen.(seen_id) <- true) in
  String.iteri f inp;
  Printf.printf "%d %d %d %d\n" counts.(0) counts.(1) counts.(2) counts.(3)

let () =
  let input = read_line () in
  try solve input with
  | Dup -> print_endline "GRESKA"
