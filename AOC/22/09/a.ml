(* Gavin Gray, AOC 22 Day 9 *)

module Posn = struct
  type t = (int * int)
  let compare (a, b) (x, y) =
    let c = Int.compare a x in
    if c = 0 then
      Int.compare b y
    else c
  let print (a, b) =
    Printf.printf "(%d %d)" a b
end

module S = Set.Make(Posn)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []


let parse_input () : Posn.t list =
  let d_to_vec = function
    | "R" -> (0, 1)
    | "L" -> (0, -1)
    | "U" -> (1, 0)
    | "D" -> (-1, 0)
  in
  read_lines "input.text"
  |> List.map (fun l ->
      let [f; s] = String.split_on_char ' ' l in
      let i = int_of_string s in
      let v = d_to_vec f in
      List.init i (fun _ -> v))
  |> List.flatten

let step head (x, y) dir =
  let vec_add (a, b) (x, y) = (a + x, b + y) in
  let move hp tp =
    let d = hp - tp in
    tp + (if abs d = 2 then d / 2 else d)
  in
  let (a', b') = vec_add head dir in
  let mx_d = max (a' - x |> abs) (b' - y |> abs) in
  let new_tail =
    if mx_d > 1 then
      head
    else (x, y)
  in
  (a', b'), new_tail

let solve ds =
  let initial_pos = (0, 0) in
  let (s, h, t) =
    List.fold_left (fun (set, head, tail) d ->
        let (new_head, new_tail) = step head tail d in
        let new_set = S.add new_tail set in
        (new_set, new_head, new_tail))
      (S.empty, initial_pos, initial_pos)
      ds
  in
  S.add t s
  |> S.add initial_pos

let () =
  let input = parse_input () in
  solve input
  |> S.cardinal
  |> Printf.printf "%d\n";
