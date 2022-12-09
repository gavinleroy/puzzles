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

let step knots dir =
  let vec_add (a, b) (x, y) = (a + x, b + y) in
  let move hp tp =
    let d = hp - tp in
    tp + (if abs d = 2 then d / 2 else d)
  in
  let rec inner ls = match ls with
    | (a, b) :: (x, y) :: rest ->
      let mx_d = max (a - x |> abs) (b - y |> abs) in
      let upd = if mx_d > 1 then
          (* XXX: using the previous `before` *)
          (* location doesn't work anymore, *)
          (* I blame physics for making things *)
          (* more complicated *)
          move a x, move b y
        else (x, y)
      in
      match rest with
      | [] -> [upd]
      | rest -> upd :: (inner (upd :: rest))
  in
  let head = List.hd knots |> (fun h -> vec_add h dir) in
  head :: (inner (head :: List.tl knots))

let solve ds =
  let initial_pos = (0, 0) in
  let list_last = fun l -> List.rev l |> List.hd in
  let (s, knots) =
    List.fold_left (fun (set, knots) d ->
        let knots' = step knots d in
        let p = list_last knots' in
        let new_set = S.add p set in
        (new_set, knots'))
      (S.empty, List.init 10 (fun _ -> initial_pos))
      ds
  in
  S.add (list_last knots) s
  |> S.add initial_pos

let () =
  let input = parse_input () in
  solve input
  |> S.cardinal
  |> Printf.printf "%d\n";
