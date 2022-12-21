(* Gavin Gray, AOC 2022 Day 14 *)

module Posn = struct
  type t = (int * int)
  let compare (a, b) (x, y) =
    let c = Int.compare a x in
    if c = 0 then
      Int.compare b y
    else c
  let fst (a, _) = a
  let snd (_, b) = b
  let down (a, b) = (a, b + 1)
  let diag_left (a, b) = (a - 1, b + 1)
  let diag_right (a, b) = (a + 1, b + 1)
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

(* Gross, lots of weird parsing *)
let parse lines : S.t =
  (* fill between two points *)
  let fill_between s (a, b) (a', b') =
    let rec fill mk beg ed =
      List.init (ed - beg + 1) (fun i -> beg + i)
      |> List.fold_left (fun acc v -> S.add (mk v) acc) s
    in
    (* fixed col, row fill *)
    if a = a' then
      let start = min b b' in
      let ed = max b b' in
      fill (fun v -> a, v) start ed
      (* fixed row, column fill *)
    else if b = b' then
      let start = min a a' in
      let ed = max a a' in
      fill (fun v -> v, b) start ed
    else failwith "diagonal line unexpected"
  in
  (* process a single row *)
  let rec row s = function
    | [] | [_] -> s
    | p1 :: _arrow :: p2 :: ls' -> begin
        let destructure s =
          String.split_on_char ',' s |> List.map int_of_string
        in
        let [a; b] = destructure p1 in
        let [x; y] = destructure p2 in
        let s' = fill_between s (a, b) (x, y) in
        row s' (p2 :: ls')
      end
  in
  (* process each line *)
  let rec inner s = function
    | [] -> s
    | l :: ls' ->
      let s' =
        String.split_on_char ' ' l
        |> row s
      in
      inner s' ls'
  in inner S.empty lines

let solve filled =
  let initial_posn = (500, 0) in
  let max_row = S.fold (fun pn m -> Posn.snd pn |> max m) filled Int.min_int in
  let floor = max_row + 2 in
  let rec drop_sand curr_p s =
    let memq (a, b) = floor <= b || S.mem (a, b) s in
    let down = Posn.down curr_p in
    let left = Posn.diag_left curr_p in
    let right = Posn.diag_right curr_p in
    if memq initial_posn then
      None
    else if not(memq down) then
      drop_sand down s
    else if not(memq left) then
      drop_sand left s
    else if not(memq right) then
      drop_sand right s
    else
      let s' = S.add curr_p s in
      Some (s', s')
  in
  Seq.unfold (drop_sand initial_posn) filled
  |> List.of_seq
  |> List.length

let () =
  read_lines "input.text"
  |> parse
  |> solve
  |> Printf.printf "%d\n"
