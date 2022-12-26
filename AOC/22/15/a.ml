(* Gavin Gray, AOC 2022 day 15 *)

module Posn = struct
  type t = (int * int)
  let compare (a, b) (x, y) =
    let c = Int.compare a x in
    if c = 0 then
      Int.compare b y
    else c
  let fst (a, _) = a
  let snd (_, b) = b
  let dist (a, b) (x, y) =
    abs ( x - a ) + abs (y - b)
  let to_string (a, b) =
    Printf.sprintf "(%d %d)" a b
  let print p =
    Printf.printf "%s\n" (to_string p)
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

let parse =
  List.map (fun l ->
      Scanf.sscanf l "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d"
        (fun sx sy bx by -> [(sx, sy); (bx, by)]))

let range_for lss y =
  let rec collapse = function
    | (a, b) :: (x, y) :: ls ->
      if a <= x && x <= b then
        collapse ((a, max b y) :: ls)
      else (a, b) :: (collapse ((x, y) :: ls))
    | o -> o
  in
  List.filter_map (fun (s, b, r) ->
      let yd = abs (y - Posn.snd s) in
      if yd > r then
        None
      else
        let d = r - yd
        and m = Posn.fst s in
        Some (m - d, m + d)) lss
  |> List.sort (fun (x, _) (x', _) -> Int.compare x x')
  |> collapse

let solve lss =
  let z = 2000000 in
  let beacons_on_row =
    List.filter_map (function
        | (_, b, _) when Posn.snd b = z -> Some b
        | _ -> None) lss
    |> S.of_list
  in
  let inner (x, y) =
    let s = y - x + 1 in
    let c = S.filter (fun (a, _) -> x <= a && a <= y) beacons_on_row
            |> S.cardinal in
    s - c
  in
  range_for lss z
  |> List.map inner
  |> List.fold_left (+) 0

let () =
  read_lines "input.text"
  |> parse
  |> List.map (fun [s; b] -> (s, b, Posn.dist s b))
  |> solve
  |> Printf.printf "%d\n"
