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

(* A lot slower than I would probably like *)
let solve lss =
  let z = 4000000 in
  for i = 0 to z do
    match range_for lss i with
    | [(a, b); (x, y)]
      when (0 <= b) && (b < z) ->
      let x64 = Int64.of_int (b + 1)
      and y64 = Int64.of_int i in
      Int64.mul x64 4000000L
      |> Int64.add y64
      |> Printf.printf "%Ld\n"
    | _ -> ()
  done

let () =
  read_lines "input.text"
  |> parse
  |> List.map (fun [s; b] -> (s, b, Posn.dist s b))
  |> solve
