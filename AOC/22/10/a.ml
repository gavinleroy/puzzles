(* Gavin Gray, AOC 2022 day 10 *)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let parse lines =
  List.map (fun l ->
      let ls = String.split_on_char ' ' l in
      if List.length ls = 1 then
        [`Noop]
      else
        [`Noop; `Set (List.nth ls 1 |> int_of_string)]) lines
  |> List.flatten

let solve ops is_interesting =
  let strength = Int.mul in
  let rec inner i x acc = function
    | [] -> acc
    | op :: rest ->
      let curr_strength =
        if is_interesting i then
          strength i x
        else 0
      in
      match op with
      | `Noop -> inner (i + 1) x (acc + curr_strength) rest
      | `Set q -> inner (i + 1) (x + q) (acc + curr_strength) rest
  in
  inner 1 1 0 ops

let () =
  read_lines "input.text"
  |> parse
  |> (fun ps ->
      solve ps (fun c ->
          List.memq c [20; 60; 100; 140; 180; 220]))
  |> Printf.printf "%d\n"
