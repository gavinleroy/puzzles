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

let solve ops is_newline =
  let overlaps i x =
    let i = Int.rem i 40 in
    (i = x || i = x + 1 || i = x - 1) in
  let rec inner i x = function
    | [] -> ()
    | op :: rest ->
      begin
        if is_newline i then
          print_newline ();
        if overlaps i x then
          print_char '#'
        else
          print_char ' ';
        match op with
        | `Noop -> inner (i + 1) x  rest
        | `Set q -> inner (i + 1) (x + q) rest
      end
  in
  inner 0 1 ops

let () =
  read_lines "input.text"
  |> parse
  |> (fun ps ->
      solve ps (fun c ->
          List.memq c [40; 80; 120; 160; 200; 240]))
