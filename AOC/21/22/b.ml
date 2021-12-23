(* Gavin Gray AOC 21 *)

type posn = Int64.t * Int64.t * Int64.t
type cuboid = posn * posn
type action = On | Off
type instr = action * cuboid

(* Parsing *)

let rec input_lines () =
  try let l = read_line () in
    l :: input_lines ()
  with _ -> []

let parse_instruction s : instr =
  let parse = fun a x1 x2 y1 y2 z1 z2 ->
    if a = "on" then
      (On, ((x1, y1, z1), (x2, y2, z2)))
    else (Off, ((x1, y1, z1), (x2, y2, z2)))
  in
  Scanf.sscanf s "%s x=%Ld..%Ld,y=%Ld..%Ld,z=%Ld..%Ld " parse

(* Logic *)

let (++) = Int64.add
let (--) = Int64.sub
let (<**) = Int64.mul

let put_inside v optional =
  match optional with
  | None -> None
  | Some d -> Some (v, d)

let flip = function
  | On -> Off
  | Off -> On

let volume ((x1, y1, z1), (x2, y2, z2)) =
  (x2 -- x1 ++ Int64.one) <**
  (y2 -- y1 ++ Int64.one) <**
  (z2 -- z1 ++ Int64.one)

let rec intersect_cuboid
    ((x1, y1, z1), (x2, y2, z2))
    ((x1', y1', z1'), (x2', y2', z2')) =
  if not (x2 < x1' || x2' < x1 ||
          y2 < y1' || y2' < y1 ||
          z2 < z1' || z2' < z1)
  then Some ((max x1 x1', max y1 y1', max z1 z1'),
             (min x2 x2', min y2 y2', min z2 z2'))
  else None

let count_on_cubes ls =
  let take_action s (act, c) =
    match act with
    | On -> s ++ volume c
    | Off -> s -- volume c
  in
  let rec loop seen on ls =
    match ls with
    | [] -> on
    | nw  :: tl ->
      let (act, new_cuboid) = nw in
      List.filter_map (fun (act', otherc) ->
          put_inside (flip act')
            (intersect_cuboid new_cuboid otherc)) seen
      |> (fun intscts ->
          List.fold_left take_action on intscts
          |> fun new_on ->
          match act with
          | On -> loop (intscts @ nw :: seen)
                    (take_action new_on nw) tl
          | Off -> loop (intscts @ seen) new_on tl)
  in loop [] Int64.zero ls

let () =
  input_lines ()
  |> List.map parse_instruction
  |> count_on_cubes
  |> Printf.printf "%Ld\n"
