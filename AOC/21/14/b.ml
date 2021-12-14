(* Gavin Gray AOC 21 *)

module PMap = Map.Make(struct
    type t = char * char
    let compare (c1, c2) (c1', c2') =
      if c1 = c1' then
        Char.compare c2 c2'
      else Char.compare c1 c1'
  end)

let rec lines_as_list () =
  try
    let l = read_line () in
    l :: lines_as_list ()
  with _ -> []

let list_to_rules =
  List.fold_left (fun acc l ->
      PMap.add (l.[0], l.[1]) l.[6] acc) PMap.empty

let alph = Array.make 26 0

let inc_a c v =
  let i = Char.code c - 65 in
  alph.(i) <- alph.(i) + v

let score () =
  Array.to_seq alph
  |> Seq.filter (fun c ->
      c > 0)
  |> List.of_seq
  |> List.sort Int.compare
  |> (fun ls ->
      let cmin = List.hd ls
      and cmax = List.rev ls
                 |> List.hd in
      cmax - cmin)

let formula_to_init s =
  String.to_seqi s
  |> Seq.fold_left (fun acc (i, c) ->
      begin
        inc_a c 1; (* HACK! *)
        if i + 1 < String.length s then
          PMap.update (c, s.[i + 1]) (function
              | Some v -> Some (v + 1)
              | None -> Some 1) acc
        else acc
      end) PMap.empty

let expand counts rules =
  PMap.fold (fun (c1, c2) cnt acc ->
      let c3 = PMap.find (c1, c2) rules in
      let p1 = (c1, c3)
      and p2 = (c3, c2)
      and updf = (function
          | Some c -> Some (c + cnt)
          | None -> Some cnt) in
      begin
        inc_a c3 cnt; (* HACK! *)
        PMap.update p1 updf
          (PMap.update p2 updf acc)
      end) counts PMap.empty

let () =
  let formula = read_line ()
                |> formula_to_init
  and _ = read_line ()
  and rules = lines_as_list ()
              |> list_to_rules
  and times = List.init 40 (fun _ -> 0) in
  let _ = List.fold_left (fun acc _ ->
      expand acc rules) formula times in
  score ()
  |> Printf.printf "%d\n"
