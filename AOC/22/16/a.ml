(* Gavin Gray, AOC 2022 day 16 *)

module G = Map.Make(String)
module S = Set.Make(String)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

type node =
  { value : int64
  ; neighbors : string list
  }

let parse ls =
  List.map (fun l ->
      try
        Scanf.sscanf l "Valve %s has flow rate=%Ld; tunnels lead to valves %s@\n"
          (fun room rate adjacent ->
             let adj_list = String.split_on_char ',' adjacent
                            |> List.map String.trim in
             room, rate, adj_list)
      with (* gross *)
        Scanf.Scan_failure _ ->
        Scanf.sscanf l "Valve %s has flow rate=%Ld; tunnel leads to valve %s"
          (fun room rate adjacent ->
             let adj_list = [adjacent] in
             room, rate, adj_list)) ls
  |> List.fold_left (fun graph (name, rate, adj_list) ->
      let name' = String.concat "-" [name; "value"] in
      let adj_list' = name' :: adj_list in
      G.add name { value=0L; neighbors=adj_list'; } graph
      |> G.add name' { value=rate; neighbors=adj_list; }
    ) G.empty

let solve graph =
  let ( ++ ) = Int64.add in
  let ( ** ) = Int64.mul in
  let hash =
    let get =
      let idx = ref 0 in
      fun _ -> let v = !idx in incr idx; v
    in
    let hashes = G.map get graph in
    fun v -> G.find v hashes
  in
  let unhash v =
    G.bindings graph
    |> List.map fst
    |> List.find (fun k -> hash k = v)
  in
  let vertices = G.bindings graph |> List.map (fun v -> fst v |> hash) in
  let num_vertices = List.length vertices in
  let infty = 1_000_000 in
  let dist = Array.make_matrix num_vertices num_vertices infty in

  let print_dist () =
    Array.iteri (fun f a ->
        Printf.printf "Shortest from %s:\n" (unhash f);
        Array.iteri (fun t v ->
            Printf.printf "  to %s: %d\n"
              (unhash t)
              v
          ) a;
      ) dist;
  in

  begin
    (* Floyd-Warshall for finding node distances *)
    G.iter (fun k { value; neighbors; } ->
        let ki = hash k in
        (* all edge weights are set to 1 *)
        List.iter (fun neighbor ->
            dist.(ki).(hash neighbor) <- 1) neighbors;
        (* distance to self => 0 *)
        dist.(ki).(ki) <- 0;) graph;
    List.iter (fun k ->
        List.iter (fun i ->
            List.iter (fun j ->
                if dist.(i).(j) > dist.(i).(k) + dist.(k).(j) then
                  dist.(i).(j) <- dist.(i).(k) + dist.(k).(j);)
              vertices)
          vertices)
      vertices
  end;

  let distance f t =
    let fk = hash f
    and tk = hash t in
    dist.(fk).(tk)
  in
  let high_value_vertices = G.fold (fun k { value; neighbors; } acc ->
      if value > 0L then
        k :: acc
      else acc) graph []
  in
  let maximum = List.fold_left max 0L
  in
  let rec loop here visited = function
    | n when n <= 0 -> 0L
    | time_left ->
      let flow = (G.find here graph).value in
      let visited' = S.add here visited in
      let next_moves = List.filter_map (fun v ->
          if S.mem v visited' then
            None
          else
            let d = distance here v in
            Some (loop v visited' (time_left - d))) high_value_vertices
      in
      flow ** (Int64.of_int time_left) ++ maximum next_moves
  in
  loop "AA" S.empty 30

let () =
  read_lines "input.text"
  |> parse
  |> solve
  |> Printf.printf "%Ld\n"
