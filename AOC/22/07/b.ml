(* Gavin Gray, AOC 2022 day 7 *)


module SS = Set.Make(String)


let fs = Hashtbl.create 100000

let default: (int64 * SS.t * SS.t) =
  (Int64.zero, SS.empty, SS.empty)

let read_lines name : string list =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None -> close_in ic; List.rev acc in
  loop []

let get_it hsh =
  let curr_opt = Hashtbl.find_opt fs hsh in
  Option.value curr_opt ~default:default

let register_files cwd new_fs =
  let hsh = String.concat "/" cwd in
  let (size, files, dirs) = get_it hsh in
  let new_f  = List.filter (fun (s, f) -> not (SS.mem f files)) new_fs in
  let n = List.map fst new_f
          |> List.fold_left (Int64.add) size in
  let fss = List.map snd new_f |> List.to_seq in
  Hashtbl.replace fs hsh (n, SS.add_seq fss files, dirs)

let register_dirs cwd new_dirs =
  let hsh = String.concat "/" cwd in
  let (size, files, dirs) = get_it hsh in
  let ds = List.to_seq new_dirs in
  Hashtbl.replace fs hsh (size, files, SS.add_seq ds dirs)

let get_output trace =
  let is_digit = function '0' .. '9' -> true | _ -> false in
  let rec inner acc rest =
    match rest with
    | [] -> (acc, rest)
    | h :: t ->
      if String.get h 0 = '$' then
        (acc, rest)
      else
        inner (h :: acc) t
  in
  let (o, rest) = inner [] trace in
  let (fs, dirs) = List.partition (fun l -> String.get l 0 |> is_digit) o in
  let ns = List.map (fun fs ->
      let ns :: fs :: _ = String.split_on_char ' ' fs in
      (Int64.of_string ns, fs)) fs in
  let ds = List.map
      (fun l -> List.nth (String.split_on_char ' ' l) 1) dirs in
  (ns, ds, rest)

let rec parse_cmd curr_dir trace =
  match trace with
  | [] -> ()
  | cmd :: rest ->
    match String.split_on_char ' ' cmd with
    | "$" :: "cd" :: ".." :: [] ->
      parse_cmd (List.tl curr_dir) rest
    | "$" :: "cd" :: new_dir :: [] ->
      parse_cmd (new_dir :: curr_dir) rest
    | "$" :: "ls" :: [] ->
      let (files, dirs, rest) = get_output rest in
      register_files curr_dir files;
      register_dirs curr_dir dirs;
      parse_cmd curr_dir rest

let size_of d =
  let rec inner current =
    let prepend c = String.concat "/" [c; current] in
    let (size, _, dirs) = get_it current in
    let dir_seq = SS.to_seq dirs in
    let dir_ns = Seq.map (fun d -> prepend d |> inner) dir_seq in
    let my_size = Seq.fold_left Int64.add size dir_ns in
    my_size
  in
  inner d

let () =
  let total_size = Int64.sub 70000000L 30000000L in
  let trace = read_lines "input.text" in
  parse_cmd [] trace;
  let needed_size = size_of "/" in
  let to_delete_size = Int64.sub needed_size total_size in
  let big_enough n = n >= to_delete_size in
  (* recomputing values but interpreting still only <1ms  *)
  let sol = Hashtbl.to_seq_keys fs
            |> Seq.map size_of
            |> Seq.filter big_enough
            |> List.of_seq
            |> List.sort Int64.compare
            |> List.hd
  in
  Printf.printf "%Ld\n" sol
