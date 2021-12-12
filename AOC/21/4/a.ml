(* Gavin Gray AOC 21 *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int _ = create_reader " %d " id
let read_long _ = create_reader " %Ld " id
let read_float _ = create_reader " %f " id
let read_ipair _ = create_reader " %d %d " (fun a b -> (a, b))
(* ~~~~~~~~~~~~~~~~~~~ helper functions ~~~~~~~~~~~~~~~~~~~~~~~ *)
let ( ** ) = Int64.mul
let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( // ) = Int64.div
let ( %% ) = Int64.rem
let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

exception Impossible

type pair = (int * int)
and board = (pair option Array.t * pair List.t)

let string_to_int s = int_of_string s

let read_nums () =
  read_line ()
  |> String.split_on_char ','
  |> List.map string_to_int

let read_board () =
  let arr = Array.make 100 None in
  for i = 0 to pred 5 do
    read_line ()
    |> Str.split (Str.regexp "[ \x20\t]+")
    |> List.iteri (fun j s ->
        let n = (string_to_int s) in
        arr.(n) <- (Some (i, j)))
  done;
  arr, []

let read_init () =
  let rec loop acc =
    try
      begin
        ignore(read_line ());
        loop (read_board () :: acc)
      end
    with _ -> acc
  in
  let nums = read_nums () in
  nums, loop []

let get_score (arr, _) =
  let score = ref 0 in
  Array.iteri (fun i v -> match v with
      | None -> ()
      | Some _ -> score := !score + i) arr;
  !score

let is_winner (_, marked) =
  List.fold_left (fun acc row ->
      acc || (List.filter (fun (r, _) -> r = row) marked
              |> List.length) = 5) false [0; 1; 2; 3; 4]
  ||
  List.fold_left (fun acc col ->
      acc || (List.filter (fun (_, c) -> c = col) marked
              |> List.length) = 5) false [0; 1; 2; 3; 4]

(* mark the spot /by putting a None there/ *)
let apply_number n (arr, marked) =
  let m = match arr.(n) with
    | None -> marked
    | Some (r, c) ->
      begin
        arr.(n) <- None;
        (r, c) :: marked
      end in
  (arr, m)

let rec play nums boards = match nums with
  | (num :: nums) ->
    (let new_boards = List.map (apply_number num) boards in
     List.find_opt (is_winner) new_boards
     |> function
     | None -> play nums new_boards
     | Some b -> get_score b * num)
  | [] -> raise Impossible

let () =
  let (nums, boards) = read_init () in
  play nums boards
  |> print_int
