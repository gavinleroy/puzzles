(* Gavin Gray AOC 21 *)

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

let board_equal (arr, marked) (arr', marked') =
  List.length marked = List.length marked'
  && List.fold_left2 (fun acc a b -> acc && a = b) true marked marked'

let list_diff l1 l2 =
  List.filter (fun x ->
      not (List.mem x l2)) l1

let rec play nums boards winners = match nums with
  | (num :: nums) ->
    (let new_boards = List.map (apply_number num) boards in
     List.find_all (is_winner) new_boards
     |> function
     | [] -> play nums new_boards winners
     | ws ->
       play nums
         (list_diff new_boards ws)
         (List.map (fun b -> get_score b * num) ws
          |> (fun l -> List.append l winners)))
  | [] -> List.hd winners

let () =
  let (nums, boards) = read_init () in
  play nums boards []
  |> Printf.printf "%d\n"
