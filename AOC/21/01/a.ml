(* Gavin Gray AOC 21 *)

let rec full_inp () =
  let get_int () =
    (try Some
           (input_line stdin)
     with _ -> None)
    |> function
    | (Some s) -> int_of_string_opt s
    | None  -> None
  in
  match get_int () with
  | Some i -> i :: full_inp ()
  | None -> []

let rec fold_ans v ns = match ns with
  | n1 :: n2 :: tl ->
    fold_ans (if n1 < n2 then
                v + 1
              else v) (n2 :: tl)
  | _ -> v

let () =
  let nums = full_inp () in
  fold_ans 0 nums
  |> Printf.printf "%d\n"
