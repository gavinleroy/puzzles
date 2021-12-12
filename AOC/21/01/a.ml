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

let get_int () =
  (try Some
         (input_line stdin)
   with _ -> None)
  |> function
  | (Some s) -> int_of_string_opt s
  | None  -> None

let rec fold_ans v o1 o2 = match o1, o2 with
  | (Some p), (Some c) ->
    get_int ()
    |> fold_ans (if p < c then
                   v + 1
                 else v) o2
  | _, None -> v
  | _ -> raise Impossible

let () =
  let f = get_int ()
  and s = get_int () in
  fold_ans 0 f s
  |> print_int
