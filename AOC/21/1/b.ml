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
  (try Some (input_line stdin)
   with _ -> None)
  |> function
  | (Some s) -> int_of_string_opt s
  | None  -> None

let rec fold_ans v o1 o2 o3 o4 =
  match o1, o2, o3, o4 with
  | (Some c1), (Some _), (Some _), (Some c4) ->
    fold_ans (v + pt c1 c4)
      o2 o3 o4 (get_int ())
  | _, _, _, None -> v
  | _ -> raise Impossible

and pt v1 v4 =
  if v1 < v4 then
    1
  else 0

let () =
  let f = get_int ()
  and s = get_int ()
  and t = get_int ()
  and l = get_int () in
  fold_ans 0 f s t l
  |> print_int
