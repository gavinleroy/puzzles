(* Programming Template OCaml
 * ~ Gavin Gray ~ *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int () = create_reader "%d" id
let read_long () = create_reader "%Ld" id
let read_float () = create_reader "%f" id
let read_ipair () = create_reader "%d %d" (fun a b -> (a, b))
(* ~~~~~~~~~~~~~~~~~~~ helper functions ~~~~~~~~~~~~~~~~~~~~~~~ *)
let ( ** ) = Int64.mul
let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( // ) = Int64.div
let ( %% ) = Int64.rem
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let () =
  let iterations = read_int () in
  let start = 2 in
  let rec f n v = if n=0 then v
    else f (n-1) (v+v-1) in
  let ans = f iterations start in
  print_int (ans * ans)
