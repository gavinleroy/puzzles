(* Yorel Nivag ~ JULI LC 21 *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int () = create_reader " %d " id
let read_long () = create_reader " %Ld " id
let read_float () = create_reader " %f " id
let read_ipair () = create_reader " %d %d " (fun a b -> (a, b))
(* ~~~~~~~~~~~~~~~~~~~ helper functions ~~~~~~~~~~~~~~~~~~~~~~~ *)
let ( ** ) = Int64.mul
let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( // ) = Int64.div
let ( %% ) = Int64.rem
let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let () =
  let cases = read_int () in
  let ds = 7 in
  for case = 1 to cases do
    let (d,x) = read_ipair () in
    let (y,z) = read_ipair () in
    let opt1 = x * ds in
    let opt2 = y * d + z * (ds - d) in
    max opt1 opt2 |> Printf.printf "%d\n"
  done
