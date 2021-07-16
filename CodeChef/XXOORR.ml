(* Programming Template OCaml
 * ~ Gavin Gray ~ *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int _ = create_reader " %d " id
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

let ceil_div numerator divisor =
  (numerator + divisor - 1) / divisor

let () =
  let cases = read_int () in
  for case = 1 to cases do
    let (n, k) = read_ipair () in
    let counts = Array.create 32 0 in
    let values = Array.init n read_int in
    Array.iter (fun i ->
        for idx = 0 to 29 do
          Array.set counts idx (counts.(idx) + ((i lsr idx) land 1))
        done ) values;
    let ans = Array.fold_left (fun acc v ->
        acc + (ceil_div v k)) 0 counts in
    Printf.printf "%d\n" ans
  done
