(* Yorel Nivag ~ JULI 21 LC *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int _ = create_reader " %d " id
let read_long _ = create_reader " %Ld " id
let read_float _ = create_reader " %f " id
let read_ipair _ = create_reader " %d %d " (fun a b -> (a, b))
let read_lpair _ = create_reader " %Ld %Ld " (fun a b -> (a, b))
(* ~~~~~~~~~~~~~~~~~~~ helper functions ~~~~~~~~~~~~~~~~~~~~~~~ *)
let ( ** ) = Int64.mul
let ( ++ ) = Int64.add
let ( -- ) = Int64.sub
let ( // ) = Int64.div
let ( %% ) = Int64.rem
let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let mod' = 1000000007L

let max_n = 100001

let sums = Array.make max_n 0L

(* compute (x ^ y) % p *)
let pow x y p =
  let rec loop res x y =
    if y <= 0L then res
    else Int64.(
        let y' = y // 2L in
        let x' = (x ** x) %% p in
        let res' = (if Int64.logand y 1L <> 0L
                    then (res ** x) %% p
                    else res) in
        loop res' x' y') in
  loop 1L (x %% p) y

let to_ith_n i =
  if i // 10L = 0L then i
  else let string_rev s =
         let len = String.length s in
         String.init len (fun i -> s.[len - 1 - i]) in
    let tail = Int64.to_string (i // 10L) in
    let full = (Int64.to_string i) ^ (string_rev tail) in
    Int64.of_string full

let fill_sums () =
  for i = 1 to max_n - 1 do
    sums.(i) <- sums.(i-1) ++ (Int64.of_int i |> to_ith_n);
    assert (sums.(i) > sums.(i-1))
  done

let () =
  fill_sums ();
  let cases = read_int () in
  for case = 1 to cases do
    let (l, r) = read_ipair () in
    let base = to_ith_n (Int64.of_int l) in
    (pow base (sums.(r) -- sums.(l)) mod')
    |> Printf.printf "%Ld\n"
  done
