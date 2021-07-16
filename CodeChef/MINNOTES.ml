(* Yorel Nivag ~ JULI 21 LC *)
(* This solution is a lot messier than I had hoped :(  *)



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
let one = Int64.one
let zero = Int64.zero
let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

let rec gcd a b =
  if b=Int64.zero then a
  else gcd b (a %% b)

let solve arr n =
  let forward = Array.make n Int64.zero in
  let backward = Array.make n Int64.zero in
  forward.(0) <- arr.(0);
  backward.(n-1) <- arr.(n-1);
  for i = 1 to (n-1) do
    forward.(i) <- (gcd forward.(i-1) arr.(i));
    backward.(n-i-1) <- (gcd backward.(n-i) arr.(n-i-1))
  done;
  let sum = Array.fold_left (++) Int64.zero arr in
  let ans = ref (min
                   ((sum -- arr.(0) ++ backward.(1)) // backward.(1))
                   ((sum -- arr.(n-1) ++ forward.(n-2)) // forward.(n-2))) in
  Array.iteri (fun idx v ->
      if idx>0 && idx<(n-1) then
        let gcdfb = gcd forward.(idx-1) backward.(idx+1) in
        let ans' = (sum -- v ++ gcdfb) // gcdfb in
        ans := min ans' !ans
      else ()) arr;
  !ans

let () =
  let cases = read_int () in
  for case = 1 to cases do
    let n = read_int () in
    let salaries = Array.init n read_long in
    (if n=1 then one else solve salaries n)
    |> Printf.printf "%Ld\n"
  done
