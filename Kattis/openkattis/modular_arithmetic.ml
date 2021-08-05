(* Programming Template OCaml
 * ~ Gavin Gray ~ *)

(* ~~~~~~~~~~~~~~~~~~~~ IO functions ~~~~~~~~~~~~~~~~~~~~~~~~~~ *)
let create_reader fmt f = Scanf.(bscanf Scanning.stdin fmt f)
let id = fun n -> n
let read_int _ = create_reader " %d " id
let read_char _ = create_reader " %c " id
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

type op = | Plus | Sub | Mult | Div

type equation = op * Int64.t * Int64.t

let read_equation () =
  create_reader " %Ld %c %Ld "
    (fun l o r ->
       (if o = '+' then Plus
        else if o = '-' then Sub
        else if o = '*' then Mult
        else if o = '/' then Div
        else assert false), l, r)

let rec gcd_ext a = function
  | 0L -> (1L, 0L, a)
  | b ->
    let s, t, g = gcd_ext b (a %% b) in
    (t, s -- (a // b) ** t, g)

let mod_inv a m =
  let mk_pos x =
    if x < 0L then x ++ m
    else x in
  match gcd_ext a m with
  | i, _, 1L -> mk_pos i
  | _ -> assert false

let compute m (o, l, r) = match o with
  | Plus -> (((l %% m) ++ (r %% m))) %% m
  | Sub -> (((l %% m) -- (r %% m)) ++ m) %% m
  | Mult -> ((l %% m) ** (r %% m)) %% m
  | Div -> ((l %% m) ** (mod_inv r m)) %% m

let solve m = read_equation ()
              |> compute m

let () =
  (* ending test case is 0 0 *)
  let rec loop_out () =
    let (n, t) = read_lpair () in
    let rec loop i =
      if i < t then
        begin
          (try
             solve n
           with _ -> -1L)
          |> Printf.printf "%Ld\n";
          loop (i ++ 1L)
        end
    in
    if n <> 0L && t <> 0L then
      begin
        loop 0L;
        loop_out ()
      end
  in
  loop_out ()
