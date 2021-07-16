(* Yorel Nivag ~ JULI 21 LC *)

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
let mone = -1
let curry f = fun a b -> f (a, b)
let uncurry f = fun (a, b) -> f a b
(* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ *)

(* FIXME get rid of these horrible global mutable variables *)
(* NOTE multipliers 100 and 400 were
 * chosen randomly as OCAML doesn't provide
 * a vector module *)
let root = 1
let max_n = 100001
let ins = Array.make max_n mone
let outs = Array.make max_n mone
let heights = Array.make max_n mone
let euler = Array.make (100 * max_n) mone
let first = Array.make max_n mone
let segtree = Array.make (400 * max_n) mone
let euler_size = ref 0

(* NOTE dfs does a lot of preprocessing
 * ~ calculate the in and out times for each node in the DFS
 * ~ calculate the euler list of the tree
 * ~ calculate the height list for the nodes *)
let dfs graph =
  euler_size := 0; (* resize the euler array *)
  let timer = ref 0 in
  let height = ref 1 in
  let fresh () = incr timer; !timer in
  let eu_pb n = euler.(!euler_size) <- n; incr euler_size in
  let set_h_i n = heights.(n) <- !height; incr height in
  let visited = Array.make max_n false in
  let rec loop v =
    visited.(v) <- true;
    ins.(v) <- fresh ();
    first.(v) <- !euler_size; (* set as first seen *)
    eu_pb v; (* push back value in euler list *)
    set_h_i v; (* increase the hight *)
    List.iter (fun v' ->
        if visited.(v') then ()
        else (loop v';
              eu_pb v)) graph.(v);
    decr height; (* decrease the height by 1 *)
    outs.(v) <- fresh () in
  loop root

let build () =
  let rec loop node b e =
    if b=e then segtree.(node) <- euler.(b)
    else
      begin
        let mid = (b + e) lsr 1 in
        loop (node lsl 1) b mid;
        loop ((node lsl 1) lor 1) (mid+1) e;
        let l = segtree.(node lsl 1) in
        let r = segtree.((node lsl 1) lor 1) in
        segtree.(node) <- (if heights.(l) < heights.(r) then l else r)
      end
  in
  loop 1 0 (!euler_size - 1)

let lca u v =
  let rec loop node b e l r =
    if b > r || e < l then -1
    else if b >= l && e <= r then segtree.(node)
    else
      begin
        let mid = (b + e) lsr 1 in
        let left = loop (node lsl 1) b mid l r in
        let right = loop ((node lsl 1) lor 1) (mid+1) e l r in
        if left < 0 then right
        else if right < 0 then left
        else if heights.(left) < heights.(right) then left
        else right
      end
  in
  let left = min first.(u) first.(v) in
  let right = max first.(u) first.(v) in
  loop 1 0 (!euler_size - 1) left right

let higher v u =
  if ins.(v) > ins.(u) then u
  else v

let lower v u =
  if ins.(v) > ins.(u) then v
  else u

let filter_path ks =
  let same_path u v = ( || )
      (ins.(u) < ins.(v) && outs.(u) > outs.(v))
      (ins.(v) < ins.(u) && outs.(v) > outs.(u))
  in
  let rec loop lb up vs ts ks = match ks with
    | [] -> (lb, up, vs, ts)
    | k :: ks' ->
      if same_path k lb then
        loop (lower lb k) (higher up k) vs (k :: ts) ks'
      else
        loop lb up (k :: vs) ts ks'
  in
  let v = List.hd ks in
  let vs = List.tl ks in
  loop v v [] [v] vs


let query ks =
  let check_path_bounds lb1 lb2 up1 up2 =
    let gt x y = ins.(x) < ins.(y) && outs.(y) < outs.(x) in
    let (up1, up2) = higher up1 up2, lower up1 up2 in
    let upp = lca lb1 lb2 in
    upp=up1 || gt upp up1
  in
  match filter_path ks with
  | (_, _, [], _) -> true
  | (lb1, up1, vs, _) ->
    let (lb2, up2, vs', ts') = filter_path vs in
    (match vs' with
     | [] -> check_path_bounds lb1 lb2 up1 up2
     | _ -> false)

let list_init n f =
  let rec loop i vs =
    if i=0 then vs
    else loop (i-1) (f() :: vs) in
  loop n []

let () =
  let cases = read_int () in
  for case = 1 to cases do
    let n = read_int () in
    let graph = Array.make (n+1) [] in
    for j = 1 to n - 1 do (* fill the tree *)
      let (u, v) = read_ipair () in
      graph.(u) <- v :: graph.(u);
      graph.(v) <- u :: graph.(v)
    done;
    dfs graph;
    build ();
    let q = read_int () in
    for j = 1 to q do (* answer the queries *)
      let k = read_int () in
      let ks = list_init k read_int in
      print_endline
        (if query ks then "YES" else "NO")
    done
  done
