(* Kattis, Triple Texting *)

exception Impossible

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec take n xs =
  if n < 1 then []
  else match xs with
    | [] -> []
    | x :: xs' -> x :: take (n-1) xs'

let rec drop n xs =
  if n < 1 then xs
  else match xs with
    | [] -> []
    | x :: xs' -> drop (n-1) xs'

let rec chunksof n xs =
  match xs with
  | [] -> []
  | xs' -> take n xs' :: chunksof n (drop n xs')

let rec zip3 xs ys zs =
  match xs, ys, zs with
  | x::xs, y::ys,z::zs ->
    (x, y, z) :: zip3 xs ys zs
  | _,_,_ -> []

let merge (a, b, c) =
  if a=b then a
  else if b=c then b
  else  c

let () =
  let ws = read_line () in
  let wl = String.length ws in
  explode ws
  |> chunksof (wl / 3)
  |> (fun xs -> match xs with
      | f :: s :: t :: _ -> zip3 f s t
      | _ -> raise Impossible)
  |> List.map merge
  |> List.to_seq
  |> String.of_seq
  |> print_endline
