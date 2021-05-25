(* Kattis, fifty shades of pink *)

module Either = struct
  type ('a,'b) t =
    | Left of 'a
    | Right of 'b

  let either f g = function
    | Left x -> f x
    | Right y -> g y
end

open Either

let contains s1 s2 =
  try
    let len = String.length s2 in
    for i = 0 to String.length s1 - len do
      if String.sub s1 i len = s2 then raise Exit
    done;
    false
  with Exit -> true

let haspink = fun s ->
  contains s "pink"

let hasrose = fun s ->
  contains s "rose"

let isvalid s =
  haspink s || hasrose s

let solve cs =
  List.map String.lowercase_ascii cs
  |> List.filter isvalid
  |> List.length
  |> fun n ->
  if n=0 then Right "I must watch Star Wars with my daughter"
  else Left n

let iden x = x

let () =
  let t = read_int () in
  let rec readt t =
    if t=0 then []
    else read_line () :: readt (t-1) in
  readt t |> solve |> either string_of_int iden |> print_endline
