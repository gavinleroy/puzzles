(* Kattis * Relocation *)

exception Impossible

let read_int_list () =
  let line = read_line () in
  let ints = Str.split (Str.regexp " ") line in
  List.map int_of_string ints

let () =
  match read_int_list () with
  | [n; q] ->
    let vs = read_int_list () in
    let arr = Array.of_list vs in
    for i = 1 to q do
      let query = read_int_list () in
      match query with
      | [1; c; x] -> Array.set arr (c - 1) x
      | [2; a; b] ->
        let a' = arr.(a - 1) in
        let b' = arr.(b - 1) in
        Printf.printf "%d\n" (a' - b' |> abs)
      | _ -> raise Impossible
    done
  | _ -> raise Impossible
