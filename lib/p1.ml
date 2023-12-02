open! Core

let rec process s : (int * int) option =
  let curr, rest =
    match s with
    | 'o' :: 'n' :: ('e' :: _ as rest) -> Some 1, rest
    | 't' :: 'w' :: ('o' :: _ as rest) -> Some 2, rest
    | 't' :: 'h' :: 'r' :: 'e' :: ('e' :: _ as rest)-> Some 3, rest
    | 'f' :: 'o' :: 'u' :: 'r' :: rest -> Some 4, rest
    | 'f' :: 'i' :: 'v' :: ('e' :: _ as rest) -> Some 5, rest
    | 's' :: 'i' :: 'x' :: rest -> Some 6, rest
    | 's' :: 'e' :: 'v' :: 'e' :: ('n' :: _ as rest) -> Some 7, rest
    | 'e' :: 'i' :: 'g' :: 'h' :: ('t' :: _ as rest) -> Some 8, rest
    | 'n' :: 'i' :: 'n' :: ('e' :: _ as rest) -> Some 9, rest
    | x :: rest when Char.is_digit x -> Some (Char.get_digit_exn x), rest
    | _ :: rest -> None, rest
    | _ -> None, []
  in
  match rest with
  | [] -> Option.map curr ~f:(fun x -> x, x)
  | _ ->
    (match curr, process rest with
     | Some x, None -> Some (x, x)
     | None, x -> x
     | Some x, Some (_, b) -> Some (x, b))
;;

let solve () =
  In_channel.read_lines "inputs/1.txt"
  (* In_channel.input_lines In_channel.stdin*)
  |> List.fold ~init:0 ~f:(fun acc s ->
    let a, b = process (String.to_list s) |> Option.value_exn in
    acc + a * 10 + b)
;;
