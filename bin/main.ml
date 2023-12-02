open! Core

let () =
  let ans = Aoc2023.P1.solve () in
  print_endline [%string {| %{ans#Int} |}]
;;
