
[@@@reftype Impl]
[@@@reqdoc "RFC6762"]
[@@@reqdoc let edns0 = "RFC2671"]

let hello s =
  Printf.printf "Hello, World! %s\n" s [@req "s2_p1_c1"]

let something x =
  match x with
  | 0 -> "0" [@req edns0 "s99"]
  | 1 -> "1"
  | 2 -> "2"
  | _ -> "3+"

let _ =
  hello (something 1) [@req "RFC9999:s18"]

