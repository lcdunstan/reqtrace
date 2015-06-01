
[@@@reftype Impl]
[@@@reqdoc let mdns = rfc 6762]
[@@@reqdoc let edns0 = rfc 2671]
[@@@reqdoc let other = uri "http://example.net/coolest_standard_ever"]

let hello s =
  Printf.printf "Hello, World! %s\n" s [@req mdns "s2_p1_c1"]

let something x =
  match x with
  | 0 -> "0" [@req edns0 "s99"]
  | 1 -> "1"
  | 2 -> "2"
  | _ -> "3+"

let _ =
  hello (something 1) [@req (rfc 9999) "s18"]

