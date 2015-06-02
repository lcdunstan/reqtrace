
let hello s =
  Printf.printf "Hello, World! %s\n" s [@req (rfc 6762) "s2_p1_c1"]

