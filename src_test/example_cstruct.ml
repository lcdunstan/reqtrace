
cstruct rr {
  uint16_t typ;
  uint16_t cls;
  uint32_t ttl;
  uint16_t rdlen
} as big_endian


let hello s =
  Printf.printf "Hello, World! %s\n" s [@req mdns "s2_p1_c1"]

let something x =
  match x with
  | 0 -> "0" [@req edns0 "s99"]
  | 1 -> "1"
  | 2 -> "2"
  | _ -> "3+"

let _ =
  hello (something 1) [@req mdns "s18"]

