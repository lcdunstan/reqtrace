
cstruct rr {
  uint16_t typ;
  uint16_t cls;
  uint32_t ttl;
  uint16_t rdlen
} as big_endian

let hello =
  Printf.printf "Hello, World!\n" [@req "foo1.2"]

