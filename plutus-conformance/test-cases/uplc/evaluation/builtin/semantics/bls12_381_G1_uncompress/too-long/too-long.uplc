-- The bytestring is the compressed version of the G1 zero point, but extended
-- to 49 bytes.
(program 1.0.0
 [
  (builtin bls12_381_G1_uncompress)
  (con bytestring #c0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
 ]
)