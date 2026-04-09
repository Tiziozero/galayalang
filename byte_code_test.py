#!/usr/bin/env python3
"""
Emits a Gala VM bytecode file for: result = a + (b * c)
  a = 2, b = 3, c = 4  →  result = 14

File layout:
  [4 bytes] magic: 0x47414C41 ('GALA')
  [4 bytes] const_count (int32)
  [const_count * 9 bytes] const pool entries:
      [1 byte]  kind  (0=VAL_I64, 1=VAL_F64)
      [8 bytes] value (little-endian int64 or double)
  [4 bytes] code_size (int32, in bytes)
  [code_size bytes] instructions
      each instruction: [1 byte opcode] [optional 4 byte int32 operand]
"""

import struct


# ── Helpers ──────────────────────────────────────────────────────────────────
def op(opcode):
    return struct.pack('<B', opcode)
# ── Const pool ───────────────────────────────────────────────────────────────
# Indices: 0=2, 1=3, 2=4
def encode_const_pool(pool):
    out = b''
    return out

# ── Bytecode ─────────────────────────────────────────────────────────────────
# Gala memory layout (flat mem[] slots):
#   slot 0 = a
#   slot 1 = b
#   slot 2 = c
#   slot 3 = result
#
# Program:
#   a = 2          CONST_I 0  / STORE 0
#   b = 3          CONST_I 1  / STORE 1
#   c = 4          CONST_I 2  / STORE 2
#   tmp = b * c    LOAD 1 / LOAD 2 / IMUL
#   result = a + tmp  LOAD 0 / ADD / STORE 3
#   halt

code  = b''
code += b'\x01\x00\x00\x00'
code += b'\x01\x00\x00\x00'
code += b'\x01\x00\x00\x00'
code += b'\x01\x00\x00\x00'
code += b'\x07\x00\x00\x00'
code += b'\x14\x00\x00\x00'
# ── Assemble file ────────────────────────────────────────────────────────────
def write_bytecode(path):

    with open(path, 'wb') as f:
        # f.write(MAGIC)
        # f.write(pool_bytes)
        # f.write(code_header)
        f.write(code)


write_bytecode("out.gbc")
