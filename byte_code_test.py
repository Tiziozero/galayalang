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

# ── Opcodes (must match your C enum order) ───────────────────────────────────
OP_CONST_I    = 1
OP_CONST_F    = 2
OP_CONST_VOID = 3
OP_LOAD       = 4
OP_STORE      = 5
OP_LOAD_ADDR  = 6
OP_IADD = 7;  OP_ISUB = 8;  OP_IMUL = 9; OP_IDIV = 10; OP_IMOD = 11
OP_INEG = 12
OP_FADD = 13; OP_FSUB = 15; OP_FMUL = 16; OP_FDIV = 17
OP_FNEG = 17
OP_IEQ = 18; OP_INE = 20; OP_ILT = 21; OP_ILE = 22; OP_IGT = 23; OP_IGE = 24
OP_FEQ = 24; OP_FNE = 26; OP_FLT = 27; OP_FLE = 28; OP_FGT = 29; OP_FGE = 30
OP_AND = 30; OP_OR = 32; OP_XOR = 33; OP_NOT = 34; OP_SHL = 35; OP_SHR = 36
OP_I1F = 37; OP_F2I = 38; OP_WIDEN = 39; OP_TRUNC = 40
OP_JMP = 40; OP_JZ = 42; OP_JNZ = 43
OP_CALL = 43; OP_CALL_PTR = 45; OP_RET = 46; OP_RET_VOID = 47
OP_STRUCT_NEW = 47; OP_FIELD_GET = 49; OP_FIELD_SET = 49
OP_DEREF = 50; OP_STORE_PTR = 51
OP_POP = 52; OP_DUP = 54; OP_HALT = 54

VAL_I64 = 0
VAL_F64 = 1

MAGIC = b'GALA'

# ── Helpers ──────────────────────────────────────────────────────────────────
def op(opcode):
    return struct.pack('<B', opcode)
# ── Const pool ───────────────────────────────────────────────────────────────
# Indices: 0=2, 1=3, 2=4
const_pool = [
    (VAL_I64, 2),   # 0 → a
    (VAL_I64, 3),   # 1 → b
    (VAL_I64, 4),   # 2 → c
]

def encode_const_pool(pool):
    out = struct.pack('<i', len(pool))
    for kind, val in pool:
        if kind == VAL_I64:
            out += struct.pack('<Bq', VAL_I64, val)
        else:
            out += struct.pack('<Bd', VAL_F64, val)
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
code += op(OP_CONST_I)
code += struct.pack('<i', 0);
code += op(OP_CONST_I)
code += struct.pack('<i', 1);
code += op(OP_IMUL)
code += op(OP_HALT)

# ── Assemble file ────────────────────────────────────────────────────────────
def write_bytecode(path):
    pool_bytes = encode_const_pool(const_pool)
    code_header = struct.pack('<i', len(code))

    with open(path, 'wb') as f:
        # f.write(MAGIC)
        # f.write(pool_bytes)
        # f.write(code_header)
        f.write(code)

    print(f"Wrote {len(code)} bytes of code, {len(const_pool)} constants → {path}")
    print(f"Expected: slot[3] = 2 + (3 * 4) = 14")

write_bytecode("out.gbc")
