// gala_codegen.c
// Walks the resolved/typechecked AST and emits bytecode for the VM.
// Assumes all nodes have n->resolved=1 and n->type set.
//
// Usage:
//   Codegen cg;
//   cg_init(&cg, vm);
//   cg_program(&cg, root_node_list);

#include "gala_vm.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

// ─── forward decls (from your compiler) ─────────────────────────────────────
// These are the types/enums your compiler already defines.
// We only reference them here — no redefinition needed when you integrate.

// NodeKind, Node, Type, TypeKind, OpType, UnaryType, Symbol, Span ...
// (already in your headers)

// ─── Codegen state ───────────────────────────────────────────────────────────

#define MAX_LOCALS 256
#define MAX_BREAKS 64

typedef struct {
    Symbol* sym;
    int     slot; // index in locals array
} LocalEntry;

typedef struct CgScope CgScope;
struct CgScope {
    LocalEntry  locals[MAX_LOCALS];
    int         local_count;
    CgScope*    parent;
};

typedef struct {
    VM*      vm;
    Chunk*   chunk;    // current chunk being written
    CgScope* scope;    // current local scope
    int      fn_index; // index of fn being compiled (for self-reference)

    // break/continue patching for loops (add when you add loops)
    int break_patches[MAX_BREAKS];
    int break_count;
} Codegen;

// ─── Scope helpers ───────────────────────────────────────────────────────────

static CgScope* scope_push(Codegen* cg) {
    CgScope* s = calloc(1, sizeof(CgScope));
    s->parent = cg->scope;
    cg->scope = s;
    return s;
}

static void scope_pop(Codegen* cg) {
    CgScope* old = cg->scope;
    cg->scope = old->parent;
    free(old);
}

static int scope_add(Codegen* cg, Symbol* sym) {
    CgScope* s = cg->scope;
    assert(s->local_count < MAX_LOCALS);
    int slot = cg->chunk->local_count++;
    s->locals[s->local_count++] = (LocalEntry){sym, slot};
    return slot;
}

static int scope_find(Codegen* cg, Symbol* sym) {
    for (CgScope* s = cg->scope; s; s = s->parent) {
        for (int i = 0; i < s->local_count; i++) {
            if (s->locals[i].sym == sym)
                return s->locals[i].slot;
        }
    }
    return -1; // not found -> global
}

// ─── Emit helpers ────────────────────────────────────────────────────────────

static inline int emit(Codegen* cg, OpCode op, int32_t operand) {
    return chunk_emit(cg->chunk, op, operand);
}
static inline int emit0(Codegen* cg, OpCode op) {
    return chunk_emit(cg->chunk, op, 0);
}

// ─── Type helpers ────────────────────────────────────────────────────────────

static int type_is_float(Type* t) {
    return t && (t->kind == tt_f32 || t->kind == tt_f64 || t->kind == tt_untyped_float);
}
static int type_is_int(Type* t) {
    switch (t ? t->kind : tt_void) {
    case tt_u8: case tt_u16: case tt_u32: case tt_u64: case tt_u128:
    case tt_i8: case tt_i16: case tt_i32: case tt_i64: case tt_i128:
    case tt_usize: case tt_char:
    case tt_untyped_unsigned_int: case tt_untyped_int:
        return 1;
    default: return 0;
    }
}

// Emit the right arithmetic opcode given the operator and the result type.
// This is called AFTER both operands have been pushed.
static void emit_binop(Codegen* cg, OpType op, Type* result_type) {
    int is_f = type_is_float(result_type);
    switch (op) {
    // your OpType enum values — adjust names to match yours
    case OP_ADD:  emit0(cg, is_f ? OP_FADD : OP_IADD); break;
    case OP_SUB:  emit0(cg, is_f ? OP_FSUB : OP_ISUB); break;
    case OP_MUL:  emit0(cg, is_f ? OP_FMUL : OP_IMUL); break;
    case OP_DIV:  emit0(cg, is_f ? OP_FDIV : OP_IDIV); break;
    case OP_MOD:  emit0(cg, OP_IMOD); break;
    case OP_EQ:   emit0(cg, is_f ? OP_FEQ : OP_IEQ); break;
    case OP_NE:   emit0(cg, is_f ? OP_FNE : OP_INE); break;
    case OP_LT:   emit0(cg, is_f ? OP_FLT : OP_ILT); break;
    case OP_LE:   emit0(cg, is_f ? OP_FLE : OP_ILE); break;
    case OP_GT:   emit0(cg, is_f ? OP_FGT : OP_IGT); break;
    case OP_GE:   emit0(cg, is_f ? OP_FGE : OP_IGE); break;
    case OP_AND:  emit0(cg, OP_AND); break;
    case OP_OR:   emit0(cg, OP_OR);  break;
    case OP_XOR:  emit0(cg, OP_XOR); break;
    case OP_SHL:  emit0(cg, OP_SHL); break;
    case OP_SHR:  emit0(cg, OP_SHR); break;
    default: assert(0 && "unhandled binop"); break;
    }
}

// Emit a cast from src type to dst type (both already resolved).
static void emit_cast(Codegen* cg, Type* src, Type* dst) {
    if (!src || !dst || src->kind == dst->kind) return;
    int src_f = type_is_float(src), dst_f = type_is_float(dst);
    if (!src_f && dst_f) { emit0(cg, OP_I2F); return; }
    if (src_f && !dst_f) { emit0(cg, OP_F2I); return; }
    // int -> smaller int: truncate
    if (!src_f && !dst_f) {
        int bits = 0;
        switch (dst->kind) {
        case tt_u8:  case tt_i8:  bits=8;  break;
        case tt_u16: case tt_i16: bits=16; break;
        case tt_u32: case tt_i32: bits=32; break;
        default: bits=64; break;
        }
        if (bits < 64) emit(cg, OP_TRUNC, bits);
    }
}

// ─── Forward declarations ────────────────────────────────────────────────────

static void cg_node(Codegen* cg, Node* n);
static void cg_expr(Codegen* cg, Node* n);
static void cg_stmt(Codegen* cg, Node* n);
static int  cg_fn(Codegen* cg_parent, Node* n, int is_lit); // returns fn index

// ─── Main dispatch ───────────────────────────────────────────────────────────

static void cg_node(Codegen* cg, Node* n) {
    if (!n) return;
    if (n->yields_value) cg_expr(cg, n);
    else                 cg_stmt(cg, n);
}

// ─── Expressions ─────────────────────────────────────────────────────────────

static void cg_expr(Codegen* cg, Node* n) {
    assert(n);
    switch (n->kind) {

    // ── integer/float literal ──────────────────────────────────────────────
    case NodeNumLit: {
        if (type_is_float(n->type)) {
            int idx = chunk_add_const_f(cg->chunk, n->number.number);
            emit(cg, OP_CONST_F, idx);
        } else {
            int idx = chunk_add_const_i(cg->chunk, (int64_t)n->number.integer);
            emit(cg, OP_CONST_I, idx);
        }
    } break;

    // ── symbol (variable read) ─────────────────────────────────────────────
    case NodeSymbol: {
        assert(n->symbol);
        int slot = scope_find(cg, n->symbol);
        if (slot >= 0) {
            emit(cg, OP_LOAD, slot);
        } else {
            // global — you'd look up global index from symbol table
            emit(cg, OP_LOAD_GLOBAL, n->symbol->global_index);
        }
    } break;

    // ── fn literal (anonymous fn) ──────────────────────────────────────────
    case NodeFnLit: {
        int fn_idx = cg_fn(cg, n, 1);
        int idx = chunk_add_const_i(cg->chunk, fn_idx);
        emit(cg, OP_CONST_I, idx); // push fn index as i64
        // then wrap as VAL_FN at runtime via a cast would need a special op;
        // simpler: just treat fn pointers as i64 fn indices and use CALL_PTR
    } break;

    // ── binop ──────────────────────────────────────────────────────────────
    case NodeBinOp: {
        // compound assignment: a += b  ->  a = a + b
        // your typechecker should have desugared these, but if not:
        cg_expr(cg, n->binop.left);
        cg_expr(cg, n->binop.right);
        emit_binop(cg, n->binop.type, n->type);
    } break;

    // ── unary ──────────────────────────────────────────────────────────────
    case NodeUnary: {
        switch (n->unary.type) {
        case UNARY_NEG:
            cg_expr(cg, n->unary.target);
            emit0(cg, type_is_float(n->type) ? OP_FNEG : OP_INEG);
            break;
        case UNARY_NOT:
            cg_expr(cg, n->unary.target);
            emit0(cg, OP_NOT);
            break;
        case UNARY_ADDR: // &x  ->  push pointer to x
            // target must be a local symbol
            assert(n->unary.target->kind == NodeSymbol);
            {
                int slot = scope_find(cg, n->unary.target->symbol);
                assert(slot >= 0 && "& on non-local not supported yet");
                emit(cg, OP_LOAD_ADDR, slot);
            }
            break;
        case UNARY_DEREF: // *ptr
            cg_expr(cg, n->unary.target);
            emit0(cg, OP_DEREF);
            break;
        default: assert(0 && "unhandled unary"); break;
        }
    } break;

    // ── cast  (expr as Type) ───────────────────────────────────────────────
    case NodeCast: {
        cg_expr(cg, n->cast.target);
        emit_cast(cg, n->cast.target->type, n->cast.to);
    } break;

    // ── field access  target.field ─────────────────────────────────────────
    case NodeFieldAccess: {
        // push the struct pointer
        cg_expr(cg, n->field_access.target);
        // if target is a value type (struct by value), take addr first —
        // for simplicity everything is heap-allocated in this VM
        assert(n->symbol && "field_access symbol must be resolved to Field symbol");
        emit(cg, OP_FIELD_GET, n->symbol->field_index);
    } break;

    // ── index  target[i] ──────────────────────────────────────────────────
    case NodeIndex: {
        cg_expr(cg, n->index.target);
        cg_expr(cg, n->index.index);
        // for now just emit FIELD_GET with dynamic index —
        // you'd add OP_ARRAY_GET when you have arrays
        assert(0 && "dynamic index not yet supported");
    } break;

    // ── struct literal  v.{x:1, y:2} ──────────────────────────────────────
    case NodeStructLit: {
        assert(n->type && n->type->kind == tt_struct);
        StructType* st = &n->type->struct_t;
        int field_count = st->count;
        // STRUCT_NEW operand encodes field_count and type_id
        // type_id: use n->type->uutid & 0xFFFF (or a dedicated type index)
        int32_t operand = ((field_count & 0xFFFF) << 16) | (n->type->uutid & 0xFFFF);
        emit(cg, OP_STRUCT_NEW, operand);
        // now fill named fields
        // node_list of NodeNamedField
        Node* fields = n->struct_literal.fields;
        assert(fields->kind == NodeNodeList);
        for (int i = 0; i < fields->node_list.count; i++) {
            Node* nf = fields->node_list.nodes[i];
            assert(nf->kind == NodeNamedField);
            // find field index in struct type
            int field_idx = -1;
            for (int j = 0; j < st->count; j++) {
                if (span_eq(st->fields[j]->name, nf->named_field.ident->ident)) {
                    field_idx = j; break;
                }
            }
            assert(field_idx >= 0 && "named field not found in struct");
            emit0(cg, OP_DUP);                   // dup the struct ptr
            cg_expr(cg, nf->named_field.expr);   // push value
            emit(cg, OP_FIELD_SET, field_idx);    // ptr->field = value
        }
        // struct ptr remains on stack as the result
    } break;

    // ── fn call ────────────────────────────────────────────────────────────
    case NodeFnCall: {
        // push args first (left to right)
        Node* args = n->fn_call.args;
        if (args) {
            assert(args->kind == NodeNodeList);
            for (int i = 0; i < args->node_list.count; i++) {
                Node* arg_node = args->node_list.nodes[i];
                cg_expr(cg, arg_node);
                // store into callee's arg slots — convention: args are the
                // first N locals of the called fn. The callee's chunk's
                // local_count already includes room for them.
                // We push them onto the stack; CALL will pick them up.
                // (Simpler: caller stores args directly into callee's locals
                //  before switching frame. Done inside OP_CALL handler.)
            }
        }
        // push fn value / call
        Node* target = n->fn_call.target;
        if (target->kind == NodeSymbol && target->symbol) {
            // direct call: symbol should have a fn_index
            int fn_idx = target->symbol->fn_index;
            if (fn_idx >= 0) {
                emit(cg, OP_CALL, fn_idx);
            } else {
                // fn stored as a local (fn literal or fn pointer)
                int slot = scope_find(cg, target->symbol);
                emit(cg, OP_LOAD, slot);
                emit0(cg, OP_CALL_PTR);
            }
        } else {
            // expression that yields a fn pointer
            cg_expr(cg, target);
            emit0(cg, OP_CALL_PTR);
        }
    } break;

    default:
        fprintf(stderr, "cg_expr: unhandled node kind %d\n", n->kind);
        assert(0);
    }
}

// ─── Statements ──────────────────────────────────────────────────────────────

static void cg_stmt(Codegen* cg, Node* n) {
    if (!n) return;
    switch (n->kind) {

    // ── block ──────────────────────────────────────────────────────────────
    case NodeBlock: {
        scope_push(cg);
        for (int i = 0; i < n->block.count; i++) {
            cg_node(cg, n->block.stmts[i]);
            // if statement produced a value and wasn't the last, pop it
            if (n->block.stmts[i]->yields_value && n->block.stmts[i] != n->block.last) {
                emit0(cg, OP_POP);
            }
        }
        scope_pop(cg);
    } break;

    // ── var dec: t := expr  or  a: u32 ────────────────────────────────────
    case NodeVarDec:
    case NodeConstDec: {
        int slot = scope_add(cg, n->symbol);
        if (n->var_dec.value) {
            cg_expr(cg, n->var_dec.value);
            emit(cg, OP_STORE, slot);
        }
        // else left as zero (calloc'd)
    } break;

    // ── return ─────────────────────────────────────────────────────────────
    case NodeRet: {
        if (n->ret.expr) {
            cg_expr(cg, n->ret.expr);
            emit0(cg, OP_RET);
        } else {
            emit0(cg, OP_RET_VOID);
        }
    } break;

    // ── if / else ──────────────────────────────────────────────────────────
    case NodeIfStmt: {
        cg_expr(cg, n->if_stmt.cond);
        int jz = emit(cg, OP_JZ, 0); // patch later

        cg_stmt(cg, n->if_stmt.block);

        if (n->if_stmt.else_block) {
            int jmp_over = emit(cg, OP_JMP, 0); // skip else
            chunk_patch(cg->chunk, jz, cg->chunk->code_len);
            cg_stmt(cg, n->if_stmt.else_block);
            chunk_patch(cg->chunk, jmp_over, cg->chunk->code_len);
        } else {
            chunk_patch(cg->chunk, jz, cg->chunk->code_len);
        }

        // alt conds (else if)
        // (extend here when you have else-if chains)
    } break;

    // ── expression statement (fn call, assignment, etc.) ───────────────────
    case NodeBinOp: {
        // assignment operators: left must be an lvalue
        // In Gala: y.y += y.y + 1  or  x.f = k
        // These are binops where the op is OP_ASSIGN or compound assign.
        // Adjust OP_ASSIGN to whatever your OpType enum uses.
        if (n->binop.type == OP_ASSIGN) {
            Node* lhs = n->binop.left;
            Node* rhs = n->binop.right;
            cg_expr(cg, rhs);
            // store into lvalue
            if (lhs->kind == NodeSymbol) {
                int slot = scope_find(cg, lhs->symbol);
                emit(cg, OP_STORE, slot);
            } else if (lhs->kind == NodeFieldAccess) {
                cg_expr(cg, lhs->field_access.target); // push ptr
                // value is already on stack below ptr — need to swap
                // emit DUP/rotate... simplest: re-eval (assumes no side effects)
                // For a real impl: emit lhs address before rhs.
                // Here: value is TOS-1, ptr is TOS — swap them
                // We don't have a SWAP op yet, so re-emit ptr first:
                // (restructure: push ptr, push value, FIELD_SET)
                // This means we should emit in order: ptr, value, FIELD_SET
                // Redo: pop the value we just pushed, save it, push ptr, push val, set
                assert(0 && "restructure field assign emission — see comment");
            } else if (lhs->kind == NodeUnary && lhs->unary.type == UNARY_DEREF) {
                // *ptr = val
                cg_expr(cg, lhs->unary.target);
                emit0(cg, OP_STORE_PTR);
            }
        } else {
            // pure expression statement (result discarded)
            cg_expr(cg, n);
            emit0(cg, OP_POP);
        }
    } break;

    case NodeFnCall: {
        cg_expr(cg, n); // emit the call
        if (n->type && n->type->kind != tt_void)
            emit0(cg, OP_POP); // discard return value
    } break;

    // ── fn dec ─────────────────────────────────────────────────────────────
    case NodeFnDec: {
        // compile fn body into a new chunk, register in vm
        int fn_idx = cg_fn(cg, n, 0);
        // store fn index into the symbol's global slot
        assert(n->symbol);
        n->symbol->fn_index = fn_idx;
        // also store as global variable so it can be loaded by name
        int gi = n->symbol->global_index;
        if (gi >= 0) {
            int idx = chunk_add_const_i(cg->chunk, fn_idx);
            emit(cg, OP_CONST_I, idx);
            emit(cg, OP_STORE_GLOBAL, gi);
        }
    } break;

    case NodeStructDec:
        // struct declarations are purely compile-time; no bytecode needed
        break;

    default:
        fprintf(stderr, "cg_stmt: unhandled node kind %d\n", n->kind);
        break;
    }
}

// ─── Function compilation ─────────────────────────────────────────────────────
// Compiles a NodeFnDec or NodeFnLit into a Chunk and registers it.
// Returns the fn index in the VM's fn table.

static int cg_fn(Codegen* parent, Node* n, int is_lit) {
    Node* args_list    = is_lit ? n->fn_dec.args  : n->fn_dec.args;
    Node* body         = is_lit ? n->fn_dec.body  : n->fn_dec.body;
    const char* name   = is_lit ? "<lambda>" :
        (n->fn_dec.ident ? n->fn_dec.ident->ident.ptr : "?");

    Chunk* chunk = chunk_new(name);

    Codegen cg = {0};
    cg.vm    = parent->vm;
    cg.chunk = chunk;

    scope_push(&cg);

    // bind args as first locals (callee convention: args already stored
    // into locals[0..N-1] by the CALL handler before ip=0)
    if (args_list && args_list->kind == NodeNodeList) {
        for (int i = 0; i < args_list->node_list.count; i++) {
            Node* arg = args_list->node_list.nodes[i];
            assert(arg->kind == NodeArg);
            scope_add(&cg, arg->symbol);
        }
    }

    if (body) cg_stmt(&cg, body);

    // ensure there's always a return
    if (!chunk->code_len ||
        chunk->code[chunk->code_len-1].op != OP_RET &&
        chunk->code[chunk->code_len-1].op != OP_RET_VOID) {
        emit0(&cg, OP_RET_VOID);
    }

    scope_pop(&cg);

    int fn_idx = vm_add_fn(parent->vm, chunk);
    cg.fn_index = fn_idx;
    return fn_idx;
}

// ─── Entry point ─────────────────────────────────────────────────────────────

typedef struct {
    VM* vm;
    Chunk* top_level; // synthetic top-level chunk for global init + main call
} CgResult;

// Call this with the top-level NodeNodeList (your program root).
// Returns the fn index of `main`, or -1 on failure.
int cg_program(VM* vm, Node* root) {
    assert(root && root->kind == NodeNodeList);

    // synthetic top-level init chunk
    Chunk* init = chunk_new("__init__");
    Codegen cg = {0};
    cg.vm    = vm;
    cg.chunk = init;
    scope_push(&cg);

    int main_fn = -1;

    for (int i = 0; i < root->node_list.count; i++) {
        Node* n = root->node_list.nodes[i];
        cg_stmt(&cg, n);
        // record main fn index
        if (n->kind == NodeFnDec && n->fn_dec.ident &&
            span_streq(n->fn_dec.ident->ident, "main")) {
            main_fn = n->symbol->fn_index;
        }
    }

    emit0(&cg, OP_HALT);
    scope_pop(&cg);

    vm_add_fn(vm, init);
    return main_fn;
}
