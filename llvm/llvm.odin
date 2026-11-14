package llvm

when ODIN_OS == .Windows {
    foreign import lib "system:LLVM-C.lib"
} else when ODIN_OS == .Darwin {
    foreign import lib "system:LLVM"
} else {
    foreign import lib "system:LLVM"
}

ModuleRef  :: distinct rawptr
ContextRef :: distinct rawptr
BuilderRef :: distinct rawptr
TypeRef    :: distinct rawptr
ValueRef   :: distinct rawptr
BasicBlockRef :: distinct rawptr
Bool :: b32 // LLVMBool is int

@(default_calling_convention="c", link_prefix="LLVM")
foreign lib {
    ModuleCreateWithName :: proc(name: cstring) -> ModuleRef ---
    Int32Type            :: proc() -> TypeRef ---
    FunctionType         :: proc(ret: TypeRef, params: [^]TypeRef, count: u32, vararg: Bool) -> TypeRef ---
    AddFunction          :: proc(m: ModuleRef, name: cstring, ty: TypeRef) -> ValueRef ---
    AppendBasicBlock     :: proc(fn: ValueRef, name: cstring) -> BasicBlockRef ---
    CreateBuilder        :: proc() -> BuilderRef ---
    PositionBuilderAtEnd :: proc(b: BuilderRef, bb: BasicBlockRef) ---
    BuildRet             :: proc(b: BuilderRef, v: ValueRef) -> ValueRef ---
    ConstInt             :: proc(ty: TypeRef, n: u64, signed: Bool) -> ValueRef ---
    DumpModule           :: proc(m: ModuleRef) ---
    DisposeBuilder       :: proc(b: BuilderRef) ---
    DisposeModule        :: proc(m: ModuleRef) ---
}
