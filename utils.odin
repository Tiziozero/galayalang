package main

import "core:strings"
import "core:fmt"

tts :: proc(t: TypeId) -> string {
    ty := get_type(t)
    if ty == nil {
        return "<invalid>"
    }

    if ty.kind == .Generic {
        return fmt.aprintf("(generic type) %s", ty.name);
    }
    if ty.name != "" {
        return ty.name
    }
    switch ty.kind {
    case .Generic:
        return "generic_type"
    case .Int64: 
        return "Int64"
    case .Int32: 
        return "Int32"
    case .Int16: 
        return "Int16"
    case .Int_8: 
        return "Int_8"
    case .UInt64: 
        return "UInt64"
    case .UInt32: 
        return "UInt32"
    case .UInt16: 
        return "UInt16"
    case .UInt_8: 
        return "UInt_8"
    case .Flt64: 
        return "Flt64"
    case .Flt32: 
        return "Flt32"
    case .Flt16: 
        return "Flt16"
    case .Flt_8: 
        return "Flt_8"
    case .Any:
        return "Any"
    case .String:
        return "String"
    case .Invalid:
        return "invalid"
    case .UntypedInteger:
        return "untyped integer"
    case .UntypedFloat:
        return "untyped float"
    case .UntypedStruct:
        return "untyped struct"
    case .ZeroInit:
        return "zero-init"
    case .Void:
        return "void"
    case .Bool, .Rune, .Byte:
        // builtins carry their printable name (e.g. "i32", "f64", "bool")
        return ty.name

    case .Pointer:
        return fmt.tprintf("^%s", tts(ty.ptr), )

    case .Slice:
        return fmt.tprintf("[]%s", tts(ty.slice.type))

    case .FixedSizeArray:
        return fmt.tprintf("[%d]%s", ty.fixed_size_array.size, tts(ty.fixed_size_array.type))

    case .Union:
        sb := strings.builder_make(allocator=get_ctx().allocator)
        strings.write_string(&sb, "union { ")
        for field, i in ty.union_fields {
            if i > 0 do strings.write_string(&sb, ", ")
            strings.write_string(&sb, field.name)
            strings.write_string(&sb, ": ")
            strings.write_string(&sb, tts(field.type))
        }
        strings.write_string(&sb, " }")
        return strings.to_string(sb)
    case .Struct:
        if ty.name != "" {
            return ty.name
        }
        // anonymous struct: expand fields
        sb := strings.builder_make(allocator=get_ctx().allocator)
        strings.write_string(&sb, "struct { ")
        for field, i in ty.structure.fields {
            if i > 0 do strings.write_string(&sb, ", ")
            strings.write_string(&sb, field.name)
            strings.write_string(&sb, ": ")
            strings.write_string(&sb, tts(field.type))
        }
        strings.write_string(&sb, " }")
        return strings.to_string(sb)

    case .Function:
        sb := strings.builder_make(allocator=get_ctx().allocator)
        strings.write_string(&sb, "fn(")
        for arg, i in ty.fn.args {
            if i > 0 do strings.write_string(&sb, ", ")
            strings.write_string(&sb, tts(arg.type))
        }
        strings.write_string(&sb, ")")

        ret := get_type(ty.fn.ret_ty)
        if ret != nil && ret.kind != .Void {
            strings.write_string(&sb, ": ")
            strings.write_string(&sb, tts(ty.fn.ret_ty))
        }
        return strings.to_string(sb)
    }

    return "<unknown>"
}
