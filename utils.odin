package main

import "core:strings"
import "core:fmt"


tts :: proc(t: TypeId) -> string {
    ty := get_type(t)
    if ty == nil {
        return "<invalid>"
    }

    if ty.kind == .Generic {
        return fmt.aprintf("(generic type) %s", ty.name)
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
    case .Void:
        return "void"

    case .Bool, .Rune, .Byte:
        return ty.name

    case .Pointer:
        return fmt.aprintf("^%s", tts(ty.ptr))

    case .Slice:
        return fmt.aprintf("[]%s", tts(ty.slice.type))

    case .FixedSizeArray:
        return fmt.aprintf(
            "[%d]%s",
            ty.fixed_size_array.size,
            tts(ty.fixed_size_array.type),
        )

    case .Union:
        sb := strings.builder_make()
        strings.write_string(&sb, "union { ")

        for field, i in ty.union_fields {
            if i > 0 {
                strings.write_string(&sb, ", ")
            }
            strings.write_string(&sb, field)
        }

        strings.write_string(&sb, " }")
        return strings.to_string(sb)

    case .Struct:
        sb := strings.builder_make()
        strings.write_string(&sb, "struct { ")

        for field, i in ty.structure.fields {
            if i > 0 {
                strings.write_string(&sb, ", ")
            }

            strings.write_string(&sb, field.name)
            strings.write_string(&sb, ": ")
            strings.write_string(&sb, tts(field.type))
        }

        strings.write_string(&sb, " }")
        return strings.to_string(sb)

    case .Function:
        fn := ty.fn
        sb := strings.builder_make()

        if fn.is_extern {
            strings.write_string(&sb, "extern ")
        }

        strings.write_string(&sb, "fn")

        // Generic parameters.
        if len(fn.type_args) > 0 {
            strings.write_string(&sb, "<")

            for arg, i in fn.type_args {
                if i > 0 {
                    strings.write_string(&sb, ", ")
                }

                strings.write_string(&sb, arg.name)
            }

            strings.write_string(&sb, ">")
        }

        // Regular arguments.
        strings.write_string(&sb, "(")

        for arg, i in fn.args {
            if i > 0 {
                strings.write_string(&sb, ", ")
            }

            if arg.name != "" {
                strings.write_string(&sb, arg.name)
                strings.write_string(&sb, ": ")
            }

            strings.write_string(&sb, tts(arg.ty))
        }

        // Variadic argument.
        if fn.is_variadic {
            if len(fn.args) > 0 {
                strings.write_string(&sb, ", ")
            }

            strings.write_string(&sb, "...")

            if fn.variadic_name != "" {
                strings.write_string(&sb, fn.variadic_name)
                strings.write_string(&sb, ": ")
            }

            strings.write_string(&sb, tts(fn.variadic_ty))
        }

        strings.write_string(&sb, ")")

        // Return type.
        strings.write_string(&sb, ": ")
        strings.write_string(&sb, tts(fn.ret_ty))

        return strings.to_string(sb)
    }

    return "<unknown>"
}


is_float_literal :: proc(text: string) -> bool {
    for c in text {
        if c == '.' do return true
    }
    return false
}
