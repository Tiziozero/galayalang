package main

import "core:strings"

Span :: struct {
    start, end: int,
}

TokenKind :: enum {
    Ident,
    Symbol,
    Number,
    String,
    Link,
    Transmute,
    TypeIdOf,
    TypeInfoOf,
    Keyword,
    Cast,
    Len,
    SizeOf,
    True,
    False,
    Any,
    EOF,
}
Keyword :: enum {
    Invalid,
    Alias,
    Fn,
    Return,
    If, Else,
    While,
    For,
    In,
    Extern,
    Export,
    Forward,
    Struct,
    Union,
    Break,
    Continue,
    Import,
}

Token :: struct {
    span: Span,
    kind: TokenKind,
    text: string,
    sid: StringId,
    kw: Keyword,
}

is_alpha :: proc(c: byte) -> bool {
    return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

is_alnum :: proc(c: byte) -> bool {
    return is_alpha(c) || is_num(c)
}
is_num :: proc(c: byte) -> bool {
    return c >= '0' && c <= '9'
}

is_space :: proc(c: byte) -> bool {
    return c == ' ' || c == '\t' || c == '\n' || c == '\r'
}

hex_digit_val :: proc(c: byte) -> int {
    switch {
    case c >= '0' && c <= '9': return int(c - '0')
    case c >= 'a' && c <= 'f': return int(c - 'a' + 10)
    case c >= 'A' && c <= 'F': return int(c - 'A' + 10)
    case: return -1
    }
}
// longest match first, so `<<=` wins over `<<` and `<`
LEX_OPS_3 :: [?]string{"<<=", ">>="}
LEX_OPS_2 :: [?]string{
    ":=", "<<", ">>", "<=", ">=", "!=", "==", "||", "&&",
    "+=", "-=", "*=", "/=", "&=", "|=", "~=", "%=", "::",
}

byte_at :: proc(buf: []byte, i: int) -> byte {
    if i < len(buf) do return buf[i]
    return 0
}

match_at :: proc(buf: []byte, i: int, s: string) -> bool {
    if i + len(s) > len(buf) do return false
    for k in 0..<len(s) {
        if buf[i + k] != s[k] do return false
    }
    return true
}

keyword_token :: proc(ident: string, span: Span) -> (tok: Token, ok: bool) {
    switch ident {
    case "fn":        return Token{span=span, kind=.Keyword, kw=.Fn}, true
    case "alias":     return Token{span=span, kind=.Keyword, kw=.Alias}, true
    case "struct":    return Token{span=span, kind=.Keyword, kw=.Struct}, true
    case "union":     return Token{span=span, kind=.Keyword, kw=.Union}, true
    case "return":    return Token{span=span, kind=.Keyword, kw=.Return}, true
    case "if":        return Token{span=span, kind=.Keyword, kw=.If}, true
    case "else", "otherwise":
                      return Token{span=span, kind=.Keyword, kw=.Else}, true
    case "while":     return Token{span=span, kind=.Keyword, kw=.While}, true
    case "for":       return Token{span=span, kind=.Keyword, kw=.For}, true
    case "in":        return Token{span=span, kind=.Keyword, kw=.In}, true
    case "extern":    return Token{span=span, kind=.Keyword, kw=.Extern}, true
    case "export":    return Token{span=span, kind=.Keyword, kw=.Export}, true
    case "forward":    return Token{span=span, kind=.Keyword, kw=.Forward}, true
    case "break":     return Token{span=span, kind=.Keyword, kw=.Break}, true
    case "continue":  return Token{span=span, kind=.Keyword, kw=.Continue}, true
    case "import":    return Token{span=span, kind=.Keyword, kw=.Import}, true
    case "cast":      return Token{span=span, kind=.Cast, kw=.Invalid}, true
    case "transmute": return Token{span=span, kind=.Transmute, kw=.Invalid}, true
    case "len":       return Token{span=span, kind=.Len, kw=.Invalid}, true
    case "size_of":   return Token{span=span, kind=.SizeOf, kw=.Invalid}, true
    case "type_id":   return Token{span=span, kind=.TypeIdOf, kw=.Invalid}, true
    case "type_info":   return Token{span=span, kind=.TypeInfoOf, kw=.Invalid}, true
    case "any":       return Token{span=span, kind=.Any, kw=.Invalid}, true
    case "true":      return Token{span=span, kind=.True, kw=.Invalid}, true
    case "false":     return Token{span=span, kind=.False, kw=.Invalid}, true
    }
    return {}, false
}

lex_file :: proc(buf: []byte) -> [dynamic]Token {
    tokens := make([dynamic]Token, get_ctx().allocator)
    i := 0
    for i < len(buf) {
        c := buf[i]
        if is_space(c) {
            i += 1
        } else if c == '/' && byte_at(buf, i+1) == '/' {
            // line comment: skip to end of line
            i += 2
            for i < len(buf) && buf[i] != '\n' {
                i += 1
            }
        } else if c == '/' && byte_at(buf, i+1) == '*' {
            // block comment: skip to closing */
            start := i
            i += 2
            for i + 1 < len(buf) && !(buf[i] == '*' && buf[i+1] == '/') {
                i += 1
            }
            if i + 1 < len(buf) {
                i += 2 // consume the closing */
            } else {
                highlight_lines(get_ctx().current_file, Span{start, start + 2})
                gala_panic("unterminated block comment")
            }
        } else if is_num(c) {
            start := i
            prefix := byte_at(buf, i+1)

            if c == '0' && (prefix == 'x' || prefix == 'X') {
                i += 2
                digits_start := i
                for i < len(buf) && hex_digit_val(buf[i]) >= 0 {
                    i += 1
                }
                if i == digits_start {
                    highlight_lines(get_ctx().current_file, Span{start, i})
                    gala_panic("expected hexadecimal digits after 0x")
                }
                if i < len(buf) && is_alnum(buf[i]) {
                    highlight_lines(get_ctx().current_file, Span{start, i + 1})
                    gala_panic("invalid digit in hexadecimal literal")
                }
            } else if c == '0' && (prefix == 'b' || prefix == 'B') {
                i += 2
                digits_start := i
                for i < len(buf) && (buf[i] == '0' || buf[i] == '1') {
                    i += 1
                }
                if i == digits_start {
                    highlight_lines(get_ctx().current_file, Span{start, i})
                    gala_panic("expected binary digits after 0b")
                }
                if i < len(buf) && is_alnum(buf[i]) {
                    highlight_lines(get_ctx().current_file, Span{start, i + 1})
                    gala_panic("invalid digit in binary literal")
                }
            } else {
                // decimal integer / floating-point literal
                for i < len(buf) && is_num(buf[i]) {
                    i += 1
                }

                if byte_at(buf, i) == '.' && is_num(byte_at(buf, i+1)) {
                    i += 1
                    for i < len(buf) && is_num(buf[i]) {
                        i += 1
                    }
                }
            }

            text := cast(string)buf[start:i]
            append(&tokens, Token{
                span = Span{start, i},
                kind = .Number,
                text = text,
            })
        } else if is_alpha(c) {
            start := i
            for i < len(buf) && is_alnum(buf[i]) {
                i += 1
            }
            ident := cast(string)buf[start:i]
            span := Span{start, i}

            after_at := len(tokens) > 0 && is_symbol(tokens[len(tokens)-1], "@")

            if ident == "_" {
                highlight_lines(get_ctx().current_file, span)
                gala_panic("invalid ident \"_\".")
            } else if ident == "link" && after_at {
                // `link` is only special as `@link(...)`
                append(&tokens, Token{span=span, kind=.Link, kw=.Invalid})
            } else if tok, is_kw := keyword_token(ident, span); is_kw {
                append(&tokens, tok)
            } else {
                append(&tokens, Token{span=span, kind=.Ident, text=ident})
            }
        } else if c == '"' {
            start := i
            i += 1 // consume opening quote

            out := make([dynamic]byte, get_ctx().allocator)

            for i < len(buf) && buf[i] != '"' {
                ch := buf[i]

                if ch == '\n' {
                    highlight_lines(get_ctx().current_file, Span{start, i})
                    gala_panic("unterminated string literal (hit newline)")
                }

                if ch == '\\' {
                    if i + 1 >= len(buf) {
                        highlight_lines(get_ctx().current_file, Span{i, i + 1})
                        gala_panic("unterminated escape sequence")
                    }
                    esc := buf[i+1]
                    switch esc {
                    case 'n':
                        append(&out, byte('\n'))
                        i += 2
                    case 't':
                        append(&out, byte('\t'))
                        i += 2
                    case 'r':
                        append(&out, byte('\r'))
                        i += 2
                    case '\\':
                        append(&out, byte('\\'))
                        i += 2
                    case '"':
                        append(&out, byte('"'))
                        i += 2
                    case '0':
                        append(&out, byte(0))
                        i += 2
                    case 'x':
                        // \xNN: exactly two hex digits
                        if i + 3 >= len(buf) {
                            highlight_lines(get_ctx().current_file, Span{i, len(buf)})
                            gala_panic("truncated \\x escape sequence")
                        }
                        hi := hex_digit_val(buf[i+2])
                        lo := hex_digit_val(buf[i+3])
                        if hi < 0 || lo < 0 {
                            highlight_lines(get_ctx().current_file, Span{i, i + 4})
                            gala_panic("invalid hex digits in \\x escape")
                        }
                        append(&out, byte(hi * 16 + lo))
                        i += 4
                    case:
                        highlight_lines(get_ctx().current_file, Span{i, i + 2})
                        gala_panic("unknown escape sequence")
                    }
                } else {
                    append(&out, ch)
                    i += 1
                }
            }

            if i >= len(buf) {
                highlight_lines(get_ctx().current_file, Span{start, i})
                gala_panic("unterminated string literal (hit EOF)")
            } else {
                i += 1 // consume closing quote
            }

            sid := intern_string(cast(string)out[:])
            append(&tokens, Token{
                span = Span{start, i},
                kind = .String,
                sid = sid,
                text = get_ctx().data[sid],
            })
        } else {
            op_len := 1
            for op in LEX_OPS_3 {
                if match_at(buf, i, op) { op_len = 3; break }
            }
            if op_len == 1 {
                for op in LEX_OPS_2 {
                    if match_at(buf, i, op) { op_len = 2; break }
                }
            }
            append(&tokens, Token{
                span = Span{i, i + op_len},
                kind = .Symbol,
                text = cast(string)buf[i:i+op_len],
            })
            i += op_len
        }
    }
    append(&tokens, Token{kind=.EOF})
    return tokens
}

StringId :: distinct u64


intern_string :: proc(s: string) -> StringId {
    if id, ok := get_ctx().table[s]; ok {
        return id
    }
    id := StringId(len(get_ctx().data))
    // NB: `s` must be a stable/owned copy, not a view into a
    // temporary buffer that gets reused/freed later
    owned := strings.clone(s, get_ctx().allocator)
    append(&get_ctx().data, owned)
    get_ctx().table[owned] = id
    return id
}
