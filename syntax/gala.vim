" ~/.vim/syntax/gala.vim

if exists("b:current_syntax")
    finish
endif

" ─────────────────────────────────────────────
" Keywords
" ─────────────────────────────────────────────

syn keyword galaKeyword fn struct union return if else while for otherwise
syn keyword galaKeyword extern export break continue import alias

" Builtin compiler/type operations
syn keyword galaBuiltin cast transmute len size_of any type_id

" Boolean literals
syn keyword galaBoolean true false

" ─────────────────────────────────────────────
" Types
" ─────────────────────────────────────────────

syn keyword galaType
            \ bool byte string
            \ f16 f32 f64
            \ i8 i16 i32 i64 int
            \ u8 u16 u32 u64

" ─────────────────────────────────────────────
" Numbers
" ─────────────────────────────────────────────

" Hexadecimal
syn match galaNumber "\<0[xX][0-9a-fA-F]\+\>"

" Decimal integers
syn match galaNumber "\<[0-9]\+\>"

" Floating point
syn match galaNumber "\<[0-9]\+\.[0-9]\+\>"

" ─────────────────────────────────────────────
" Strings
" ─────────────────────────────────────────────

syn region galaString
            \ start=+"+
            \ skip=+\\."+ 
            \ end=+"+
            \ contains=galaEscape

syn match galaEscape contained "\\\\\|\\\"\|\\n\|\\t\|\\r\|\\0\|\\x[0-9a-fA-F]\{2}"

" ─────────────────────────────────────────────
" Comments
" ─────────────────────────────────────────────

syn match galaComment "//.*$"

syn region galaComment
            \ start="/\*"
            \ end="\*/"

" ─────────────────────────────────────────────
" Function declarations / calls
" ─────────────────────────────────────────────

" fn foo(...)
syn match galaFunctionDecl "\<fn\>\s\+\zs[A-Za-z_][A-Za-z0-9_]*"

" foo(...)
syn match galaFunctionCall "\<[A-Za-z_][A-Za-z0-9_]*\>\ze\s*("

" ─────────────────────────────────────────────
" Highlight links
" ─────────────────────────────────────────────

hi def link galaKeyword Keyword
hi def link galaBuiltin Keyword
hi def link galaBoolean Boolean
hi def link galaType Type
hi def link galaNumber Number
hi def link galaString String
hi def link galaEscape SpecialChar
hi def link galaComment Comment
hi def link galaFunctionDecl Function
hi def link galaFunctionCall Function

let b:current_syntax = "gala"
