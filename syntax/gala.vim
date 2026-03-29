syn keyword galaKeyword fn struct return if else
syn match galaNumber "\d\+"
syn keyword galaType f16 f32 f64 u8 u16 u32 u64 i8 i16 i32 i64

hi def link galaKeyword Keyword
hi def link galaNumber Number
hi def link galaType Type
" hi galaType guifg=#FF00FF ctermfg=Magenta
