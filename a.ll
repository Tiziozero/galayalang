; ModuleID = 'llvm-link'
source_filename = "llvm-link"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-i128:128-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

%0 = type { { ptr, i64 }, i64, i64 }
%1 = type { { ptr, i64 }, i64, i64, i64, i64, { ptr, i64 } }
%2 = type { [4096 x i8], i64 }
%3 = type { i64, i64 }
%4 = type { i64, i64 }
%5 = type { i64, i64 }
%gala_151743._core_cfunctions_posix.TypeField = type { { ptr, i64 }, i64, i64 }
%gala_151743._core_cfunctions_posix.TypeInfo = type { { ptr, i64 }, i64, i64, i64, i64, { ptr, i64 } }
%gala_104923._core_core_posix.Builder = type { [4096 x i8], i64 }
%gala_104923._core_core_posix.Timespec = type { i64, i64 }

@string7 = private unnamed_addr constant [3 x i8] c"0x\00", align 1
@string8 = private unnamed_addr constant [4 x i8] c"nan\00", align 1
@string9 = private unnamed_addr constant [4 x i8] c"inf\00", align 1
@string10 = private unnamed_addr constant [10 x i8] c"<too big>\00", align 1
@string11 = private unnamed_addr constant [4 x i8] c" {}\00", align 1
@string12 = private unnamed_addr constant [4 x i8] c" { \00", align 1
@string13 = private unnamed_addr constant [3 x i8] c", \00", align 1
@string14 = private unnamed_addr constant [4 x i8] c" = \00", align 1
@string15 = private unnamed_addr constant [3 x i8] c" }\00", align 1
@string16 = private unnamed_addr constant [5 x i8] c"true\00", align 1
@string17 = private unnamed_addr constant [6 x i8] c"false\00", align 1
@string18 = private unnamed_addr constant [4 x i8] c"nil\00", align 1
@string19 = private unnamed_addr constant [10 x i8] c"<missing>\00", align 1
@string3 = private unnamed_addr constant [18 x i8] c"Hello from posix!\00", align 1
@string50 = private unnamed_addr constant [3 x i8] c"i8\00", align 1
@string51 = private unnamed_addr constant [4 x i8] c"i16\00", align 1
@string52 = private unnamed_addr constant [4 x i8] c"i32\00", align 1
@string53 = private unnamed_addr constant [4 x i8] c"i64\00", align 1
@string54 = private unnamed_addr constant [3 x i8] c"u8\00", align 1
@string55 = private unnamed_addr constant [4 x i8] c"u16\00", align 1
@string56 = private unnamed_addr constant [4 x i8] c"u32\00", align 1
@string57 = private unnamed_addr constant [4 x i8] c"u64\00", align 1
@string58 = private unnamed_addr constant [3 x i8] c"f8\00", align 1
@string59 = private unnamed_addr constant [4 x i8] c"f16\00", align 1
@string60 = private unnamed_addr constant [4 x i8] c"f32\00", align 1
@string61 = private unnamed_addr constant [4 x i8] c"f64\00", align 1
@string62 = private unnamed_addr constant [4 x i8] c"any\00", align 1
@string63 = private unnamed_addr constant [5 x i8] c"bool\00", align 1
@string64 = private unnamed_addr constant [5 x i8] c"byte\00", align 1
@string65 = private unnamed_addr constant [7 x i8] c"rawptr\00", align 1
@string66 = private unnamed_addr constant [7 x i8] c"string\00", align 1
@string67 = private unnamed_addr constant [10 x i8] c"TypeField\00", align 1
@string68 = private unnamed_addr constant [5 x i8] c"name\00", align 1
@string69 = private unnamed_addr constant [7 x i8] c"typeid\00", align 1
@string70 = private unnamed_addr constant [7 x i8] c"offset\00", align 1
@tfields71 = private constant [3 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string68, i64 4 }, i64 17, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string69, i64 6 }, i64 3, i64 ptrtoint (ptr getelementptr (%0, ptr null, i32 0, i32 1) to i64) }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string70, i64 6 }, i64 3, i64 ptrtoint (ptr getelementptr (%0, ptr null, i32 0, i32 2) to i64) }]
@string72 = private unnamed_addr constant [12 x i8] c"[]TypeField\00", align 1
@string73 = private unnamed_addr constant [9 x i8] c"TypeInfo\00", align 1
@string74 = private unnamed_addr constant [5 x i8] c"kind\00", align 1
@string75 = private unnamed_addr constant [5 x i8] c"size\00", align 1
@string76 = private unnamed_addr constant [5 x i8] c"elem\00", align 1
@string77 = private unnamed_addr constant [6 x i8] c"count\00", align 1
@string78 = private unnamed_addr constant [7 x i8] c"fields\00", align 1
@tfields79 = private constant [6 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string68, i64 4 }, i64 17, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string74, i64 4 }, i64 3, i64 ptrtoint (ptr getelementptr (%1, ptr null, i32 0, i32 1) to i64) }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string75, i64 4 }, i64 3, i64 ptrtoint (ptr getelementptr (%1, ptr null, i32 0, i32 2) to i64) }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string76, i64 4 }, i64 3, i64 ptrtoint (ptr getelementptr (%1, ptr null, i32 0, i32 3) to i64) }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string77, i64 5 }, i64 3, i64 ptrtoint (ptr getelementptr (%1, ptr null, i32 0, i32 4) to i64) }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string78, i64 6 }, i64 19, i64 ptrtoint (ptr getelementptr (%1, ptr null, i32 0, i32 5) to i64) }]
@string80 = private unnamed_addr constant [6 x i8] c"^byte\00", align 1
@string81 = private unnamed_addr constant [6 x i8] c"[]any\00", align 1
@string82 = private unnamed_addr constant [15 x i8] c"fn(^byte): i32\00", align 1
@string83 = private unnamed_addr constant [23 x i8] c"fn(rawptr, ^byte): i32\00", align 1
@string84 = private unnamed_addr constant [27 x i8] c"fn(^byte, u64, ^byte): i32\00", align 1
@string85 = private unnamed_addr constant [25 x i8] c"fn(^byte, ^byte): rawptr\00", align 1
@string86 = private unnamed_addr constant [16 x i8] c"fn(rawptr): i32\00", align 1
@string87 = private unnamed_addr constant [23 x i8] c"fn(^byte, rawptr): i32\00", align 1
@string88 = private unnamed_addr constant [33 x i8] c"fn(^byte, u64, u64, rawptr): u64\00", align 1
@string89 = private unnamed_addr constant [26 x i8] c"fn(rawptr, i64, i32): i32\00", align 1
@string90 = private unnamed_addr constant [16 x i8] c"fn(rawptr): i64\00", align 1
@string91 = private unnamed_addr constant [16 x i8] c"fn(u64): rawptr\00", align 1
@string92 = private unnamed_addr constant [21 x i8] c"fn(u64, u64): rawptr\00", align 1
@string93 = private unnamed_addr constant [24 x i8] c"fn(rawptr, u64): rawptr\00", align 1
@string94 = private unnamed_addr constant [11 x i8] c"fn(rawptr)\00", align 1
@string95 = private unnamed_addr constant [32 x i8] c"fn(rawptr, rawptr, u64): rawptr\00", align 1
@string96 = private unnamed_addr constant [29 x i8] c"fn(rawptr, i32, u64): rawptr\00", align 1
@string97 = private unnamed_addr constant [29 x i8] c"fn(rawptr, rawptr, u64): i32\00", align 1
@string98 = private unnamed_addr constant [15 x i8] c"fn(^byte): u64\00", align 1
@string99 = private unnamed_addr constant [22 x i8] c"fn(^byte, ^byte): i32\00", align 1
@string100 = private unnamed_addr constant [27 x i8] c"fn(^byte, ^byte, u64): i32\00", align 1
@string101 = private unnamed_addr constant [24 x i8] c"fn(^byte, ^byte): ^byte\00", align 1
@string102 = private unnamed_addr constant [29 x i8] c"fn(^byte, ^byte, u64): ^byte\00", align 1
@string103 = private unnamed_addr constant [22 x i8] c"fn(^byte, i32): ^byte\00", align 1
@string104 = private unnamed_addr constant [15 x i8] c"fn(^byte): i64\00", align 1
@string105 = private unnamed_addr constant [7 x i8] c"^^byte\00", align 1
@string106 = private unnamed_addr constant [28 x i8] c"fn(^byte, ^^byte, i32): i64\00", align 1
@string107 = private unnamed_addr constant [28 x i8] c"fn(^byte, ^^byte, i32): u64\00", align 1
@string108 = private unnamed_addr constant [23 x i8] c"fn(^byte, ^^byte): f32\00", align 1
@string109 = private unnamed_addr constant [23 x i8] c"fn(^byte, ^^byte): f64\00", align 1
@string110 = private unnamed_addr constant [10 x i8] c"fn(): i32\00", align 1
@string111 = private unnamed_addr constant [8 x i8] c"fn(u32)\00", align 1
@string112 = private unnamed_addr constant [8 x i8] c"fn(i32)\00", align 1
@string113 = private unnamed_addr constant [5 x i8] c"fn()\00", align 1
@string114 = private unnamed_addr constant [14 x i8] c"fn(fn()): i32\00", align 1
@string115 = private unnamed_addr constant [17 x i8] c"fn(^byte): ^byte\00", align 1
@string116 = private unnamed_addr constant [13 x i8] c"fn(f64): f64\00", align 1
@string117 = private unnamed_addr constant [18 x i8] c"fn(f64, f64): f64\00", align 1
@string118 = private unnamed_addr constant [13 x i8] c"fn(f32): f32\00", align 1
@string119 = private unnamed_addr constant [18 x i8] c"fn(f32, f32): f32\00", align 1
@string120 = private unnamed_addr constant [8 x i8] c"Builder\00", align 1
@string121 = private unnamed_addr constant [5 x i8] c"data\00", align 1
@tfields122 = private constant [2 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string121, i64 4 }, i64 110, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string77, i64 5 }, i64 3, i64 ptrtoint (ptr getelementptr (%2, ptr null, i32 0, i32 1) to i64) }]
@string123 = private unnamed_addr constant [9 x i8] c"Timespec\00", align 1
@string124 = private unnamed_addr constant [7 x i8] c"tv_sec\00", align 1
@string125 = private unnamed_addr constant [8 x i8] c"tv_nsec\00", align 1
@tfields126 = private constant [2 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string124, i64 6 }, i64 3, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string125, i64 7 }, i64 3, i64 ptrtoint (ptr getelementptr (%3, ptr null, i32 0, i32 1) to i64) }]
@string127 = private unnamed_addr constant [11 x i8] c"[4096]byte\00", align 1
@string128 = private unnamed_addr constant [25 x i8] c"fn(i32, ^byte, i64): i64\00", align 1
@string129 = private unnamed_addr constant [18 x i8] c"fn(string): ^byte\00", align 1
@string130 = private unnamed_addr constant [25 x i8] c"fn(string, string): bool\00", align 1
@string131 = private unnamed_addr constant [14 x i8] c"fn(): Builder\00", align 1
@string132 = private unnamed_addr constant [9 x i8] c"^Builder\00", align 1
@string133 = private unnamed_addr constant [19 x i8] c"fn(^Builder, byte)\00", align 1
@string134 = private unnamed_addr constant [21 x i8] c"fn(^Builder, string)\00", align 1
@string135 = private unnamed_addr constant [18 x i8] c"fn(^Builder, u64)\00", align 1
@string136 = private unnamed_addr constant [7 x i8] c"[]byte\00", align 1
@string137 = private unnamed_addr constant [21 x i8] c"fn(^Builder): []byte\00", align 1
@string138 = private unnamed_addr constant [21 x i8] c"fn(i32, []byte): i64\00", align 1
@string139 = private unnamed_addr constant [16 x i8] c"fn([]byte): i64\00", align 1
@string140 = private unnamed_addr constant [23 x i8] c"fn(^Builder, i32): i64\00", align 1
@string141 = private unnamed_addr constant [28 x i8] c"fn(^Builder, u64, u64, i64)\00", align 1
@string142 = private unnamed_addr constant [18 x i8] c"fn(^Builder, i64)\00", align 1
@string143 = private unnamed_addr constant [18 x i8] c"fn(^Builder, f64)\00", align 1
@string144 = private unnamed_addr constant [5 x i8] c"^i64\00", align 1
@string145 = private unnamed_addr constant [5 x i8] c"^i32\00", align 1
@string146 = private unnamed_addr constant [5 x i8] c"^i16\00", align 1
@string147 = private unnamed_addr constant [4 x i8] c"^i8\00", align 1
@string148 = private unnamed_addr constant [26 x i8] c"fn(^Builder, rawptr, i64)\00", align 1
@string149 = private unnamed_addr constant [5 x i8] c"^u64\00", align 1
@string150 = private unnamed_addr constant [5 x i8] c"^u32\00", align 1
@string151 = private unnamed_addr constant [5 x i8] c"^u16\00", align 1
@string152 = private unnamed_addr constant [4 x i8] c"^u8\00", align 1
@string153 = private unnamed_addr constant [5 x i8] c"^f64\00", align 1
@string154 = private unnamed_addr constant [5 x i8] c"^f32\00", align 1
@string155 = private unnamed_addr constant [10 x i8] c"^TypeInfo\00", align 1
@string156 = private unnamed_addr constant [32 x i8] c"fn(^Builder, rawptr, ^TypeInfo)\00", align 1
@string157 = private unnamed_addr constant [8 x i8] c"^rawptr\00", align 1
@string158 = private unnamed_addr constant [6 x i8] c"^bool\00", align 1
@string159 = private unnamed_addr constant [8 x i8] c"^string\00", align 1
@string160 = private unnamed_addr constant [5 x i8] c"^any\00", align 1
@string161 = private unnamed_addr constant [32 x i8] c"fn(^Builder, rawptr, i64, bool)\00", align 1
@string162 = private unnamed_addr constant [18 x i8] c"fn(^Builder, any)\00", align 1
@string163 = private unnamed_addr constant [28 x i8] c"fn(string, i64, byte): bool\00", align 1
@string164 = private unnamed_addr constant [17 x i8] c"fn(string): bool\00", align 1
@string165 = private unnamed_addr constant [28 x i8] c"fn(^Builder, string, []any)\00", align 1
@string166 = private unnamed_addr constant [19 x i8] c"fn([]any): ^string\00", align 1
@string167 = private unnamed_addr constant [26 x i8] c"fn(^Builder, []any, bool)\00", align 1
@string168 = private unnamed_addr constant [11 x i8] c"fn(): bool\00", align 1
@string169 = private unnamed_addr constant [21 x i8] c"fn(i32, []any, bool)\00", align 1
@string170 = private unnamed_addr constant [23 x i8] c"fn(i32, string, []any)\00", align 1
@string171 = private unnamed_addr constant [11 x i8] c"fn(string)\00", align 1
@string172 = private unnamed_addr constant [16 x i8] c"fn(i32, string)\00", align 1
@string173 = private unnamed_addr constant [13 x i8] c"fn(^Builder)\00", align 1
@string174 = private unnamed_addr constant [10 x i8] c"^Timespec\00", align 1
@string175 = private unnamed_addr constant [30 x i8] c"fn(^Timespec, ^Timespec): i32\00", align 1
@string176 = private unnamed_addr constant [8 x i8] c"fn(i64)\00", align 1
@string177 = private unnamed_addr constant [3 x i8] c"s1\00", align 1
@string178 = private unnamed_addr constant [2 x i8] c"a\00", align 1
@string179 = private unnamed_addr constant [2 x i8] c"b\00", align 1
@tfields180 = private constant [2 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string178, i64 1 }, i64 3, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string179, i64 1 }, i64 3, i64 ptrtoint (ptr getelementptr (%4, ptr null, i32 0, i32 1) to i64) }]
@string181 = private unnamed_addr constant [3 x i8] c"s2\00", align 1
@tfields182 = private constant [2 x %gala_151743._core_cfunctions_posix.TypeField] [%gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string178, i64 1 }, i64 3, i64 0 }, %gala_151743._core_cfunctions_posix.TypeField { { ptr, i64 } { ptr @string179, i64 1 }, i64 3, i64 ptrtoint (ptr getelementptr (%5, ptr null, i32 0, i32 1) to i64) }]
@string183 = private unnamed_addr constant [16 x i8] c"fn(s1, s2): i32\00", align 1
@string184 = private unnamed_addr constant [17 x i8] c"fn(any): ^string\00", align 1
@string185 = private unnamed_addr constant [18 x i8] c"fn([]byte): ^byte\00", align 1
@gala_151743._core_cfunctions_posix.SEEK_SET = global i32 0
@gala_151743._core_cfunctions_posix.SEEK_CUR = global i32 0
@gala_151743._core_cfunctions_posix.SEEK_END = global i32 0
@__gala_types = constant [186 x %gala_151743._core_cfunctions_posix.TypeInfo] [%gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string50, i64 2 }, i64 1, i64 1, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string51, i64 3 }, i64 1, i64 2, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string52, i64 3 }, i64 1, i64 4, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string53, i64 3 }, i64 1, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string54, i64 2 }, i64 2, i64 1, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string55, i64 3 }, i64 2, i64 2, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string56, i64 3 }, i64 2, i64 4, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string57, i64 3 }, i64 2, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string58, i64 2 }, i64 3, i64 1, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string59, i64 3 }, i64 3, i64 2, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string60, i64 3 }, i64 3, i64 4, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string61, i64 3 }, i64 3, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string62, i64 3 }, i64 14, i64 16, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string63, i64 4 }, i64 4, i64 1, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string64, i64 4 }, i64 5, i64 1, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string65, i64 6 }, i64 8, i64 8, i64 12, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string66, i64 6 }, i64 7, i64 16, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string67, i64 9 }, i64 11, i64 32, i64 0, i64 0, { ptr, i64 } { ptr @tfields71, i64 3 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string72, i64 11 }, i64 9, i64 16, i64 18, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string73, i64 8 }, i64 11, i64 64, i64 0, i64 0, { ptr, i64 } { ptr @tfields79, i64 6 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string80, i64 5 }, i64 8, i64 8, i64 15, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string81, i64 5 }, i64 9, i64 16, i64 13, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string82, i64 14 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string83, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string84, i64 26 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string85, i64 24 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string86, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string87, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string88, i64 32 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string88, i64 32 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string86, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string89, i64 25 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string90, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string91, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string92, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string93, i64 23 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string94, i64 10 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string95, i64 31 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string95, i64 31 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string96, i64 28 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string97, i64 28 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string98, i64 14 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string99, i64 21 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string100, i64 26 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string101, i64 23 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string102, i64 28 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string101, i64 23 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string102, i64 28 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string103, i64 21 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string103, i64 21 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string82, i64 14 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string104, i64 14 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string104, i64 14 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string105, i64 6 }, i64 8, i64 8, i64 21, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string106, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string106, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string107, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string107, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string108, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string109, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string110, i64 9 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string111, i64 7 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string90, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string112, i64 7 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string113, i64 4 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string113, i64 4 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string114, i64 13 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string115, i64 16 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string117, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string117, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string116, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string119, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string119, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string118, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string120, i64 7 }, i64 11, i64 4104, i64 0, i64 0, { ptr, i64 } { ptr @tfields122, i64 2 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string123, i64 8 }, i64 11, i64 16, i64 0, i64 0, { ptr, i64 } { ptr @tfields126, i64 2 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string127, i64 10 }, i64 10, i64 4096, i64 15, i64 4096, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string128, i64 24 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string93, i64 23 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string129, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string130, i64 24 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string131, i64 13 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string132, i64 8 }, i64 8, i64 8, i64 108, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string133, i64 18 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string134, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string135, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string136, i64 6 }, i64 9, i64 16, i64 15, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string137, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string138, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string139, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string140, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string141, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string135, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string142, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string135, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string143, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string144, i64 4 }, i64 8, i64 8, i64 3, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string145, i64 4 }, i64 8, i64 8, i64 2, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string146, i64 4 }, i64 8, i64 8, i64 1, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string147, i64 3 }, i64 8, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string148, i64 25 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string149, i64 4 }, i64 8, i64 8, i64 7, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string150, i64 4 }, i64 8, i64 8, i64 6, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string151, i64 4 }, i64 8, i64 8, i64 5, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string152, i64 3 }, i64 8, i64 8, i64 4, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string148, i64 25 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string153, i64 4 }, i64 8, i64 8, i64 11, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string154, i64 4 }, i64 8, i64 8, i64 10, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string148, i64 25 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string155, i64 9 }, i64 8, i64 8, i64 20, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string156, i64 31 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string157, i64 7 }, i64 8, i64 8, i64 16, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string156, i64 31 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string158, i64 5 }, i64 8, i64 8, i64 14, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string159, i64 7 }, i64 8, i64 8, i64 17, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string160, i64 4 }, i64 8, i64 8, i64 13, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string161, i64 31 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string162, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string163, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string164, i64 16 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string165, i64 27 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string166, i64 18 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string167, i64 25 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string168, i64 10 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string169, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string170, i64 22 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string113, i64 4 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string113, i64 4 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string171, i64 10 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string113, i64 4 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string171, i64 10 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string112, i64 7 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string172, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string173, i64 12 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string134, i64 20 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string174, i64 9 }, i64 8, i64 8, i64 109, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string175, i64 29 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string176, i64 7 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo zeroinitializer, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string177, i64 2 }, i64 11, i64 16, i64 0, i64 0, { ptr, i64 } { ptr @tfields180, i64 2 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string181, i64 2 }, i64 11, i64 16, i64 0, i64 0, { ptr, i64 } { ptr @tfields182, i64 2 } }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string110, i64 9 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string183, i64 15 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string184, i64 16 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }, %gala_151743._core_cfunctions_posix.TypeInfo { { ptr, i64 } { ptr @string185, i64 17 }, i64 13, i64 8, i64 0, i64 0, { ptr, i64 } zeroinitializer }]

define ptr @gala_104923._core_core_posix.ptr_add(ptr %p, i64 %bytes) {
entry:
  %0 = ptrtoint ptr %p to i64
  %1 = add i64 %0, %bytes
  %2 = inttoptr i64 %1 to ptr
  ret ptr %2
}

define ptr @gala_104923._core_core_posix.str_data({ ptr, i64 } %s) {
entry:
  %0 = extractvalue { ptr, i64 } %s, 1
  %1 = icmp eq i64 %0, 0
  br i1 %1, label %base_block_label24, label %end_label24

base_block_label24:                               ; preds = %entry
  %2 = inttoptr i64 0 to ptr
  ret ptr %2

end_label24:                                      ; preds = %entry
  %3 = extractvalue { ptr, i64 } %s, 0
  %4 = getelementptr inbounds i8, ptr %3, i64 0
  ret ptr %4
}

define zeroext i1 @gala_104923._core_core_posix.str_eq({ ptr, i64 } %a, { ptr, i64 } %b) {
entry:
  %i.302 = alloca i64, align 8
  %0 = extractvalue { ptr, i64 } %a, 1
  %1 = extractvalue { ptr, i64 } %b, 1
  %2 = icmp ne i64 %0, %1
  br i1 %2, label %base_block_label30, label %end_label30

base_block_label30:                               ; preds = %entry
  ret i1 false

end_label30:                                      ; preds = %entry
  store i64 0, ptr %i.302, align 8
  br label %while_cond_label34

while_cond_label34:                               ; preds = %end_label38, %end_label30
  %3 = load i64, ptr %i.302, align 8
  %4 = extractvalue { ptr, i64 } %a, 1
  %5 = icmp slt i64 %3, %4
  br i1 %5, label %while_body_label34, label %while_end_label34

while_body_label34:                               ; preds = %while_cond_label34
  %6 = extractvalue { ptr, i64 } %a, 0
  %7 = load i64, ptr %i.302, align 8
  %8 = getelementptr inbounds i8, ptr %6, i64 %7
  %9 = load i8, ptr %8, align 1
  %10 = extractvalue { ptr, i64 } %b, 0
  %11 = load i64, ptr %i.302, align 8
  %12 = getelementptr inbounds i8, ptr %10, i64 %11
  %13 = load i8, ptr %12, align 1
  %14 = icmp ne i8 %9, %13
  br i1 %14, label %base_block_label38, label %end_label38

base_block_label38:                               ; preds = %while_body_label34
  ret i1 false

end_label38:                                      ; preds = %while_body_label34
  %15 = load i64, ptr %i.302, align 8
  %16 = add i64 %15, 1
  store i64 %16, ptr %i.302, align 8
  br label %while_cond_label34

while_end_label34:                                ; preds = %while_cond_label34
  ret i1 true
}

define void @gala_104923._core_core_posix.builder_make(ptr sret(%gala_104923._core_core_posix.Builder) align 8 %.sret) {
entry:
  %0 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %.sret, i32 0, i32 0
  call void @llvm.memset.p0.i64(ptr %0, i8 0, i64 4096, i1 false)
  %1 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %.sret, i32 0, i32 1
  store i64 0, ptr %1, align 8
  ret void
}

; Function Attrs: nocallback nofree nounwind willreturn memory(argmem: write)
declare void @llvm.memset.p0.i64(ptr nocapture writeonly, i8, i64, i1 immarg) #0

define void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %c) {
entry:
  %0 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 1
  %1 = load i64, ptr %0, align 8
  %2 = icmp slt i64 %1, 4096
  br i1 %2, label %base_block_label52, label %end_label52

base_block_label52:                               ; preds = %entry
  %3 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 0
  %4 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 1
  %5 = load i64, ptr %4, align 8
  %6 = getelementptr inbounds i8, ptr %3, i64 %5
  store i8 %c, ptr %6, align 1
  %7 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 1
  %8 = load i64, ptr %7, align 8
  %9 = add i64 %8, 1
  store i64 %9, ptr %7, align 8
  br label %end_label52

end_label52:                                      ; preds = %base_block_label52, %entry
  ret void
}

define void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %s) {
entry:
  %i.307 = alloca i64, align 8
  store i64 0, ptr %i.307, align 8
  br label %while_cond_label63

while_cond_label63:                               ; preds = %while_body_label63, %entry
  %0 = load i64, ptr %i.307, align 8
  %1 = extractvalue { ptr, i64 } %s, 1
  %2 = icmp slt i64 %0, %1
  br i1 %2, label %while_body_label63, label %while_end_label63

while_body_label63:                               ; preds = %while_cond_label63
  %3 = extractvalue { ptr, i64 } %s, 0
  %4 = load i64, ptr %i.307, align 8
  %5 = getelementptr inbounds i8, ptr %3, i64 %4
  %6 = load i8, ptr %5, align 1
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %6)
  %7 = load i64, ptr %i.307, align 8
  %8 = add i64 %7, 1
  store i64 %8, ptr %i.307, align 8
  br label %while_cond_label63

while_end_label63:                                ; preds = %while_cond_label63
  ret void
}

define void @gala_104923._core_core_posix.builder_push_digit(ptr %b, i64 %d) {
entry:
  %0 = icmp ult i64 %d, 10
  br i1 %0, label %base_block_label73, label %else_block_label73

base_block_label73:                               ; preds = %entry
  %1 = add i64 48, %d
  %2 = trunc i64 %1 to i8
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %2)
  br label %end_label73

else_block_label73:                               ; preds = %entry
  %3 = add i64 97, %d
  %4 = sub i64 %3, 10
  %5 = trunc i64 %4 to i8
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %5)
  br label %end_label73

end_label73:                                      ; preds = %else_block_label73, %base_block_label73
  ret void
}

define { ptr, i64 } @gala_104923._core_core_posix.builder_bytes(ptr %b) {
entry:
  %0 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 1
  %1 = load i64, ptr %0, align 8
  %2 = sub nuw nsw i64 %1, 0
  %3 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 0
  %4 = getelementptr inbounds i8, ptr %3, i64 0
  %5 = insertvalue { ptr, i64 } undef, ptr %4, 0
  %6 = insertvalue { ptr, i64 } %5, i64 %2, 1
  ret { ptr, i64 } %6
}

define i64 @gala_104923._core_core_posix.write_fd(i32 %fd, { ptr, i64 } %data) {
entry:
  %total.313 = alloca i64, align 8
  %size.314 = alloca i64, align 8
  %rest.315 = alloca { ptr, i64 }, align 8
  %written.316 = alloca i64, align 8
  store i64 0, ptr %total.313, align 8
  %0 = extractvalue { ptr, i64 } %data, 1
  store i64 %0, ptr %size.314, align 8
  br label %while_cond_label88

while_cond_label88:                               ; preds = %end_label104, %entry
  %1 = load i64, ptr %total.313, align 8
  %2 = load i64, ptr %size.314, align 8
  %3 = icmp slt i64 %1, %2
  br i1 %3, label %while_body_label88, label %while_end_label88

while_body_label88:                               ; preds = %while_cond_label88
  %4 = load i64, ptr %total.313, align 8
  %5 = extractvalue { ptr, i64 } %data, 1
  %6 = sub nuw nsw i64 %5, %4
  %7 = extractvalue { ptr, i64 } %data, 0
  %8 = getelementptr inbounds i8, ptr %7, i64 %4
  %9 = insertvalue { ptr, i64 } undef, ptr %8, 0
  %10 = insertvalue { ptr, i64 } %9, i64 %6, 1
  store { ptr, i64 } %10, ptr %rest.315, align 8
  %11 = load { ptr, i64 }, ptr %rest.315, align 8
  %12 = call ptr @gala_104923._core_core_posix.raw_data.a_byte.v0({ ptr, i64 } %11)
  %13 = load { ptr, i64 }, ptr %rest.315, align 8
  %14 = extractvalue { ptr, i64 } %13, 1
  %15 = call i64 @write(i32 %fd, ptr %12, i64 %14)
  store i64 %15, ptr %written.316, align 8
  %16 = load i64, ptr %written.316, align 8
  %17 = icmp sle i64 %16, 0
  br i1 %17, label %base_block_label104, label %end_label104

base_block_label104:                              ; preds = %while_body_label88
  %18 = sub i64 0, 1
  ret i64 %18

end_label104:                                     ; preds = %while_body_label88
  %19 = load i64, ptr %total.313, align 8
  %20 = load i64, ptr %written.316, align 8
  %21 = add i64 %19, %20
  store i64 %21, ptr %total.313, align 8
  br label %while_cond_label88

while_end_label88:                                ; preds = %while_cond_label88
  %22 = load i64, ptr %total.313, align 8
  ret i64 %22
}

define ptr @gala_104923._core_core_posix.raw_data.a_byte.v0({ ptr, i64 } %v) {
entry:
  %0 = extractvalue { ptr, i64 } %v, 1
  %1 = icmp eq i64 %0, 0
  br i1 %1, label %base_block_label593, label %end_label593

base_block_label593:                              ; preds = %entry
  %2 = inttoptr i64 0 to ptr
  ret ptr %2

end_label593:                                     ; preds = %entry
  %3 = extractvalue { ptr, i64 } %v, 0
  %4 = getelementptr inbounds i8, ptr %3, i64 0
  ret ptr %4
}

declare i64 @write(i32, ptr, i64)

define i64 @gala_104923._core_core_posix.write_stdout({ ptr, i64 } %data) {
entry:
  %0 = call i64 @gala_104923._core_core_posix.write_fd(i32 1, { ptr, i64 } %data)
  ret i64 %0
}

define i64 @gala_104923._core_core_posix.builder_flush(ptr %b, i32 %fd) {
entry:
  %written.320 = alloca i64, align 8
  %0 = call { ptr, i64 } @gala_104923._core_core_posix.builder_bytes(ptr %b)
  %1 = call i64 @gala_104923._core_core_posix.write_fd(i32 %fd, { ptr, i64 } %0)
  store i64 %1, ptr %written.320, align 8
  %2 = getelementptr inbounds %gala_104923._core_core_posix.Builder, ptr %b, i32 0, i32 1
  store i64 0, ptr %2, align 8
  %3 = load i64, ptr %written.320, align 8
  ret i64 %3
}

define void @gala_104923._core_core_posix.fmt_u64_padded(ptr %b, i64 %v, i64 %base, i64 %width) {
entry:
  %div.325 = alloca i64, align 8
  %digits.326 = alloca i64, align 8
  store i64 1, ptr %div.325, align 8
  store i64 1, ptr %digits.326, align 8
  br label %while_cond_label117

while_cond_label117:                              ; preds = %while_body_label117, %entry
  %0 = load i64, ptr %div.325, align 8
  %1 = udiv i64 %v, %0
  %2 = icmp uge i64 %1, %base
  br i1 %2, label %while_body_label117, label %while_end_label117

while_body_label117:                              ; preds = %while_cond_label117
  %3 = load i64, ptr %div.325, align 8
  %4 = mul i64 %3, %base
  store i64 %4, ptr %div.325, align 8
  %5 = load i64, ptr %digits.326, align 8
  %6 = add i64 %5, 1
  store i64 %6, ptr %digits.326, align 8
  br label %while_cond_label117

while_end_label117:                               ; preds = %while_cond_label117
  br label %while_cond_label125

while_cond_label125:                              ; preds = %while_body_label125, %while_end_label117
  %7 = load i64, ptr %digits.326, align 8
  %8 = icmp slt i64 %7, %width
  br i1 %8, label %while_body_label125, label %while_end_label125

while_body_label125:                              ; preds = %while_cond_label125
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 48)
  %9 = load i64, ptr %digits.326, align 8
  %10 = add i64 %9, 1
  store i64 %10, ptr %digits.326, align 8
  br label %while_cond_label125

while_end_label125:                               ; preds = %while_cond_label125
  br label %while_cond_label130

while_cond_label130:                              ; preds = %while_body_label130, %while_end_label125
  %11 = load i64, ptr %div.325, align 8
  %12 = icmp ugt i64 %11, 0
  br i1 %12, label %while_body_label130, label %while_end_label130

while_body_label130:                              ; preds = %while_cond_label130
  %13 = load i64, ptr %div.325, align 8
  %14 = udiv i64 %v, %13
  %15 = urem i64 %14, %base
  call void @gala_104923._core_core_posix.builder_push_digit(ptr %b, i64 %15)
  %16 = load i64, ptr %div.325, align 8
  %17 = udiv i64 %16, %base
  store i64 %17, ptr %div.325, align 8
  br label %while_cond_label130

while_end_label130:                               ; preds = %while_cond_label130
  ret void
}

define void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %v) {
entry:
  call void @gala_104923._core_core_posix.fmt_u64_padded(ptr %b, i64 %v, i64 10, i64 1)
  ret void
}

define void @gala_104923._core_core_posix.fmt_i64(ptr %b, i64 %v) {
entry:
  %0 = icmp slt i64 %v, 0
  br i1 %0, label %base_block_label138, label %else_block_label138

base_block_label138:                              ; preds = %entry
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 45)
  %1 = sub i64 0, %v
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %1)
  br label %end_label138

else_block_label138:                              ; preds = %entry
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %v)
  br label %end_label138

end_label138:                                     ; preds = %else_block_label138, %base_block_label138
  ret void
}

define void @gala_104923._core_core_posix.fmt_hex(ptr %b, i64 %v) {
entry:
  %0 = getelementptr inbounds [3 x i8], ptr @string7, i64 0, i64 0
  %1 = insertvalue { ptr, i64 } undef, ptr %0, 0
  %2 = insertvalue { ptr, i64 } %1, i64 2, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %2)
  call void @gala_104923._core_core_posix.fmt_u64_padded(ptr %b, i64 %v, i64 16, i64 1)
  ret void
}

define void @gala_104923._core_core_posix.fmt_f64(ptr %b, double %x) {
entry:
  %v.335 = alloca double, align 8
  %int_part.336 = alloca i64, align 8
  %frac.337 = alloca i64, align 8
  %frac_digits.338 = alloca i64, align 8
  %slot186 = alloca i1, align 1
  %0 = fcmp une double %x, %x
  br i1 %0, label %base_block_label144, label %end_label144

base_block_label144:                              ; preds = %entry
  %1 = getelementptr inbounds [4 x i8], ptr @string8, i64 0, i64 0
  %2 = insertvalue { ptr, i64 } undef, ptr %1, 0
  %3 = insertvalue { ptr, i64 } %2, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %3)
  ret void

end_label144:                                     ; preds = %entry
  store double %x, ptr %v.335, align 8
  %4 = load double, ptr %v.335, align 8
  %5 = fcmp olt double %4, 0.000000e+00
  br i1 %5, label %base_block_label149, label %end_label149

base_block_label149:                              ; preds = %end_label144
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 45)
  %6 = load double, ptr %v.335, align 8
  %7 = fsub double 0.000000e+00, %6
  store double %7, ptr %v.335, align 8
  br label %end_label149

end_label149:                                     ; preds = %base_block_label149, %end_label144
  %8 = load double, ptr %v.335, align 8
  %9 = load double, ptr %v.335, align 8
  %10 = fsub double %8, %9
  %11 = fcmp une double %10, 0.000000e+00
  br i1 %11, label %base_block_label154, label %end_label154

base_block_label154:                              ; preds = %end_label149
  %12 = getelementptr inbounds [4 x i8], ptr @string9, i64 0, i64 0
  %13 = insertvalue { ptr, i64 } undef, ptr %12, 0
  %14 = insertvalue { ptr, i64 } %13, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %14)
  ret void

end_label154:                                     ; preds = %end_label149
  %15 = load double, ptr %v.335, align 8
  %16 = fcmp oge double %15, 0x43F0000000000000
  br i1 %16, label %base_block_label162, label %end_label162

base_block_label162:                              ; preds = %end_label154
  %17 = getelementptr inbounds [10 x i8], ptr @string10, i64 0, i64 0
  %18 = insertvalue { ptr, i64 } undef, ptr %17, 0
  %19 = insertvalue { ptr, i64 } %18, i64 9, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %19)
  ret void

end_label162:                                     ; preds = %end_label154
  %20 = load double, ptr %v.335, align 8
  %21 = fptoui double %20 to i64
  store i64 %21, ptr %int_part.336, align 8
  %22 = load double, ptr %v.335, align 8
  %23 = load i64, ptr %int_part.336, align 8
  %24 = uitofp i64 %23 to double
  %25 = fsub double %22, %24
  %26 = fmul double %25, 1.000000e+06
  %27 = fadd double %26, 5.000000e-01
  %28 = fptoui double %27 to i64
  store i64 %28, ptr %frac.337, align 8
  %29 = load i64, ptr %frac.337, align 8
  %30 = icmp uge i64 %29, 1000000
  br i1 %30, label %base_block_label177, label %end_label177

base_block_label177:                              ; preds = %end_label162
  %31 = load i64, ptr %int_part.336, align 8
  %32 = add i64 %31, 1
  store i64 %32, ptr %int_part.336, align 8
  %33 = load i64, ptr %frac.337, align 8
  %34 = sub i64 %33, 1000000
  store i64 %34, ptr %frac.337, align 8
  br label %end_label177

end_label177:                                     ; preds = %base_block_label177, %end_label162
  %35 = load i64, ptr %int_part.336, align 8
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %35)
  store i64 6, ptr %frac_digits.338, align 8
  br label %while_cond_label185

while_cond_label185:                              ; preds = %while_body_label185, %end_label177
  %36 = load i64, ptr %frac_digits.338, align 8
  %37 = icmp sgt i64 %36, 0
  store i1 %37, ptr %slot186, align 1
  br i1 %37, label %logic.rhs.187, label %logic.end.187

logic.rhs.187:                                    ; preds = %while_cond_label185
  %38 = load i64, ptr %frac.337, align 8
  %39 = urem i64 %38, 10
  %40 = icmp eq i64 %39, 0
  store i1 %40, ptr %slot186, align 1
  br label %logic.end.187

logic.end.187:                                    ; preds = %logic.rhs.187, %while_cond_label185
  %41 = load i1, ptr %slot186, align 1
  br i1 %41, label %while_body_label185, label %while_end_label185

while_body_label185:                              ; preds = %logic.end.187
  %42 = load i64, ptr %frac.337, align 8
  %43 = udiv i64 %42, 10
  store i64 %43, ptr %frac.337, align 8
  %44 = load i64, ptr %frac_digits.338, align 8
  %45 = sub i64 %44, 1
  store i64 %45, ptr %frac_digits.338, align 8
  br label %while_cond_label185

while_end_label185:                               ; preds = %logic.end.187
  %46 = load i64, ptr %frac_digits.338, align 8
  %47 = icmp sgt i64 %46, 0
  br i1 %47, label %base_block_label198, label %end_label198

base_block_label198:                              ; preds = %while_end_label185
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 46)
  %48 = load i64, ptr %frac.337, align 8
  %49 = load i64, ptr %frac_digits.338, align 8
  call void @gala_104923._core_core_posix.fmt_u64_padded(ptr %b, i64 %48, i64 10, i64 %49)
  br label %end_label198

end_label198:                                     ; preds = %base_block_label198, %while_end_label185
  ret void
}

define void @gala_104923._core_core_posix.fmt_int_at(ptr %b, ptr %data, i64 %size) {
entry:
  %p.342 = alloca ptr, align 8
  %p.343 = alloca ptr, align 8
  %p.344 = alloca ptr, align 8
  %p.345 = alloca ptr, align 8
  %0 = icmp eq i64 %size, 8
  br i1 %0, label %base_block_label203, label %alt_cond_label203_0

base_block_label203:                              ; preds = %entry
  %1 = bitcast ptr %data to ptr
  store ptr %1, ptr %p.342, align 8
  %2 = load ptr, ptr %p.342, align 8
  %3 = load i64, ptr %2, align 8
  call void @gala_104923._core_core_posix.fmt_i64(ptr %b, i64 %3)
  br label %end_label203

alt_cond_label203_0:                              ; preds = %entry
  %4 = icmp eq i64 %size, 4
  br i1 %4, label %alt_block_label203_0, label %alt_cond_label203_1

alt_block_label203_0:                             ; preds = %alt_cond_label203_0
  %5 = bitcast ptr %data to ptr
  store ptr %5, ptr %p.343, align 8
  %6 = load ptr, ptr %p.343, align 8
  %7 = load i32, ptr %6, align 4
  %8 = sext i32 %7 to i64
  call void @gala_104923._core_core_posix.fmt_i64(ptr %b, i64 %8)
  br label %end_label203

alt_cond_label203_1:                              ; preds = %alt_cond_label203_0
  %9 = icmp eq i64 %size, 2
  br i1 %9, label %alt_block_label203_1, label %else_block_label203

alt_block_label203_1:                             ; preds = %alt_cond_label203_1
  %10 = bitcast ptr %data to ptr
  store ptr %10, ptr %p.344, align 8
  %11 = load ptr, ptr %p.344, align 8
  %12 = load i16, ptr %11, align 2
  %13 = sext i16 %12 to i64
  call void @gala_104923._core_core_posix.fmt_i64(ptr %b, i64 %13)
  br label %end_label203

else_block_label203:                              ; preds = %alt_cond_label203_1
  %14 = bitcast ptr %data to ptr
  store ptr %14, ptr %p.345, align 8
  %15 = load ptr, ptr %p.345, align 8
  %16 = load i8, ptr %15, align 1
  %17 = sext i8 %16 to i64
  call void @gala_104923._core_core_posix.fmt_i64(ptr %b, i64 %17)
  br label %end_label203

end_label203:                                     ; preds = %else_block_label203, %alt_block_label203_1, %alt_block_label203_0, %base_block_label203
  ret void
}

define void @gala_104923._core_core_posix.fmt_uint_at(ptr %b, ptr %data, i64 %size) {
entry:
  %p.349 = alloca ptr, align 8
  %p.350 = alloca ptr, align 8
  %p.351 = alloca ptr, align 8
  %p.352 = alloca ptr, align 8
  %0 = icmp eq i64 %size, 8
  br i1 %0, label %base_block_label222, label %alt_cond_label222_0

base_block_label222:                              ; preds = %entry
  %1 = bitcast ptr %data to ptr
  store ptr %1, ptr %p.349, align 8
  %2 = load ptr, ptr %p.349, align 8
  %3 = load i64, ptr %2, align 8
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %3)
  br label %end_label222

alt_cond_label222_0:                              ; preds = %entry
  %4 = icmp eq i64 %size, 4
  br i1 %4, label %alt_block_label222_0, label %alt_cond_label222_1

alt_block_label222_0:                             ; preds = %alt_cond_label222_0
  %5 = bitcast ptr %data to ptr
  store ptr %5, ptr %p.350, align 8
  %6 = load ptr, ptr %p.350, align 8
  %7 = load i32, ptr %6, align 4
  %8 = zext i32 %7 to i64
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %8)
  br label %end_label222

alt_cond_label222_1:                              ; preds = %alt_cond_label222_0
  %9 = icmp eq i64 %size, 2
  br i1 %9, label %alt_block_label222_1, label %else_block_label222

alt_block_label222_1:                             ; preds = %alt_cond_label222_1
  %10 = bitcast ptr %data to ptr
  store ptr %10, ptr %p.351, align 8
  %11 = load ptr, ptr %p.351, align 8
  %12 = load i16, ptr %11, align 2
  %13 = zext i16 %12 to i64
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %13)
  br label %end_label222

else_block_label222:                              ; preds = %alt_cond_label222_1
  %14 = bitcast ptr %data to ptr
  store ptr %14, ptr %p.352, align 8
  %15 = load ptr, ptr %p.352, align 8
  %16 = load i8, ptr %15, align 1
  %17 = zext i8 %16 to i64
  call void @gala_104923._core_core_posix.fmt_u64(ptr %b, i64 %17)
  br label %end_label222

end_label222:                                     ; preds = %else_block_label222, %alt_block_label222_1, %alt_block_label222_0, %base_block_label222
  ret void
}

define void @gala_104923._core_core_posix.fmt_float_at(ptr %b, ptr %data, i64 %size) {
entry:
  %p.356 = alloca ptr, align 8
  %p.357 = alloca ptr, align 8
  %0 = icmp eq i64 %size, 8
  br i1 %0, label %base_block_label241, label %else_block_label241

base_block_label241:                              ; preds = %entry
  %1 = bitcast ptr %data to ptr
  store ptr %1, ptr %p.356, align 8
  %2 = load ptr, ptr %p.356, align 8
  %3 = load double, ptr %2, align 8
  call void @gala_104923._core_core_posix.fmt_f64(ptr %b, double %3)
  br label %end_label241

else_block_label241:                              ; preds = %entry
  %4 = bitcast ptr %data to ptr
  store ptr %4, ptr %p.357, align 8
  %5 = load ptr, ptr %p.357, align 8
  %6 = load float, ptr %5, align 4
  %7 = fpext float %6 to double
  call void @gala_104923._core_core_posix.fmt_f64(ptr %b, double %7)
  br label %end_label241

end_label241:                                     ; preds = %else_block_label241, %base_block_label241
  ret void
}

define void @gala_104923._core_core_posix.fmt_struct_at(ptr %b, ptr %data, ptr %info) {
entry:
  %i.361 = alloca i64, align 8
  %__for_iter0.443 = alloca { ptr, i64 }, align 8
  %__for_idx0.444 = alloca i64, align 8
  %f.362 = alloca %gala_151743._core_cfunctions_posix.TypeField, align 8
  %0 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 0
  %1 = load { ptr, i64 }, ptr %0, align 8
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %1)
  %2 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 5
  %3 = load { ptr, i64 }, ptr %2, align 8
  %4 = extractvalue { ptr, i64 } %3, 1
  %5 = icmp eq i64 %4, 0
  br i1 %5, label %base_block_label252, label %end_label252

base_block_label252:                              ; preds = %entry
  %6 = getelementptr inbounds [4 x i8], ptr @string11, i64 0, i64 0
  %7 = insertvalue { ptr, i64 } undef, ptr %6, 0
  %8 = insertvalue { ptr, i64 } %7, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %8)
  ret void

end_label252:                                     ; preds = %entry
  %9 = getelementptr inbounds [4 x i8], ptr @string12, i64 0, i64 0
  %10 = insertvalue { ptr, i64 } undef, ptr %9, 0
  %11 = insertvalue { ptr, i64 } %10, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %11)
  store i64 0, ptr %i.361, align 8
  %12 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 5
  %13 = load { ptr, i64 }, ptr %12, align 8
  store { ptr, i64 } %13, ptr %__for_iter0.443, align 8
  store i64 0, ptr %__for_idx0.444, align 8
  br label %while_cond_label265

while_cond_label265:                              ; preds = %end_label277, %end_label252
  %14 = load i64, ptr %__for_idx0.444, align 8
  %15 = load { ptr, i64 }, ptr %__for_iter0.443, align 8
  %16 = extractvalue { ptr, i64 } %15, 1
  %17 = icmp slt i64 %14, %16
  br i1 %17, label %while_body_label265, label %while_end_label265

while_body_label265:                              ; preds = %while_cond_label265
  %18 = load { ptr, i64 }, ptr %__for_iter0.443, align 8
  %19 = extractvalue { ptr, i64 } %18, 0
  %20 = load i64, ptr %__for_idx0.444, align 8
  %21 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeField, ptr %19, i64 %20
  %22 = load %gala_151743._core_cfunctions_posix.TypeField, ptr %21, align 8
  store %gala_151743._core_cfunctions_posix.TypeField %22, ptr %f.362, align 8
  %23 = load i64, ptr %__for_idx0.444, align 8
  %24 = add i64 %23, 1
  store i64 %24, ptr %__for_idx0.444, align 8
  %25 = load i64, ptr %i.361, align 8
  %26 = icmp sgt i64 %25, 0
  br i1 %26, label %base_block_label277, label %end_label277

base_block_label277:                              ; preds = %while_body_label265
  %27 = getelementptr inbounds [3 x i8], ptr @string13, i64 0, i64 0
  %28 = insertvalue { ptr, i64 } undef, ptr %27, 0
  %29 = insertvalue { ptr, i64 } %28, i64 2, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %29)
  br label %end_label277

end_label277:                                     ; preds = %base_block_label277, %while_body_label265
  %30 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeField, ptr %f.362, i32 0, i32 0
  %31 = load { ptr, i64 }, ptr %30, align 8
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %31)
  %32 = getelementptr inbounds [4 x i8], ptr @string14, i64 0, i64 0
  %33 = insertvalue { ptr, i64 } undef, ptr %32, 0
  %34 = insertvalue { ptr, i64 } %33, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %34)
  %35 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeField, ptr %f.362, i32 0, i32 2
  %36 = load i64, ptr %35, align 8
  %37 = call ptr @gala_104923._core_core_posix.ptr_add(ptr %data, i64 %36)
  %38 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeField, ptr %f.362, i32 0, i32 1
  %39 = load i64, ptr %38, align 8
  call void @gala_104923._core_core_posix.fmt_value(ptr %b, ptr %37, i64 %39, i1 zeroext true)
  %40 = load i64, ptr %i.361, align 8
  %41 = add i64 %40, 1
  store i64 %41, ptr %i.361, align 8
  br label %while_cond_label265

while_end_label265:                               ; preds = %while_cond_label265
  %42 = getelementptr inbounds [3 x i8], ptr @string15, i64 0, i64 0
  %43 = insertvalue { ptr, i64 } undef, ptr %42, 0
  %44 = insertvalue { ptr, i64 } %43, i64 2, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %44)
  ret void
}

define void @gala_104923._core_core_posix.fmt_value(ptr %b, ptr %data, i64 %id, i1 zeroext %nested) {
entry:
  %info.376 = alloca ptr, align 8
  %k.377 = alloca i64, align 8
  %slot345 = alloca i1, align 1
  %slot347 = alloca i1, align 1
  %bool_p.378 = alloca ptr, align 8
  %str_p.379 = alloca ptr, align 8
  %addr_p.380 = alloca ptr, align 8
  %any_p.381 = alloca ptr, align 8
  %slot408 = alloca i1, align 1
  %0 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr @__gala_types, i64 %id
  store ptr %0, ptr %info.376, align 8
  %1 = load ptr, ptr %info.376, align 8
  %2 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %1, i32 0, i32 1
  %3 = load i64, ptr %2, align 8
  store i64 %3, ptr %k.377, align 8
  %4 = load i64, ptr %k.377, align 8
  %5 = icmp eq i64 %4, 1
  br i1 %5, label %base_block_label339, label %alt_cond_label339_0

base_block_label339:                              ; preds = %entry
  %6 = load ptr, ptr %info.376, align 8
  %7 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %6, i32 0, i32 2
  %8 = load i64, ptr %7, align 8
  call void @gala_104923._core_core_posix.fmt_int_at(ptr %b, ptr %data, i64 %8)
  br label %end_label339

alt_cond_label339_0:                              ; preds = %entry
  %9 = load i64, ptr %k.377, align 8
  %10 = icmp eq i64 %9, 2
  store i1 %10, ptr %slot347, align 1
  br i1 %10, label %logic.end.348, label %logic.rhs.348

logic.rhs.348:                                    ; preds = %alt_cond_label339_0
  %11 = load i64, ptr %k.377, align 8
  %12 = icmp eq i64 %11, 5
  store i1 %12, ptr %slot347, align 1
  br label %logic.end.348

logic.end.348:                                    ; preds = %logic.rhs.348, %alt_cond_label339_0
  %13 = load i1, ptr %slot347, align 1
  store i1 %13, ptr %slot345, align 1
  br i1 %13, label %logic.end.346, label %logic.rhs.346

logic.rhs.346:                                    ; preds = %logic.end.348
  %14 = load i64, ptr %k.377, align 8
  %15 = icmp eq i64 %14, 6
  store i1 %15, ptr %slot345, align 1
  br label %logic.end.346

logic.end.346:                                    ; preds = %logic.rhs.346, %logic.end.348
  %16 = load i1, ptr %slot345, align 1
  br i1 %16, label %alt_block_label339_0, label %alt_cond_label339_1

alt_block_label339_0:                             ; preds = %logic.end.346
  %17 = load ptr, ptr %info.376, align 8
  %18 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %17, i32 0, i32 2
  %19 = load i64, ptr %18, align 8
  call void @gala_104923._core_core_posix.fmt_uint_at(ptr %b, ptr %data, i64 %19)
  br label %end_label339

alt_cond_label339_1:                              ; preds = %logic.end.346
  %20 = load i64, ptr %k.377, align 8
  %21 = icmp eq i64 %20, 3
  br i1 %21, label %alt_block_label339_1, label %alt_cond_label339_2

alt_block_label339_1:                             ; preds = %alt_cond_label339_1
  %22 = load ptr, ptr %info.376, align 8
  %23 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %22, i32 0, i32 2
  %24 = load i64, ptr %23, align 8
  call void @gala_104923._core_core_posix.fmt_float_at(ptr %b, ptr %data, i64 %24)
  br label %end_label339

alt_cond_label339_2:                              ; preds = %alt_cond_label339_1
  %25 = load i64, ptr %k.377, align 8
  %26 = icmp eq i64 %25, 4
  br i1 %26, label %alt_block_label339_2, label %alt_cond_label339_3

alt_block_label339_2:                             ; preds = %alt_cond_label339_2
  %27 = bitcast ptr %data to ptr
  store ptr %27, ptr %bool_p.378, align 8
  %28 = load ptr, ptr %bool_p.378, align 8
  %29 = load i1, ptr %28, align 1
  br i1 %29, label %base_block_label368, label %else_block_label368

base_block_label368:                              ; preds = %alt_block_label339_2
  %30 = getelementptr inbounds [5 x i8], ptr @string16, i64 0, i64 0
  %31 = insertvalue { ptr, i64 } undef, ptr %30, 0
  %32 = insertvalue { ptr, i64 } %31, i64 4, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %32)
  br label %end_label368

else_block_label368:                              ; preds = %alt_block_label339_2
  %33 = getelementptr inbounds [6 x i8], ptr @string17, i64 0, i64 0
  %34 = insertvalue { ptr, i64 } undef, ptr %33, 0
  %35 = insertvalue { ptr, i64 } %34, i64 5, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %35)
  br label %end_label368

end_label368:                                     ; preds = %else_block_label368, %base_block_label368
  br label %end_label339

alt_cond_label339_3:                              ; preds = %alt_cond_label339_2
  %36 = load i64, ptr %k.377, align 8
  %37 = icmp eq i64 %36, 7
  br i1 %37, label %alt_block_label339_3, label %alt_cond_label339_4

alt_block_label339_3:                             ; preds = %alt_cond_label339_3
  %38 = bitcast ptr %data to ptr
  store ptr %38, ptr %str_p.379, align 8
  br i1 %nested, label %base_block_label380, label %end_label380

base_block_label380:                              ; preds = %alt_block_label339_3
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 34)
  br label %end_label380

end_label380:                                     ; preds = %base_block_label380, %alt_block_label339_3
  %39 = load ptr, ptr %str_p.379, align 8
  %40 = load { ptr, i64 }, ptr %39, align 8
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %40)
  br i1 %nested, label %base_block_label383, label %end_label383

base_block_label383:                              ; preds = %end_label380
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 34)
  br label %end_label383

end_label383:                                     ; preds = %base_block_label383, %end_label380
  br label %end_label339

alt_cond_label339_4:                              ; preds = %alt_cond_label339_3
  %41 = load i64, ptr %k.377, align 8
  %42 = icmp eq i64 %41, 8
  br i1 %42, label %alt_block_label339_4, label %alt_cond_label339_5

alt_block_label339_4:                             ; preds = %alt_cond_label339_4
  %43 = bitcast ptr %data to ptr
  store ptr %43, ptr %addr_p.380, align 8
  %44 = load ptr, ptr %addr_p.380, align 8
  %45 = load i64, ptr %44, align 8
  %46 = icmp eq i64 %45, 0
  br i1 %46, label %base_block_label387, label %else_block_label387

base_block_label387:                              ; preds = %alt_block_label339_4
  %47 = getelementptr inbounds [4 x i8], ptr @string18, i64 0, i64 0
  %48 = insertvalue { ptr, i64 } undef, ptr %47, 0
  %49 = insertvalue { ptr, i64 } %48, i64 3, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %49)
  br label %end_label387

else_block_label387:                              ; preds = %alt_block_label339_4
  %50 = load ptr, ptr %addr_p.380, align 8
  %51 = load i64, ptr %50, align 8
  call void @gala_104923._core_core_posix.fmt_hex(ptr %b, i64 %51)
  br label %end_label387

end_label387:                                     ; preds = %else_block_label387, %base_block_label387
  br label %end_label339

alt_cond_label339_5:                              ; preds = %alt_cond_label339_4
  %52 = load i64, ptr %k.377, align 8
  %53 = icmp eq i64 %52, 14
  br i1 %53, label %alt_block_label339_5, label %alt_cond_label339_6

alt_block_label339_5:                             ; preds = %alt_cond_label339_5
  %54 = bitcast ptr %data to ptr
  store ptr %54, ptr %any_p.381, align 8
  %55 = load ptr, ptr %any_p.381, align 8
  %56 = getelementptr inbounds { ptr, i64 }, ptr %55, i32 0, i32 0
  %57 = load ptr, ptr %56, align 8
  %58 = load ptr, ptr %any_p.381, align 8
  %59 = getelementptr inbounds { ptr, i64 }, ptr %58, i32 0, i32 1
  %60 = load i64, ptr %59, align 8
  call void @gala_104923._core_core_posix.fmt_value(ptr %b, ptr %57, i64 %60, i1 zeroext %nested)
  br label %end_label339

alt_cond_label339_6:                              ; preds = %alt_cond_label339_5
  %61 = load i64, ptr %k.377, align 8
  %62 = icmp eq i64 %61, 11
  br i1 %62, label %alt_block_label339_6, label %alt_cond_label339_7

alt_block_label339_6:                             ; preds = %alt_cond_label339_6
  %63 = load ptr, ptr %info.376, align 8
  call void @gala_104923._core_core_posix.fmt_struct_at(ptr %b, ptr %data, ptr %63)
  br label %end_label339

alt_cond_label339_7:                              ; preds = %alt_cond_label339_6
  %64 = load i64, ptr %k.377, align 8
  %65 = icmp eq i64 %64, 10
  store i1 %65, ptr %slot408, align 1
  br i1 %65, label %logic.end.409, label %logic.rhs.409

logic.rhs.409:                                    ; preds = %alt_cond_label339_7
  %66 = load i64, ptr %k.377, align 8
  %67 = icmp eq i64 %66, 9
  store i1 %67, ptr %slot408, align 1
  br label %logic.end.409

logic.end.409:                                    ; preds = %logic.rhs.409, %alt_cond_label339_7
  %68 = load i1, ptr %slot408, align 1
  br i1 %68, label %alt_block_label339_7, label %else_block_label339

alt_block_label339_7:                             ; preds = %logic.end.409
  %69 = load ptr, ptr %info.376, align 8
  call void @gala_104923._core_core_posix.fmt_list_at(ptr %b, ptr %data, ptr %69)
  br label %end_label339

else_block_label339:                              ; preds = %logic.end.409
  %70 = load ptr, ptr %info.376, align 8
  %71 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %70, i32 0, i32 0
  %72 = load { ptr, i64 }, ptr %71, align 8
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %72)
  br label %end_label339

end_label339:                                     ; preds = %else_block_label339, %alt_block_label339_7, %alt_block_label339_6, %alt_block_label339_5, %end_label387, %end_label383, %end_label368, %alt_block_label339_1, %alt_block_label339_0, %base_block_label339
  ret void
}

define void @gala_104923._core_core_posix.fmt_list_at(ptr %b, ptr %data, ptr %info) {
entry:
  %elems.366 = alloca ptr, align 8
  %n.367 = alloca i64, align 8
  %slice_ptr.368 = alloca ptr, align 8
  %slice_count.369 = alloca ptr, align 8
  %elem_size.370 = alloca i64, align 8
  %i.371 = alloca i64, align 8
  store ptr %data, ptr %elems.366, align 8
  %0 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 4
  %1 = load i64, ptr %0, align 8
  store i64 %1, ptr %n.367, align 8
  %2 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 1
  %3 = load i64, ptr %2, align 8
  %4 = icmp eq i64 %3, 9
  br i1 %4, label %base_block_label300, label %end_label300

base_block_label300:                              ; preds = %entry
  %5 = bitcast ptr %data to ptr
  store ptr %5, ptr %slice_ptr.368, align 8
  %6 = call ptr @gala_104923._core_core_posix.ptr_add(ptr %data, i64 8)
  %7 = bitcast ptr %6 to ptr
  store ptr %7, ptr %slice_count.369, align 8
  %8 = load ptr, ptr %slice_ptr.368, align 8
  %9 = load ptr, ptr %8, align 8
  store ptr %9, ptr %elems.366, align 8
  %10 = load ptr, ptr %slice_count.369, align 8
  %11 = load i64, ptr %10, align 8
  store i64 %11, ptr %n.367, align 8
  br label %end_label300

end_label300:                                     ; preds = %base_block_label300, %entry
  %12 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 3
  %13 = load i64, ptr %12, align 8
  %14 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr @__gala_types, i64 %13
  %15 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %14, i32 0, i32 2
  %16 = load i64, ptr %15, align 8
  store i64 %16, ptr %elem_size.370, align 8
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 91)
  store i64 0, ptr %i.371, align 8
  br label %while_cond_label316

while_cond_label316:                              ; preds = %end_label320, %end_label300
  %17 = load i64, ptr %i.371, align 8
  %18 = load i64, ptr %n.367, align 8
  %19 = icmp slt i64 %17, %18
  br i1 %19, label %while_body_label316, label %while_end_label316

while_body_label316:                              ; preds = %while_cond_label316
  %20 = load i64, ptr %i.371, align 8
  %21 = icmp sgt i64 %20, 0
  br i1 %21, label %base_block_label320, label %end_label320

base_block_label320:                              ; preds = %while_body_label316
  %22 = getelementptr inbounds [3 x i8], ptr @string13, i64 0, i64 0
  %23 = insertvalue { ptr, i64 } undef, ptr %22, 0
  %24 = insertvalue { ptr, i64 } %23, i64 2, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %24)
  br label %end_label320

end_label320:                                     ; preds = %base_block_label320, %while_body_label316
  %25 = load ptr, ptr %elems.366, align 8
  %26 = load i64, ptr %i.371, align 8
  %27 = load i64, ptr %elem_size.370, align 8
  %28 = mul i64 %26, %27
  %29 = call ptr @gala_104923._core_core_posix.ptr_add(ptr %25, i64 %28)
  %30 = getelementptr inbounds %gala_151743._core_cfunctions_posix.TypeInfo, ptr %info, i32 0, i32 3
  %31 = load i64, ptr %30, align 8
  call void @gala_104923._core_core_posix.fmt_value(ptr %b, ptr %29, i64 %31, i1 zeroext true)
  %32 = load i64, ptr %i.371, align 8
  %33 = add i64 %32, 1
  store i64 %33, ptr %i.371, align 8
  br label %while_cond_label316

while_end_label316:                               ; preds = %while_cond_label316
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 93)
  ret void
}

define void @gala_104923._core_core_posix.fmt_any(ptr %b, { ptr, i64 } %v) {
entry:
  %0 = extractvalue { ptr, i64 } %v, 0
  %1 = extractvalue { ptr, i64 } %v, 1
  call void @gala_104923._core_core_posix.fmt_value(ptr %b, ptr %0, i64 %1, i1 zeroext false)
  ret void
}

define zeroext i1 @gala_104923._core_core_posix.next_is({ ptr, i64 } %s, i64 %i, i8 zeroext %c) {
entry:
  %slot421 = alloca i1, align 1
  %0 = add i64 %i, 1
  %1 = extractvalue { ptr, i64 } %s, 1
  %2 = icmp slt i64 %0, %1
  store i1 %2, ptr %slot421, align 1
  br i1 %2, label %logic.rhs.422, label %logic.end.422

logic.rhs.422:                                    ; preds = %entry
  %3 = extractvalue { ptr, i64 } %s, 0
  %4 = add i64 %i, 1
  %5 = getelementptr inbounds i8, ptr %3, i64 %4
  %6 = load i8, ptr %5, align 1
  %7 = icmp eq i8 %6, %c
  store i1 %7, ptr %slot421, align 1
  br label %logic.end.422

logic.end.422:                                    ; preds = %logic.rhs.422, %entry
  %8 = load i1, ptr %slot421, align 1
  ret i1 %8
}

define zeroext i1 @gala_104923._core_core_posix.has_placeholder({ ptr, i64 } %s) {
entry:
  %size.388 = alloca i64, align 8
  %i.389 = alloca i64, align 8
  %c.390 = alloca i8, align 1
  %slot442 = alloca i1, align 1
  %slot450 = alloca i1, align 1
  %slot452 = alloca i1, align 1
  %0 = extractvalue { ptr, i64 } %s, 1
  store i64 %0, ptr %size.388, align 8
  store i64 0, ptr %i.389, align 8
  br label %while_cond_label433

while_cond_label433:                              ; preds = %end_label449, %entry
  %1 = load i64, ptr %i.389, align 8
  %2 = load i64, ptr %size.388, align 8
  %3 = icmp slt i64 %1, %2
  br i1 %3, label %while_body_label433, label %while_end_label433

while_body_label433:                              ; preds = %while_cond_label433
  %4 = extractvalue { ptr, i64 } %s, 0
  %5 = load i64, ptr %i.389, align 8
  %6 = getelementptr inbounds i8, ptr %4, i64 %5
  %7 = load i8, ptr %6, align 1
  store i8 %7, ptr %c.390, align 1
  %8 = load i8, ptr %c.390, align 1
  %9 = icmp eq i8 %8, 123
  store i1 %9, ptr %slot442, align 1
  br i1 %9, label %logic.rhs.443, label %logic.end.443

logic.rhs.443:                                    ; preds = %while_body_label433
  %10 = load i64, ptr %i.389, align 8
  %11 = call zeroext i1 @gala_104923._core_core_posix.next_is({ ptr, i64 } %s, i64 %10, i8 zeroext 125)
  store i1 %11, ptr %slot442, align 1
  br label %logic.end.443

logic.end.443:                                    ; preds = %logic.rhs.443, %while_body_label433
  %12 = load i1, ptr %slot442, align 1
  br i1 %12, label %base_block_label441, label %end_label441

base_block_label441:                              ; preds = %logic.end.443
  ret i1 true

end_label441:                                     ; preds = %logic.end.443
  %13 = load i8, ptr %c.390, align 1
  %14 = icmp eq i8 %13, 123
  store i1 %14, ptr %slot452, align 1
  br i1 %14, label %logic.end.453, label %logic.rhs.453

logic.rhs.453:                                    ; preds = %end_label441
  %15 = load i8, ptr %c.390, align 1
  %16 = icmp eq i8 %15, 125
  store i1 %16, ptr %slot452, align 1
  br label %logic.end.453

logic.end.453:                                    ; preds = %logic.rhs.453, %end_label441
  %17 = load i1, ptr %slot452, align 1
  store i1 %17, ptr %slot450, align 1
  br i1 %17, label %logic.rhs.451, label %logic.end.451

logic.rhs.451:                                    ; preds = %logic.end.453
  %18 = load i64, ptr %i.389, align 8
  %19 = load i8, ptr %c.390, align 1
  %20 = call zeroext i1 @gala_104923._core_core_posix.next_is({ ptr, i64 } %s, i64 %18, i8 zeroext %19)
  store i1 %20, ptr %slot450, align 1
  br label %logic.end.451

logic.end.451:                                    ; preds = %logic.rhs.451, %logic.end.453
  %21 = load i1, ptr %slot450, align 1
  br i1 %21, label %base_block_label449, label %else_block_label449

base_block_label449:                              ; preds = %logic.end.451
  %22 = load i64, ptr %i.389, align 8
  %23 = add i64 %22, 2
  store i64 %23, ptr %i.389, align 8
  br label %end_label449

else_block_label449:                              ; preds = %logic.end.451
  %24 = load i64, ptr %i.389, align 8
  %25 = add i64 %24, 1
  store i64 %25, ptr %i.389, align 8
  br label %end_label449

end_label449:                                     ; preds = %else_block_label449, %base_block_label449
  br label %while_cond_label433

while_end_label433:                               ; preds = %while_cond_label433
  ret i1 false
}

define void @gala_104923._core_core_posix.format_into(ptr %b, { ptr, i64 } %fmt_str, { ptr, i64 } %args) {
entry:
  %size.394 = alloca i64, align 8
  %i.395 = alloca i64, align 8
  %arg_index.396 = alloca i64, align 8
  %c.397 = alloca i8, align 1
  %slot477 = alloca i1, align 1
  %slot499 = alloca i1, align 1
  %slot501 = alloca i1, align 1
  %0 = extractvalue { ptr, i64 } %fmt_str, 1
  store i64 %0, ptr %size.394, align 8
  store i64 0, ptr %i.395, align 8
  store i64 0, ptr %arg_index.396, align 8
  br label %while_cond_label468

while_cond_label468:                              ; preds = %end_label476, %entry
  %1 = load i64, ptr %i.395, align 8
  %2 = load i64, ptr %size.394, align 8
  %3 = icmp slt i64 %1, %2
  br i1 %3, label %while_body_label468, label %while_end_label468

while_body_label468:                              ; preds = %while_cond_label468
  %4 = extractvalue { ptr, i64 } %fmt_str, 0
  %5 = load i64, ptr %i.395, align 8
  %6 = getelementptr inbounds i8, ptr %4, i64 %5
  %7 = load i8, ptr %6, align 1
  store i8 %7, ptr %c.397, align 1
  %8 = load i8, ptr %c.397, align 1
  %9 = icmp eq i8 %8, 123
  store i1 %9, ptr %slot477, align 1
  br i1 %9, label %logic.rhs.478, label %logic.end.478

logic.rhs.478:                                    ; preds = %while_body_label468
  %10 = load i64, ptr %i.395, align 8
  %11 = call zeroext i1 @gala_104923._core_core_posix.next_is({ ptr, i64 } %fmt_str, i64 %10, i8 zeroext 125)
  store i1 %11, ptr %slot477, align 1
  br label %logic.end.478

logic.end.478:                                    ; preds = %logic.rhs.478, %while_body_label468
  %12 = load i1, ptr %slot477, align 1
  br i1 %12, label %base_block_label476, label %alt_cond_label476_0

base_block_label476:                              ; preds = %logic.end.478
  %13 = load i64, ptr %arg_index.396, align 8
  %14 = extractvalue { ptr, i64 } %args, 1
  %15 = icmp slt i64 %13, %14
  br i1 %15, label %base_block_label484, label %else_block_label484

base_block_label484:                              ; preds = %base_block_label476
  %16 = extractvalue { ptr, i64 } %args, 0
  %17 = load i64, ptr %arg_index.396, align 8
  %18 = getelementptr inbounds { ptr, i64 }, ptr %16, i64 %17
  %19 = load { ptr, i64 }, ptr %18, align 8
  call void @gala_104923._core_core_posix.fmt_any(ptr %b, { ptr, i64 } %19)
  br label %end_label484

else_block_label484:                              ; preds = %base_block_label476
  %20 = getelementptr inbounds [10 x i8], ptr @string19, i64 0, i64 0
  %21 = insertvalue { ptr, i64 } undef, ptr %20, 0
  %22 = insertvalue { ptr, i64 } %21, i64 9, 1
  call void @gala_104923._core_core_posix.builder_push_string(ptr %b, { ptr, i64 } %22)
  br label %end_label484

end_label484:                                     ; preds = %else_block_label484, %base_block_label484
  %23 = load i64, ptr %arg_index.396, align 8
  %24 = add i64 %23, 1
  store i64 %24, ptr %arg_index.396, align 8
  %25 = load i64, ptr %i.395, align 8
  %26 = add i64 %25, 2
  store i64 %26, ptr %i.395, align 8
  br label %end_label476

alt_cond_label476_0:                              ; preds = %logic.end.478
  %27 = load i8, ptr %c.397, align 1
  %28 = icmp eq i8 %27, 123
  store i1 %28, ptr %slot501, align 1
  br i1 %28, label %logic.end.502, label %logic.rhs.502

logic.rhs.502:                                    ; preds = %alt_cond_label476_0
  %29 = load i8, ptr %c.397, align 1
  %30 = icmp eq i8 %29, 125
  store i1 %30, ptr %slot501, align 1
  br label %logic.end.502

logic.end.502:                                    ; preds = %logic.rhs.502, %alt_cond_label476_0
  %31 = load i1, ptr %slot501, align 1
  store i1 %31, ptr %slot499, align 1
  br i1 %31, label %logic.rhs.500, label %logic.end.500

logic.rhs.500:                                    ; preds = %logic.end.502
  %32 = load i64, ptr %i.395, align 8
  %33 = load i8, ptr %c.397, align 1
  %34 = call zeroext i1 @gala_104923._core_core_posix.next_is({ ptr, i64 } %fmt_str, i64 %32, i8 zeroext %33)
  store i1 %34, ptr %slot499, align 1
  br label %logic.end.500

logic.end.500:                                    ; preds = %logic.rhs.500, %logic.end.502
  %35 = load i1, ptr %slot499, align 1
  br i1 %35, label %alt_block_label476_0, label %else_block_label476

alt_block_label476_0:                             ; preds = %logic.end.500
  %36 = load i8, ptr %c.397, align 1
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %36)
  %37 = load i64, ptr %i.395, align 8
  %38 = add i64 %37, 2
  store i64 %38, ptr %i.395, align 8
  br label %end_label476

else_block_label476:                              ; preds = %logic.end.500
  %39 = load i8, ptr %c.397, align 1
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext %39)
  %40 = load i64, ptr %i.395, align 8
  %41 = add i64 %40, 1
  store i64 %41, ptr %i.395, align 8
  br label %end_label476

end_label476:                                     ; preds = %else_block_label476, %alt_block_label476_0, %end_label484
  br label %while_cond_label468

while_end_label468:                               ; preds = %while_cond_label468
  ret void
}

define ptr @gala_104923._core_core_posix.leading_format({ ptr, i64 } %data) {
entry:
  %first.399 = alloca ptr, align 8
  %slot527 = alloca i1, align 1
  %0 = extractvalue { ptr, i64 } %data, 1
  %1 = icmp eq i64 %0, 0
  br i1 %1, label %base_block_label518, label %end_label518

base_block_label518:                              ; preds = %entry
  %2 = inttoptr i64 0 to ptr
  ret ptr %2

end_label518:                                     ; preds = %entry
  %3 = extractvalue { ptr, i64 } %data, 0
  %4 = getelementptr inbounds { ptr, i64 }, ptr %3, i64 0
  %5 = load { ptr, i64 }, ptr %4, align 8
  %6 = call ptr @gala_104923._core_core_posix.any_as.any.string.v0({ ptr, i64 } %5)
  store ptr %6, ptr %first.399, align 8
  %7 = load ptr, ptr %first.399, align 8
  %8 = ptrtoint ptr %7 to i64
  %9 = inttoptr i64 0 to ptr
  %10 = ptrtoint ptr %9 to i64
  %11 = icmp ne i64 %8, %10
  store i1 %11, ptr %slot527, align 1
  br i1 %11, label %logic.rhs.528, label %logic.end.528

logic.rhs.528:                                    ; preds = %end_label518
  %12 = load ptr, ptr %first.399, align 8
  %13 = load { ptr, i64 }, ptr %12, align 8
  %14 = call zeroext i1 @gala_104923._core_core_posix.has_placeholder({ ptr, i64 } %13)
  store i1 %14, ptr %slot527, align 1
  br label %logic.end.528

logic.end.528:                                    ; preds = %logic.rhs.528, %end_label518
  %15 = load i1, ptr %slot527, align 1
  br i1 %15, label %base_block_label526, label %end_label526

base_block_label526:                              ; preds = %logic.end.528
  %16 = load ptr, ptr %first.399, align 8
  ret ptr %16

end_label526:                                     ; preds = %logic.end.528
  %17 = inttoptr i64 0 to ptr
  ret ptr %17
}

define ptr @gala_104923._core_core_posix.any_as.any.string.v0({ ptr, i64 } %v) {
entry:
  %0 = extractvalue { ptr, i64 } %v, 1
  %1 = icmp eq i64 %0, 17
  br i1 %1, label %base_block_label587, label %end_label587

base_block_label587:                              ; preds = %entry
  %2 = extractvalue { ptr, i64 } %v, 0
  %3 = bitcast ptr %2 to ptr
  ret ptr %3

end_label587:                                     ; preds = %entry
  %4 = inttoptr i64 0 to ptr
  ret ptr %4
}

define void @gala_104923._core_core_posix.print_into(ptr %b, { ptr, i64 } %data, i1 zeroext %newline) {
entry:
  %fmt_str.403 = alloca ptr, align 8
  %i.404 = alloca i64, align 8
  %__for_iter1.445 = alloca { ptr, i64 }, align 8
  %__for_idx1.446 = alloca i64, align 8
  %v.405 = alloca { ptr, i64 }, align 8
  %0 = call ptr @gala_104923._core_core_posix.leading_format({ ptr, i64 } %data)
  store ptr %0, ptr %fmt_str.403, align 8
  %1 = load ptr, ptr %fmt_str.403, align 8
  %2 = ptrtoint ptr %1 to i64
  %3 = inttoptr i64 0 to ptr
  %4 = ptrtoint ptr %3 to i64
  %5 = icmp ne i64 %2, %4
  br i1 %5, label %base_block_label541, label %else_block_label541

base_block_label541:                              ; preds = %entry
  %6 = load ptr, ptr %fmt_str.403, align 8
  %7 = load { ptr, i64 }, ptr %6, align 8
  %8 = extractvalue { ptr, i64 } %data, 1
  %9 = sub nuw nsw i64 %8, 1
  %10 = extractvalue { ptr, i64 } %data, 0
  %11 = getelementptr inbounds { ptr, i64 }, ptr %10, i64 1
  %12 = insertvalue { ptr, i64 } undef, ptr %11, 0
  %13 = insertvalue { ptr, i64 } %12, i64 %9, 1
  call void @gala_104923._core_core_posix.format_into(ptr %b, { ptr, i64 } %7, { ptr, i64 } %13)
  br label %end_label541

else_block_label541:                              ; preds = %entry
  store i64 0, ptr %i.404, align 8
  store { ptr, i64 } %data, ptr %__for_iter1.445, align 8
  store i64 0, ptr %__for_idx1.446, align 8
  br label %while_cond_label555

while_cond_label555:                              ; preds = %end_label567, %else_block_label541
  %14 = load i64, ptr %__for_idx1.446, align 8
  %15 = load { ptr, i64 }, ptr %__for_iter1.445, align 8
  %16 = extractvalue { ptr, i64 } %15, 1
  %17 = icmp slt i64 %14, %16
  br i1 %17, label %while_body_label555, label %while_end_label555

while_body_label555:                              ; preds = %while_cond_label555
  %18 = load { ptr, i64 }, ptr %__for_iter1.445, align 8
  %19 = extractvalue { ptr, i64 } %18, 0
  %20 = load i64, ptr %__for_idx1.446, align 8
  %21 = getelementptr inbounds { ptr, i64 }, ptr %19, i64 %20
  %22 = load { ptr, i64 }, ptr %21, align 8
  store { ptr, i64 } %22, ptr %v.405, align 8
  %23 = load i64, ptr %__for_idx1.446, align 8
  %24 = add i64 %23, 1
  store i64 %24, ptr %__for_idx1.446, align 8
  %25 = load i64, ptr %i.404, align 8
  %26 = icmp sgt i64 %25, 0
  br i1 %26, label %base_block_label567, label %end_label567

base_block_label567:                              ; preds = %while_body_label555
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 32)
  br label %end_label567

end_label567:                                     ; preds = %base_block_label567, %while_body_label555
  %27 = load { ptr, i64 }, ptr %v.405, align 8
  call void @gala_104923._core_core_posix.fmt_any(ptr %b, { ptr, i64 } %27)
  %28 = load i64, ptr %i.404, align 8
  %29 = add i64 %28, 1
  store i64 %29, ptr %i.404, align 8
  br label %while_cond_label555

while_end_label555:                               ; preds = %while_cond_label555
  br label %end_label541

end_label541:                                     ; preds = %while_end_label555, %base_block_label541
  br i1 %newline, label %base_block_label573, label %end_label573

base_block_label573:                              ; preds = %end_label541
  call void @gala_104923._core_core_posix.builder_push_byte(ptr %b, i8 zeroext 10)
  br label %end_label573

end_label573:                                     ; preds = %base_block_label573, %end_label541
  ret void
}

define zeroext i1 @gala_104923._core_core_posix.__default_0() {
entry:
  ret i1 false
}

define void @gala_104923._core_core_posix.print_to_fd(i32 %fd, { ptr, i64 } %data, i1 zeroext %newline) {
entry:
  %b.409 = alloca %gala_104923._core_core_posix.Builder, align 8
  call void @gala_104923._core_core_posix.builder_make(ptr sret(%gala_104923._core_core_posix.Builder) align 8 %b.409)
  call void @gala_104923._core_core_posix.print_into(ptr %b.409, { ptr, i64 } %data, i1 zeroext %newline)
  %0 = call i64 @gala_104923._core_core_posix.builder_flush(ptr %b.409, i32 %fd)
  ret void
}

define void @gala_104923._core_core_posix.format_to_fd(i32 %fd, { ptr, i64 } %fmt_str, { ptr, i64 } %args) {
entry:
  %b.413 = alloca %gala_104923._core_core_posix.Builder, align 8
  call void @gala_104923._core_core_posix.builder_make(ptr sret(%gala_104923._core_core_posix.Builder) align 8 %b.413)
  call void @gala_104923._core_core_posix.format_into(ptr %b.413, { ptr, i64 } %fmt_str, { ptr, i64 } %args)
  %0 = call i64 @gala_104923._core_core_posix.builder_flush(ptr %b.413, i32 %fd)
  ret void
}

define void @gala_104923._core_core_posix.print({ ptr, i64 } %data.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.print_to_fd(i32 1, { ptr, i64 } %data.gala_variadic, i1 zeroext false)
  ret void
}

define void @gala_104923._core_core_posix.println({ ptr, i64 } %data.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.print_to_fd(i32 1, { ptr, i64 } %data.gala_variadic, i1 zeroext true)
  ret void
}

define void @gala_104923._core_core_posix.printfmt({ ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.format_to_fd(i32 1, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic)
  ret void
}

define void @gala_104923._core_core_posix.eprintln({ ptr, i64 } %data.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.print_to_fd(i32 2, { ptr, i64 } %data.gala_variadic, i1 zeroext true)
  ret void
}

define void @gala_104923._core_core_posix.eprintfmt({ ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.format_to_fd(i32 2, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic)
  ret void
}

define void @gala_104923._core_core_posix.fprintln(i32 %fd, { ptr, i64 } %data.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.print_to_fd(i32 %fd, { ptr, i64 } %data.gala_variadic, i1 zeroext true)
  ret void
}

define void @gala_104923._core_core_posix.fprintfmt(i32 %fd, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.format_to_fd(i32 %fd, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic)
  ret void
}

define void @gala_104923._core_core_posix.bprintln(ptr %b, { ptr, i64 } %data.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.print_into(ptr %b, { ptr, i64 } %data.gala_variadic, i1 zeroext true)
  ret void
}

define void @gala_104923._core_core_posix.bprintfmt(ptr %b, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic) {
entry:
  call void @gala_104923._core_core_posix.format_into(ptr %b, { ptr, i64 } %fmt_str, { ptr, i64 } %args.gala_variadic)
  ret void
}

define void @gala_104923._core_core_posix.sleep_ms(i64 %ms) {
entry:
  %ts.434 = alloca %gala_104923._core_core_posix.Timespec, align 8
  %0 = icmp sle i64 %ms, 0
  br i1 %0, label %base_block_label576, label %end_label576

base_block_label576:                              ; preds = %entry
  ret void

end_label576:                                     ; preds = %entry
  %1 = sdiv i64 %ms, 1000
  %2 = srem i64 %ms, 1000
  %3 = mul i64 %2, 1000000
  %4 = insertvalue %gala_104923._core_core_posix.Timespec undef, i64 %1, 0
  %5 = insertvalue %gala_104923._core_core_posix.Timespec %4, i64 %3, 1
  store %gala_104923._core_core_posix.Timespec %5, ptr %ts.434, align 8
  br label %while_cond_label583

while_cond_label583:                              ; preds = %while_body_label583, %end_label576
  %6 = call i32 @nanosleep(ptr %ts.434, ptr %ts.434)
  %7 = sub i32 0, 1
  %8 = icmp eq i32 %6, %7
  br i1 %8, label %while_body_label583, label %while_end_label583

while_body_label583:                              ; preds = %while_cond_label583
  br label %while_cond_label583

while_end_label583:                               ; preds = %while_cond_label583
  ret void
}

declare i32 @nanosleep(ptr, ptr)

define i32 @gala_507430._main.main() {
entry:
  %slot21 = alloca [1 x { ptr, i64 }], align 8
  %slot25 = alloca { ptr, i64 }, align 8
  %slot31 = alloca [3 x { ptr, i64 }], align 8
  %slot32 = alloca i64, align 8
  %slot36 = alloca i64, align 8
  %slot40 = alloca i64, align 8
  %0 = getelementptr inbounds [18 x i8], ptr @string3, i64 0, i64 0
  %1 = insertvalue { ptr, i64 } undef, ptr %0, 0
  %2 = insertvalue { ptr, i64 } %1, i64 17, 1
  store { ptr, i64 } %2, ptr %slot25, align 8
  %3 = insertvalue { ptr, i64 } undef, ptr %slot25, 0
  %4 = insertvalue { ptr, i64 } %3, i64 17, 1
  %5 = getelementptr inbounds [1 x { ptr, i64 }], ptr %slot21, i64 0, i64 0
  store { ptr, i64 } %4, ptr %5, align 8
  %6 = insertvalue { ptr, i64 } undef, ptr %slot21, 0
  %7 = insertvalue { ptr, i64 } %6, i64 1, 1
  call void @gala_104923._core_core_posix.println({ ptr, i64 } %7)
  store i64 180, ptr %slot32, align 8
  %8 = insertvalue { ptr, i64 } undef, ptr %slot32, 0
  %9 = insertvalue { ptr, i64 } %8, i64 3, 1
  %10 = getelementptr inbounds [3 x { ptr, i64 }], ptr %slot31, i64 0, i64 0
  store { ptr, i64 } %9, ptr %10, align 8
  store i64 181, ptr %slot36, align 8
  %11 = insertvalue { ptr, i64 } undef, ptr %slot36, 0
  %12 = insertvalue { ptr, i64 } %11, i64 3, 1
  %13 = getelementptr inbounds [3 x { ptr, i64 }], ptr %slot31, i64 0, i64 1
  store { ptr, i64 } %12, ptr %13, align 8
  store i64 109, ptr %slot40, align 8
  %14 = insertvalue { ptr, i64 } undef, ptr %slot40, 0
  %15 = insertvalue { ptr, i64 } %14, i64 3, 1
  %16 = getelementptr inbounds [3 x { ptr, i64 }], ptr %slot31, i64 0, i64 2
  store { ptr, i64 } %15, ptr %16, align 8
  %17 = insertvalue { ptr, i64 } undef, ptr %slot31, 0
  %18 = insertvalue { ptr, i64 } %17, i64 3, 1
  call void @gala_104923._core_core_posix.println({ ptr, i64 } %18)
  ret i32 0
}

define i32 @f({ i64, i64 } %x.abi, { i64, i64 } %y.abi) {
entry:
  %0 = alloca %gala_104923._core_core_posix.Timespec, align 8
  store { i64, i64 } %x.abi, ptr %0, align 8
  %1 = load %gala_104923._core_core_posix.Timespec, ptr %0, align 8
  %2 = alloca %gala_104923._core_core_posix.Timespec, align 8
  store { i64, i64 } %y.abi, ptr %2, align 8
  %3 = load %gala_104923._core_core_posix.Timespec, ptr %2, align 8
  ret i32 0
}

define i32 @main(i32 %argc, ptr %argv) {
  call void @gala_151743._core_cfunctions_posix.__init_globals()
  %result = call i32 @gala_507430._main.main()
  ret i32 %result
}

define void @gala_151743._core_cfunctions_posix.__init_globals() {
entry:
  store i32 0, ptr @gala_151743._core_cfunctions_posix.SEEK_SET, align 4
  store i32 1, ptr @gala_151743._core_cfunctions_posix.SEEK_CUR, align 4
  store i32 2, ptr @gala_151743._core_cfunctions_posix.SEEK_END, align 4
  ret void
}

attributes #0 = { nocallback nofree nounwind willreturn memory(argmem: write) }
