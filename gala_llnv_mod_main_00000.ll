target triple = "x86_64-pc-linux-gnu"
define i32 @b() {
entry:
ret i32 2 
}

define i32 @main() {
entry:
%k = alloca i32
%t0 = call i32 @b()
store i32 %t0, i32* %k

%a = alloca i32
%t1 = load i32, i32* %k
%t2 = add i32 4, %t1
store i32 %t2, i32* %a

%t3 = load i32, i32* %a
%t4 = add i32 %t3, 1
store i32 %t4, i32* %a
%t5 = add i32 1, 2
%t6 = add i32 %t5, 7
%t7 = load i32, i32* %a
%t8 = add i32 %t6, %t7
ret i32 %t8 
}

