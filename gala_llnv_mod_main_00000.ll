target triple = "x86_64-pc-linux-gnu"
define i32 @b(i32 %f) {
entry:
%t0 = add i32 1, %f
ret i32 %t0 
}

define i32 @main() {
entry:
%a = alloca i32
store i32 4, i32* %a

%t1 = load i32, i32* %a
%t2 = add i32 %t1, 1
%t3 = call i32 @b(i32 %t2)
store i32 %t3, i32* %a
%t4 = load i32, i32* %a
%t5 = load i32, i32* %a
%t6 = mul i32 %t4, %t5
ret i32 %t6 
}

