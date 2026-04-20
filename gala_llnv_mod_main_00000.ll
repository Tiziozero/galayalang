target triple = "x86_64-pc-linux-gnu"
define i32 @b(i32 %f) {
entry:
%t0 = add i32 1, %f
ret i32 %t0 
}

define i32 @main() {
entry:
%a = alloca i32
store i32 0, i32* %a

%i = alloca i32
store i32 0, i32* %i

br label %loop1.cond

loop1.cond:
%t2 = load i32, i32* %i
%t3 = icmp ule i32 %t2, 3
%loop1_cond_res = icmp ne i1 %t3, 0
br i1 %loop1_cond_res, label %loop1.body, label %loop1.end

loop1.body:
%t4 = load i32, i32* %a
%t5 = add i32 %t4, 2
store i32 %t5, i32* %a
%t6 = load i32, i32* %i
%t7 = add i32 %t6, 1
store i32 %t7, i32* %i
br label %loop1.cond

loop1.end:

%t8 = load i32, i32* %a
ret i32 %t8 
}

