// generated using uqc, the galayalang compiler
#include <stdint.h>
typedef uint8_t    u8;
typedef uint16_t   u16;
typedef uint32_t   u32;
typedef uint64_t   u64;
typedef int8_t     i8;
typedef int16_t    i16;
typedef int32_t    i32;
typedef int64_t    i64;
#include <stdio.h>
u32 i = (0);
u32 j;
u32 fib(u32 a){
if (((a)<=(1))) {
return (a);
}

return ((fib(((a)-(1))))+(fib(((a)-(2)))));

}
void _void_ret(){

}
i32 main(){
return (fib((6)));

}
