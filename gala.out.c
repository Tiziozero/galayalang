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
u32 k = (11);
i32 dooer_of_things(u32* a){
return ((*(a))+(5));

}
i32 main(){
u32 a = (11);
u32* b = (&(a));
return (dooer_of_things((b)));

}
