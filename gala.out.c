// generated using uqc, the galayalang compiler
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
typedef uint8_t    u8;
typedef uint16_t   u16;
typedef uint32_t   u32;
typedef uint64_t   u64;
typedef int8_t     i8;
typedef int16_t    i16;
typedef int32_t    i32;
typedef int64_t    i64;
typedef float      f32;
typedef double     f64;
typedef size_t     usize;
#include <stdio.h>
typedef struct {
f32 x;
f32 y;
}vec;

void doer_of_things(vec* p){
f32 k = 25;
((*p).x=(1*k));
((*p).y=(8*k));

}
i32 main(){
vec p = {.x=1, .y=2};
(p.x=3);
(p.y=2);
doer_of_things((&p));

return (i32)(p.x+p.y);

}
