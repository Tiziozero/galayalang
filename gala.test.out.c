// generated using uqc, the galayalang compiler
void print();
#include <stdint.h>
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
typedef u64        usize;
// GALASTART
typedef struct {
f32 x;
f32 y;
}vec2;

typedef struct {
vec2 pos;
f32 width;
f32 height;
}rect;

typedef struct {
f32 min;
f32 max;
f32 sum;
}stats;

vec2 vec2_add(vec2 a,vec2 b){
vec2 result = {.x=(a.x+b.x), .y=(a.y+b.y)};
return result;

}
vec2 vec2_scale(vec2 v,f32 s){
vec2 result = {.x=(v.x*s), .y=(v.y*s)};
return result;

}
f32 vec2_dot(vec2 a,vec2 b){
return ((a.x*b.x)+(a.y*b.y));

}
f32 rect_area(rect r){
return (r.width*r.height);

}
void rect_translate(rect* r,vec2 offset){
(r->pos.x=(r->pos.x+offset.x));
(r->pos.y=(r->pos.y+offset.y));

}
stats compute_stats(f32 a,f32 b,f32 c){
f32 min_val = a;
f32 max_val = a;
f32 sum_val = ((a+b)+c);
if ((b<min_val)) {
(min_val=b);

}

if ((c<min_val)) {
(min_val=c);

}

if ((b>max_val)) {
(max_val=b);

}

if ((c>max_val)) {
(max_val=c);

}

stats result = {.min=min_val, .max=max_val, .sum=sum_val};
return result;

}
void mutate_vec(vec2* v){
(v->x=(v->x*2));
(v->y=(v->y*3));

}
vec2 make_unit_x(){
vec2 a = {.x=1.0, .y=0.0};
return a;

}
// GALAEND
#include <unistd.h> // for syscall numbers (optional, can use numbers directly)
void print_string(const char *s) {
// Linux x86_64 syscall: write(fd=1, buf=s, count=len)
const char *p = s;
long len = 0;

// Compute string length manually (no strlen)
while (p[len] != '\0') {
len++;
}

// syscall: write(1, s, len)
asm volatile(
"movq $1,  %%rax  \n"  // syscall number 1 = sys_write
"movq $1,  %%rdi  \n"  // fd = 1 (stdout)
"movq %0, %%rsi \n"  // buffer
"movq %1, %%rdx \n"  // length
"syscall"
:
: "r"(s), "r"(len)
: "rax", "rdi", "rsi", "rdx"
);
}

