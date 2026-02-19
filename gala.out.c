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
void print();
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
char* c = "Hello, World!!";
print();

vec p = {.x=1, .y=2};
(p.x=3);
(p.y=2);

doer_of_things((&p));

return (i32)(p.x+p.y);

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

void _start() {
print_string("Hello from Linux syscall!\n");

main(); // call main
// exit(0) without libc
asm volatile(
"movq $60, %%rax \n" // syscall number 60 = exit
"xor %%rdi, %%rdi \n" // exit code 0
"syscall"
:
:
: "rax", "rdi"
);
}
void print() { print_string("Print Function called.!!!\n"); }