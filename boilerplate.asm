format ELF64

public main
extrn print_str
extern main

segment readable writeable
msg db "hello from fasm", 10
msg_len = $ - msg

segment readable executable
start:

    mov     rdi, rax
    mov     rax, 60           ; sys_exit
    syscall

