format ELF64 executable
entry _start

segment readable executable

; Your compiler should emit a symbol called 'main'
extrn main

_start:
    ; argc = [rsp], argv = rsp+8
    mov     rdi, [rsp]        ; argc
    lea     rsi, [rsp + 8]    ; argv

    and     rsp, -16          ; stack align
    call    main              ; call user main(argc, argv)

    ; exit(main_return)
    mov     rdi, rax
    mov     rax, 60           ; sys_exit
    syscall

; print_str(ptr, len)
public print_str
print_str:
    mov     rdx, rsi          ; len
    mov     rsi, rdi          ; buffer
    mov     rdi, 1            ; stdout
    mov     rax, 1            ; write syscall
    syscall
    ret
