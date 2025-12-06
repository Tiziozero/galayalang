format ELF64 executable
entry start

segment readable executable
macro write fd, msg, len {
    mov rax, 1
    mov rdi, fd
    mov rsi, msg
    mov rdx, len
    syscall
}
start:
  write 1,msg, 16
  push 8
  pop rsp

  mov rdi, rbx
  mov rax, 60
  syscall

segment readable writable
msg db "Hello, World! 1", 10
