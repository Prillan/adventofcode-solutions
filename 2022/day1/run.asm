; ----------------------------------------------------------------------------------------
;       Advent of Code, 2022, Day 01
;
; Compile with
;
;     nasm -felf64 run.asm && ld -o run run.o && ./run
;
; ----------------------------------------------------------------------------------------

        %define SYS_fstat 0x05
        %define SYS_mmap 0x09
        %define SYS_open 0x02
        %define O_RDONLY 0x00
        %define MAP_POPULATE 0x08000
        %define MAP_PRIVATE 0x02
        %define PROT_READ 0x01

        global    _start

        section   .text
_start:
        mov     rax, QWORD [rsp + 8*2]
        call    open            ; fd = rax = open(argv[1])
        push    rax
        call    length          ; rax = length(fd)
        mov     rbx, rax
        pop     rax
        call    mmap            ; *char data = mmap(fd, length)
        call    top3            ; elf1, elf2, elf3 = top3(data)
        add     rbx, rcx
        add     rbx, rax
        push    rbx
        call    writeResult
        pop     rax
        call    writeResult
        mov     rax, 60                 ; system call for exit
        xor     rdi, rdi                ; exit code 0
        syscall                         ; invoke operating system to exit

        ;; Main logic.
        ;; rax - largest (elf1)
        ;; rbx - middle  (elf2)
        ;; rcx - lowest  (elf3)
        ;; rdx - current elf total (elf)
        ;; r8  - current snack total (snack)
        ;; r9  - pointer
        ;; while (c != 0)
        %define elf1  rax
        %define elf2  rbx
        %define elf3  rcx
        %define elf   rdx
        %define snack r8
        %define pos   r9
top3:   mov pos, rax
        xor elf1, elf1
        xor elf2, elf2
        xor elf3, elf3
        xor rsi, rsi
top3_new_elf:
        xor elf, elf
top3_new_snack:
        xor snack, snack
top3_snack_loop:
        mov sil, BYTE [ pos ]
        add pos, 1
        cmp si, 0x0a
        je  top3_finalize_snack
        cmp si, 0
        je  top3_finalize_snack
        sub si, '0'
        imul snack, 10
        add snack, rsi
        jmp top3_snack_loop
top3_finalize_snack:
        add elf, snack
        cmp snack, 0
        jne top3_new_snack
top3_finalize_elf:
        cmp  elf, 0
        je   top3_done
        cmp  elf, elf3
        jl   top3_new_elf
        xchg elf, elf3
        cmp  elf3, elf2
        jl   top3_new_elf
        xchg elf3, elf2
        cmp  elf2, elf1
        jl   top3_new_elf
        xchg elf2, elf1
        jmp  top3_new_elf
top3_done:
        ret

open:   mov     rdi, rax        ; const char* -- pointer to file name
        mov     rax, SYS_open   ; syscall
        mov     rsi, O_RDONLY   ; int flags
        syscall
        ret

mmap:   mov     r8, rax          ; int fd
        mov     rsi, rbx         ; size_t length
        mov     rdi, 0           ; void *addr
        mov     rdx, PROT_READ   ; int prot
        mov     r10, (MAP_PRIVATE | MAP_POPULATE) ; int flags
        xor     r9, r9           ; off_t offset
        mov     rax, SYS_mmap    ; syscall
        syscall
        ret

;;; length(int fd): size_t
length: mov     rdi, rax        ; int fd
        sub     rsp, 0x100      ; make room for statbuf
        mov     rsi, rsp        ; struct stat *statbuf
        mov     rax, SYS_fstat  ; syscall
        syscall
        mov     rax, QWORD [rsp + 8 * 6]
        add     rsp, 0x100
        ret

writeResult:
        ;; Now we have our answer in rax, write it out
        %define MAX_DIGITS 64 ; max 64 digits, pretty arbitrary
        sub     rsp, MAX_DIGITS + 1
        mov     rbx, MAX_DIGITS
        mov     BYTE [rsp + rbx], 0x0a  ; newline!
        sub     rbx, 1
        mov     rcx, 10                 ; divisor
itoaLoop:
        xor     rdx, rdx        ; clear upper part of dividend
        ;; lower part is already set to what remains of our num
        div     rcx
        ;; rax = rax / 10
        ;; rdx = rax % 10
        add     dl, '0'         ; convert to char
        mov     BYTE [rsp + rbx], dl
        sub     rbx, 1
        cmp     rbx, 0          ; if we've reached all digits, stop
        je      writeResultWrite
        cmp     rax, 0          ; if the quotient is 0, stop
        je      writeResultWrite
        jmp     itoaLoop
writeResultWrite:
        ;; Write to stdout
        mov       rax, 1                  ; system call for write
        mov       rdi, 1                  ; file handle 1 is stdout
        lea       rsi, [rsp+rbx+1]        ; address of string to output
        mov       rdx, MAX_DIGITS         ; number of bytes
        sub       rdx, rbx
        syscall                           ; invoke operating system to do the write
        add       rsp, MAX_DIGITS + 1
        ret
