; ----------------------------------------------------------------------------------------
;       Advent of Code, 2020, Day XX
;
; Compile with
;
;     nasm -felf64 run.asm && ld -o run run.o && ./run
;
; ----------------------------------------------------------------------------------------

        %define MAX_INPUT_BYTES 8192
        %define MAX_NUMBERS 4096

        global    _start

        section   .text
_start:
        call    readNums
        call    solve1
        call    writeResult
        call    solve2
        call    writeResult
        mov     rax, 60                 ; system call for exit
        xor     rdi, rdi                ; exit code 0
        syscall                         ; invoke operating system to exit

parse:  ret

solve1: mov rax, 1
        ret

solve2: mov rax, 2
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

        section .bss

buffer:         resb MAX_INPUT_BYTES   ; We can't handle larger input than this
numbers:        resd MAX_NUMBERS       ; Store at most this many numbers
