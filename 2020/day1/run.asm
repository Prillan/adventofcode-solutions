; ----------------------------------------------------------------------------------------
;       Advent of Code, 2020, Day 1
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
        push    rax
        call    solve1
        call    writeResult
        pop     rax
        call    solve2
        call    writeResult
        mov     rax, 60                 ; system call for exit
        xor     rdi, rdi                ; exit code 0
        syscall                         ; invoke operating system to exit

readNums:
        mov       rax, 0                ; read
        mov       rdi, 0                ; stdin
        lea       rsi, [buffer]         ; to input buffer
        mov       rdx, MAX_INPUT_BYTES  ; read this many chars
        syscall
        mov       r9, rax       ; bytes read from stdin
        xor       r10, r10      ; current buffer offset
        xor       rcx, rcx      ; nums read
readNumsLoop:
        push    rcx
        call    readNum
        pop     rcx
        cmp     rax, -1
        je      readNumsDone
        mov     [numbers+4*rcx], eax
        add     rcx, 1
        jmp     readNumsLoop
readNumsDone:
        mov     rax, rcx
        ret

readNum:
        ;; Reads a natural number from stdin, and returns it in rax.
        ;; -1 indicates eof.
        ;; We keep the number of bytes read in r10. Not the best idea,
        ;; *shrug*.
        cmp       r10, r9
        je        readNumEof
        mov       rbx, 0
        mov       rax, 0
        xor       rdx, rdx
        mov       r11, 10       ; just the number 10
        xor       r12, r12      ; did we read anything?
readNumLoop:
        cmp       r10, r9       ; all bytes, return num
        je        readNumRetNum
        mov       cl, [buffer+r10]
        sub       cl, '0'       ; if not ..
        js        readNumRetNum
        cmp       cl, 10        ; .. a number, return num
        jae       readNumRetNum
        mul       r11
        mov       r12b, 1
        add       rax, rcx
        add       r10, 1
        jmp       readNumLoop
        ret
readNumEof:
        mov     rax, -1
        ret
readNumRetNum:
        add     r10, 1
        cmp     r12, 0          ; If we didn't read anything
        je      readNumEof      ; ... return -1
        ret

solve1: mov r9, rax             ; number of numbers
        %define i eax
        %define iv ebx
        %define j ecx
        %define jv edx
        mov i, 0
solve1outer:
        mov iv, [numbers + i*4]
        mov j, i
        add j, 1
solve1inner:
        mov edi, iv
        mov jv, [numbers + j*4]
        add edi, jv
        cmp edi, 2020
        je  solve1found
        add j, 1
        cmp j, r9d
        jb  solve1inner
        add i, 1
        cmp i, r9d
        jb  solve1outer
        ;; below shouldn't happen
        mov rax, -1
        ret
solve1found:
        mov eax, iv
        mul jv
        ret

solve2: mov r10, rax             ; number of numbers
        %define i eax
        %define iv ebx
        %define j ecx
        %define jv edx
        %define ijv edi
        %define k r8d
        %define kv r9d
        mov i, 0
solve2outer:
        mov iv, [numbers + i*4]
        mov j, i
        add j, 1
solve2middle:
        mov jv, [numbers + j*4]
        mov ijv, jv
        add ijv, iv
        cmp ijv, 2020
        jae solve2middleEnd
        mov k, j
        add k, 1
solve2inner:
        mov esi, ijv
        mov kv, [numbers + k*4]
        add esi, kv
        cmp esi, 2020
        je  solve2found
        add k, 1
        cmp k, r10d
        jb  solve2inner
solve2middleEnd:
        add j, 1
        cmp j, r10d
        jb  solve2middle
        add i, 1
        cmp i, r10d
        jb  solve2outer
        ;; below shouldn't happen
        mov rax, -1
        ret
solve2found:
        mov eax, iv
        mul jv
        mul kv
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
        mov       rdx, MAX_DIGITS + 1     ; number of bytes
        sub       rdx, rbx
        sub       rdx, 1
        syscall                           ; invoke operating system to do the write
        add       rsp, MAX_DIGITS + 1
        ret

        section .bss

buffer:         resb MAX_INPUT_BYTES   ; We can't handle larger input than this
numbers:        resd MAX_NUMBERS       ; Store at most this many numbers
