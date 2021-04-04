; ----------------------------------------------------------------------------------------
;       Advent of Code, 2020, Day 23
;
; Compile with
;
;     nasm -felf64 run.asm && ld -o run run.o && ./run
;
; Currently runs in ~0.2 s
; ----------------------------------------------------------------------------------------

        global    _start

        section   .text
_start:
        mov       rax, 0                ; read
        mov       rdi, 0                ; stdin
        lea       rsi, [inputBuffer]    ; to input buffer
        mov       rdx, 9                ; read 9 chars
        syscall

        ;; Read numbers from inputBuffer to inputSeq
        xor     ebx, ebx
fill:   xor     ecx, ecx
        mov     cl, [inputBuffer + ebx]
        sub     cl, '0'
        sub     cl, 1           ; remove one to work better with the linked list format
        mov     [inputSeq + ebx*4], ecx
        sub     rax, 1
        add     ebx, 1
        cmp     rax, 0
        jne     fill

        mov     rsi, 9
        mov     rdi, 100
        call    solve
        mov     rsi, 1000000
        mov     rdi, 10000000
        call    solve
        mov     rax, 60                 ; system call for exit
        xor     rdi, rdi                ; exit code 0
        syscall                         ; invoke operating system to exit

solve:
        ;; rsi stores input size
        ;; rdi stores number of loops
        ;; Setup linked list
        xor     rbx, rbx          ; i, loop var
        mov     ecx, [inputSeq + 8*4]    ; last visited, initialize as last value
        cmp     rsi, 9
        je      llloop
        mov     rcx, rsi                ; last visisted is the last if we have more than 9 elements
        sub     ecx, 1
llloop: mov     eax, [inputSeq + rbx*4] ; value @ i
        mov     [buffer + ecx*4], eax   ; link last visited to current
        mov     ecx, eax
        add     rbx, 1
        cmp     rbx, 9           ; i != input length (9)
        jne     llloop
        mov     edx, [inputSeq]
        mov     r8, rdx  ; store current value in r8
        mov     edx, [inputSeq + 8*4]
        mov     r9, rdx  ; store last value in r9
        cmp     rsi, 9
        je      moveSetup
        ;; Now, start at last in input, as rbx, and fill the rest of
        ;; the buffer.
        mov     eax, ebx
llfillRest:
        add     eax, 1
        mov     [buffer + ebx*4], eax
        add     ebx, 1
        cmp     rbx, rsi
        jne     llfillRest
        ;; Set previous last one to point to first in chain
        mov     DWORD [buffer + r9*4], 9
        ;; Set last value to the first (i.e current) one
        sub     ebx, 1
        mov     [buffer + rbx*4], r8d
        mov     r9, rsi
        sub     r9, 1

moveSetup:
        xor     eax, eax
move:   mov     r10d, [buffer + r8*4]   ; c1 = *current
        mov     r11d, [buffer + r10*4]  ; c2 = *c1
        mov     r12d, [buffer + r11*4]  ; c3 = *c2
        mov     r15d, [buffer + r12*4]  ; afterC3 = *c3
        mov     r13, r8                 ; dest
destloop:
        sub     r13, 1                  ; dest--
        cmp     r13, -1
        jne     destcheck
        mov     r13, rsi        ; wrap around, max value is rsi-1
        sub     r13, 1
destcheck:
        cmp     r13, r10        ; loop if dest = any of c1 c2 c3
        je      destloop
        cmp     r13, r11
        je      destloop
        cmp     r13, r12
        je      destloop
destfound:
        ;; Now, update pointers, the values were last read in the
        ;; following order: c3, c2, c1, curr, so it makes sense to
        ;; update them in that order, but we can't write c3 until
        ;; we've read dest, so do that first, and write to it.
        mov     r14d, [buffer + r13*4] ; afterDest = *dest
        mov     [buffer + r13*4], r10d ; *dest = c1
        mov     [buffer + r12*4], r14d ; *c3 = afterDest
        mov     [buffer + r8*4], r15d  ; *curr = afterC3
        ;; xchg    r10d, [buffer + r13*4] ; *dest = c1; afterDest = *dest
        ;; mov     [buffer + r12*4], r10d ; *c3 = afterDest
        ;; mov     [buffer + r8*4], r15d  ; *curr = afterC3
movedone:
        mov     r9, r8
        mov     r8, r15

        add     rax, 1
        cmp     rax, rdi        ; loop move rdi times
        jne     move

        cmp     rsi, 9
        je      writePart1
        jmp     writePart2

writePart1:
        sub     rsp, 0x10
        mov     rax, 0
        mov     rbx, 0          ; index to start reading at
readloop:
        mov     ecx, [buffer + rbx*4]
        mov     rdx, rcx
        add     rdx, 1
        add     rdx, '0'       ; ascii conv + 1
        mov     [rsp + rax], dl
        mov     rbx, rcx
        add     rax, 1
        cmp     rax, 8
        jne     readloop
        mov     [rsp + rax], BYTE 0x0a ; newline!
writePart1write:
        ;; Write to stdout
        mov       rax, 1                  ; system call for write
        mov       rdi, 1                  ; file handle 1 is stdout
        lea       rsi, [rsp]              ; address of string to output
        mov       rdx, 9                  ; number of bytes (8 nums + newline)
        syscall                           ; invoke operating system to do the write
        add       rsp, 0x10
        ret

writePart2:
        sub     rsp, 0x20
        mov     ebx, 0          ; index to start reading at
        mov     rax, 1          ; resulting product
        mov     ebx, [buffer + rbx*4]
        mov     rdi, rbx
        add     rdi, 1
        mul     rdi
        mov     ebx, [buffer + rbx*4]
        add     rbx, 1
        mul     rbx
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
        add       rsp, 0x20
        ret

        section   .data

message:  db        "input.txt", 0      ; note the newline at the end

        section .bss

inputBuffer:    resb 16
inputSeq:       resd 9
buffer:         resd 1000000
