
.global main
main:
    pushq %rbx
    movq %rsp, %rbp
    subq $8, %rsp
    jmp start
    
conclusion:
    addq $8, %rsp
    popq %rbp
    retq
    
exit:
    movq $0, %rbx
    movq $1, %rax
    int $128
    
start:
    movq $41, -8(%rbp)
    movq -8(%rbp), %rax
    addq $1, %rax
    jmp conclusion
    

