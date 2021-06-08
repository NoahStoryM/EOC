
.global main
main:
    pushq %rbx
    movq %rsp, %rbp
    subq $40, %rsp
    jmp start
    
conclusion:
    addq $40, %rsp
    popq %rbp
    retq
    
exit:
    movq $0, %rbx
    movq $1, %rax
    int $128
    
start:
    movq $19, -8(%rbp)
    addq $3, -8(%rbp)
    movq $1, -32(%rbp)
    negq -32(%rbp)
    movq -8(%rbp), %rax
    movq %rax, -24(%rbp)
    movq -32(%rbp), %rax
    addq %rax, -24(%rbp)
    movq $4, -40(%rbp)
    movq -24(%rbp), %rax
    addq %rax, -40(%rbp)
    movq -40(%rbp), %rax
    movq %rax, -16(%rbp)
    negq -16(%rbp)
    movq -16(%rbp), %rax
    addq $7, %rax
    jmp conclusion
    

