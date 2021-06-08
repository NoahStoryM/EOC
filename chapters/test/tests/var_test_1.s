
.global main
main:
    pushq %rbx
    movq %rsp, %rbp
    subq $0, %rsp
    jmp start
    
conclusion:
    addq $0, %rsp
    popq %rbp
    retq
    
exit:
    movq $0, %rbx
    movq $1, %rax
    int $128
    
start:
    movq $42, %rax
    jmp conclusion
    

