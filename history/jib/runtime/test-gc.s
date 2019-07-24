	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 10
	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	movl	$0, -4(%rbp)
	movl	$1048576, %rdi
	movq	%rax, -16(%rbp)
	movq	-16(%rbp), %rdi
	callq	_rt_create
	movq	%rax, -24(%rbp)         # rt = -24
	movb	$0, %al
	callq	_rt_create_parent_frame_context
	movq	%rax, -40(%rbp)
	movq	%rdx, -32(%rbp)
	movl	$0, -44(%rbp)
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	cmpl	$1000, -44(%rbp)        ## imm = 0x3E8
	jge	LBB0_4
## BB#2:                                ##   in Loop: Header=BB0_1 Depth=1
	movq	-24(%rbp), %rdi    # 1 rt pointer
	movq	%rbp, %rsi         # 2 ctx base_pointer
	movq	%rsp, %rdx         # 3 ctx stack_pointer
	leaq	L_.str(%rip), %rcx # 4 raw string
	callq	_rt_alloc_str
	movq	%rax, -56(%rbp)
## BB#3:                                ##   in Loop: Header=BB0_1 Depth=1
	movl	-44(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -44(%rbp)
	jmp	LBB0_1
LBB0_4:
	movl	-4(%rbp), %eax
	addq	$64, %rsp
	popq	%rbp
	retq

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"zomg really long string"


.subsections_via_symbols
