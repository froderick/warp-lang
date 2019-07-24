	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 11
	.globl	_foo
	.align	4, 0x90
_foo:                                   ## @foo
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	leaq	L_.str(%rip), %rax
	movl	%edi, -4(%rbp)
	movl	-4(%rbp), %esi
	movq	%rax, %rdi
	movb	$0, %al
	callq	_printf
	movl	$200, %esi
	movl	%eax, -8(%rbp)          ## 4-byte Spill
	movl	%esi, %eax
	addq	$16, %rsp
	popq	%rbp
	retq

	.globl	_bar
	.align	4, 0x90
_bar:                                   ## @bar
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$64, %rsp
	leaq	16(%rbp), %rax
	leaq	L_.str.1(%rip), %rdi
	movq	(%rax), %rsi
	movq	%rax, -8(%rbp)          ## 8-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.2(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	8(%rsi), %rsi
	movl	%eax, -12(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.3(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	16(%rsi), %rsi
	movl	%eax, -16(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.4(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	24(%rsi), %rsi
	movl	%eax, -20(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.5(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	32(%rsi), %rsi
	movl	%eax, -24(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.6(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	40(%rsi), %rsi
	movl	%eax, -28(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.7(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	48(%rsi), %rsi
	movl	%eax, -32(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.8(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	56(%rsi), %rsi
	movl	%eax, -36(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.9(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	64(%rsi), %rsi
	movl	%eax, -40(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.10(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	72(%rsi), %rsi
	movl	%eax, -44(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	L_.str.11(%rip), %rdi
	movq	-8(%rbp), %rsi          ## 8-byte Reload
	movq	80(%rsi), %rsi
	movl	%eax, -48(%rbp)         ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	movl	$600, %ecx              ## imm = 0x258
	movl	%eax, -52(%rbp)         ## 4-byte Spill
	movl	%ecx, %eax
	addq	$64, %rsp
	popq	%rbp
	retq

	.globl	_main
	.align	4, 0x90
_main:                                  ## @main
## BB#0:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$240, %rsp
	movl	$0, -4(%rbp)
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	$0, -20(%rbp)
	cmpl	$0, -20(%rbp)
	je	LBB2_2
## BB#1:
	leaq	L_.str.12(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -116(%rbp)        ## 4-byte Spill
	jmp	LBB2_3
LBB2_2:
	leaq	L_.str.13(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	%eax, -120(%rbp)        ## 4-byte Spill
LBB2_3:
	leaq	L_.str.14(%rip), %rdi
	movb	$0, %al
	callq	_printf
	movl	$100, %edi
	movl	%eax, -124(%rbp)        ## 4-byte Spill
	callq	_foo
	leaq	L_.str.15(%rip), %rdi
	movl	%eax, -128(%rbp)        ## 4-byte Spill
	movb	$0, %al
	callq	_printf
	leaq	-112(%rbp), %rdi
	movq	$301, -112(%rbp)        ## imm = 0x12D
	movq	$302, -104(%rbp)        ## imm = 0x12E
	movq	$303, -96(%rbp)         ## imm = 0x12F
	movq	$304, -88(%rbp)         ## imm = 0x130
	movq	$305, -80(%rbp)         ## imm = 0x131
	movq	$306, -72(%rbp)         ## imm = 0x132
	movq	$307, -64(%rbp)         ## imm = 0x133
	movq	$308, -56(%rbp)         ## imm = 0x134
	movq	$309, -48(%rbp)         ## imm = 0x135
	movq	$310, -40(%rbp)         ## imm = 0x136
	movq	$311, -32(%rbp)         ## imm = 0x137
	movq	%rsp, %rcx
	movl	$11, %edx
	movl	%edx, %esi
	leaq	-112(%rbp), %r8
	movq	%rcx, -136(%rbp)        ## 8-byte Spill
	movq	%rsi, %rcx
	movq	-136(%rbp), %rsi        ## 8-byte Reload
	movq	%rdi, -144(%rbp)        ## 8-byte Spill
	movq	%rsi, %rdi
	movq	%r8, %rsi
	rep;movsq
	movl	%eax, -148(%rbp)        ## 4-byte Spill
	callq	_bar
	xorl	%edx, %edx
	movl	%eax, -152(%rbp)        ## 4-byte Spill
	movl	%edx, %eax
	addq	$240, %rsp
	popq	%rbp
	retq

	.section	__TEXT,__cstring,cstring_literals
L_.str:                                 ## @.str
	.asciz	"hi more: %i\n"

L_.str.1:                               ## @.str.1
	.asciz	"blob.a: %llu\n"

L_.str.2:                               ## @.str.2
	.asciz	"blob.b: %llu\n"

L_.str.3:                               ## @.str.3
	.asciz	"blob.c: %llu\n"

L_.str.4:                               ## @.str.4
	.asciz	"blob.d: %llu\n"

L_.str.5:                               ## @.str.5
	.asciz	"blob.e: %llu\n"

L_.str.6:                               ## @.str.6
	.asciz	"blob.f: %llu\n"

L_.str.7:                               ## @.str.7
	.asciz	"blob.g: %llu\n"

L_.str.8:                               ## @.str.8
	.asciz	"blob.h: %llu\n"

L_.str.9:                               ## @.str.9
	.asciz	"blob.i: %llu\n"

L_.str.10:                              ## @.str.10
	.asciz	"blob.j: %llu\n"

L_.str.11:                              ## @.str.11
	.asciz	"blob.k: %llu\n"

L_.str.12:                              ## @.str.12
	.asciz	"it is true\n"

L_.str.13:                              ## @.str.13
	.asciz	"it is false\n"

L_.str.14:                              ## @.str.14
	.asciz	"hi there\n"

L_.str.15:                              ## @.str.15
	.asciz	"more fun\n"


.subsections_via_symbols
