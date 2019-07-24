	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 10
	.globl	_main
	.align	4, 0x90
_main:
	pushq	%rbp
	movq	%rsp, %rbp
	subq	$16, %rsp
	callq	_make_bingo
	movq	%rax, holder(%rip)
	movq	holder(%rip), %rdi
	callq	_print_bingo
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbp
	retq

        ; https://www.sourceware.org/binutils/docs-2.12/as.info/Section.html#Section
        .section .mine, "w"
holder:
        .quad 0

