	.file	"test.c"
	.section	.rodata
.LC0:
	.string	"\"hi there\""
	.text
	.globl	main
	//.type	main, @function /* optional */

testprint:
  pushq	%rbp
	movq	%rsp, %rbp

  // arg1 should already be in rdi, so pass to puts
	call	puts@PLT

	leave
	ret

/*
  NOTE about stack alignment and function calls.

  When a function begins executing, rsp % 16 == 8. x86_64 code expects
  that before another function call is made, rsp % 16 == 0. As its common
  for functions to start by pushing rbp onto the stack, this often isn't
  a problem. It doesn't seem to break things if the rsp is misaligned
  (based on trial and error), but the internet and the AMD64 spec pdf
  swear it should be aligned and that *bad things* can happen if its not.

  Go figure.
*/

main:
  // prologue
	pushq	%rbp                    /* push parent base pointer onto the stack */
	movq	%rsp, %rbp              /* use top of stack for new base pointer */

  // push params onto stack (not actually used)
	subq	$16, %rsp               /* allocate 16 bytes to the stack frame */
//	movl	%edi, -4(%rbp)          /* 32 bit arg 1 into stack, 4 bytes past base pointer because its value is 4 bytes wide */
//	movq	%rsi, -16(%rbp)         /* 64 bit arg 2 into stack, not sure why at -16 instead of -8 */

  /*
	  pushes the value of the eip register onto the stack (the address of the instruction after the call)
    so that later the 'ret' instruction can use it to jump back after the call completes
  */
	leaq	.LC0(%rip), %rdi
	call	testprint

	leaq	.LC1(%rip), %rdi
	call	testprint

	movl	$0, %eax                /* set function return value */

  // epilogue
	leave                         /* put base pointer, stack pointer back */
	ret                           /* pops the return address off the stack and jumps to it */

  // optional directives
	.size	main, .-main
	.ident	"GCC: (Ubuntu 6.2.0-5ubuntu12) 6.2.0 20161005"
	.section	.note.GNU-stack, "", @progbits


	.section	.rodata
.LC1:
	.string	"dux!"
