# alternatives to this
# - store runtime in global variable (c)
# - store entry point base pointer in global variable (c)
# - initialize entry point base pointer via (c)
# - asm calls init
# - asm allocates

.section __TEXT, __text, regular, pure_instructions
.macosx_version_min 10, 10
.globl   _main
.align   4, 0x90

_main:
	pushq %rbp
	movq  %rsp, %rbp

# store main $rsp in global var
movq %rbp, main_rsp(%rip)

# create rt instance, store in global var
movq  main_rsp(%rip), %rdi
movq  $500, %rsi
callq _rt_create
movq  %rax, rt_instance(%rip)

# make room on stack for stuff
subq $64, %rsp

# allocate a list
movq  rt_instance(%rip), %rdi  # 1 rt pointer
movq  %rbp, %rsi               # 2 ctx base_pointer
movq  %rsp, %rdx               # 3 ctx stack_pointer
callq _rt_alloc_list
movq  %rax, -24(%rbp)

# allocate a string
movq  rt_instance(%rip), %rdi  # 1 rt pointer
movq  %rbp, %rsi               # 2 ctx base_pointer
movq  %rsp, %rdx               # 3 ctx stack_pointer
leaq  L_.str(%rip), %rcx       # 4 raw string
callq _rt_alloc_str
movq  %rax, -32(%rbp)

# cons string into list
movq  rt_instance(%rip), %rdi  # 1 rt pointer
movq  %rbp, %rsi               # 2 ctx base_pointer
movq  %rsp, %rdx               # 3 ctx stack_pointer
movq  -32(%rbp), %rcx          # 4 string
movq  -24(%rbp), %r8           # 5 list
callq _rt_val_cons
movq  %rax, -24(%rbp)

# forget about list
# movq	$0, -24(%rbp)

# loop and allocate to force gc
movq $0, -8(%rbp)

alloc_loop:
	cmpl $20, -8(%rbp)
	jge  alloc_loop_done

# print something
leaq  L_.str(%rip), %rdi
callq _printf

# allocate a string
movq  rt_instance(%rip), %rdi  # 1 rt pointer
movq  %rbp, %rsi               # 2 ctx base_pointer
movq  %rsp, %rdx               # 3 ctx stack_pointer
leaq  L_.str(%rip), %rcx       # 4 raw string
callq _rt_alloc_str
movq  %rax, -16(%rbp)

# increment counter and loop
	movq -8(%rbp), %rax
	addq $1, %rax
	movq %rax, -8(%rbp)
	jmp  alloc_loop

alloc_loop_done:
	addq $64, %rsp

# destroy rt instance
movq  rt_instance(%rip), %rdi
callq _rt_destroy

popq %rbp
retq

# https: // www.sourceware.org/binutils/docs-2.12/as.info/Section.html#Section
.section .mine, "w"

main_rsp:
	.quad 0

rt_instance:
	.quad 0

	.section __TEXT, __cstring, cstring_literals
	L_.str:                                 ## @.str
	.asciz   "hi mom this text needs to be longer\n"

