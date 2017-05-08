	.file	"test-symbol-const.ll"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408     # double 1
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	vmovsd	.LCPI0_0(%rip), %xmm0
	callq	createConstSymbol
	movq	%rax, a(%rip)
	movq	%rax, %rdi
	callq	value
	xorl	%eax, %eax
	popq	%rdx
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.type	a,@object               # @a
	.bss
	.globl	a
	.align	8
a:
	.quad	0
	.size	a, 8

	.type	.Lfmtint,@object        # @fmtint
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lfmtint:
	.asciz	"%d\n"
	.size	.Lfmtint, 4

	.type	.Lfmtstr,@object        # @fmtstr
.Lfmtstr:
	.asciz	"%s\n"
	.size	.Lfmtstr, 4

	.type	.Lfloatstr,@object      # @floatstr
.Lfloatstr:
	.asciz	"%f\n"
	.size	.Lfloatstr, 4


	.section	".note.GNU-stack","",@progbits
