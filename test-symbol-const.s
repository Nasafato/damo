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
	callq	createSymbol
	movq	%rax, a(%rip)
	callq	createSymbol
	movq	%rax, b(%rip)
	callq	createSymbol
	movq	%rax, c(%rip)
	callq	createSymbol
	movq	%rax, d(%rip)
	vmovsd	.LCPI0_0(%rip), %xmm0
	movq	%rax, %rdi
	callq	setSymbolValue
	movq	%rax, d(%rip)
	movq	b(%rip), %rdi
	movq	c(%rip), %rsi
	movl	$.Ltmp, %edx
	callq	createRoot
	movq	d(%rip), %rsi
	movl	$.Ltmp12, %edx
	movq	%rax, %rdi
	callq	createRoot
	movq	%rax, a(%rip)
	xorl	%eax, %eax
	popq	%rdx
	ret
.Ltmp3:
	.size	main, .Ltmp3-main
	.cfi_endproc

	.type	a,@object               # @a
	.bss
	.globl	a
	.align	8
a:
	.quad	0
	.size	a, 8

	.type	b,@object               # @b
	.globl	b
	.align	8
b:
	.quad	0
	.size	b, 8

	.type	c,@object               # @c
	.globl	c
	.align	8
c:
	.quad	0
	.size	c, 8

	.type	d,@object               # @d
	.globl	d
	.align	8
d:
	.quad	0
	.size	d, 8

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

	.type	.Ltmp,@object           # @tmp
.Ltmp:
	.asciz	"PLUS"
	.size	.Ltmp, 5

	.type	.Ltmp12,@object         # @tmp1
.Ltmp12:
	.asciz	"PLUS"
	.size	.Ltmp12, 5


	.section	".note.GNU-stack","",@progbits
