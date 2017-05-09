	.file	"test-symbol-left-right-val.ll"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408     # double 1
.LCPI0_1:
	.quad	4613937818241073152     # double 3
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
	movq	b(%rip), %rdi
	vmovsd	.LCPI0_0(%rip), %xmm0
	callq	setSymbolValue
	movq	%rax, b(%rip)
	movq	c(%rip), %rdi
	vmovsd	.LCPI0_1(%rip), %xmm0
	callq	setSymbolValue
	movq	%rax, c(%rip)
	movq	b(%rip), %rdi
	movl	$.Ltmp, %edx
	movq	%rax, %rsi
	callq	createRoot
	movq	%rax, a(%rip)
	movq	%rax, %rdi
	callq	left
	movq	%rax, %rdi
	callq	value
	movl	$.Lfloatstr, %edi
	movb	$1, %al
	callq	printf
	movq	a(%rip), %rdi
	callq	right
	movq	%rax, %rdi
	callq	value
	movl	$.Lfloatstr, %edi
	movb	$1, %al
	callq	printf
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


	.section	".note.GNU-stack","",@progbits
