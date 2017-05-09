	.file	"test-symbol-const.ll"
	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI0_0:
	.quad	4607182418800017408     # double 1
.LCPI0_1:
	.quad	4611686018427387904     # double 2
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	callq	createSymbol
	movq	%rax, a(%rip)
	callq	createSymbol
	movq	%rax, b(%rip)
	callq	createSymbol
	movq	%rax, c(%rip)
	callq	createSymbol
	movq	%rax, d(%rip)
	callq	createSymbol
	movq	%rax, f(%rip)
	callq	createSymbol
	movq	%rax, g(%rip)
	callq	createSymbol
	movq	%rax, h(%rip)
	movq	d(%rip), %rdi
	vmovsd	.LCPI0_0(%rip), %xmm0
	callq	setSymbolValue
	movq	%rax, d(%rip)
	movq	b(%rip), %rbx
	callq	createSymbol
	movq	%rax, %rdi
	vmovsd	.LCPI0_0(%rip), %xmm0
	callq	setSymbolValue
	movl	$.Ltmp, %edx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	createRoot
	movq	%rax, a(%rip)
	movq	c(%rip), %rdi
	callq	sym_test
	movq	g(%rip), %rdi
	vmovsd	.LCPI0_0(%rip), %xmm0
	callq	setSymbolValue
	movq	%rax, g(%rip)
	movq	h(%rip), %rdi
	vmovsd	.LCPI0_1(%rip), %xmm0
	callq	setSymbolValue
	movq	%rax, h(%rip)
	movq	g(%rip), %rdi
	movl	$.Ltmp14, %edx
	movq	%rax, %rsi
	callq	createRoot
	movq	%rax, f(%rip)
	movl	$.Ltmp35, %edi
	movl	$.Ltmp26, %esi
	callq	streq
	andb	$1, %al
	movb	%al, temp(%rip)
	je	.LBB0_2
# BB#1:                                 # %then
	movl	$.Lfmtstr, %edi
	movl	$.Ltmp4, %esi
	xorl	%eax, %eax
	callq	printf
.LBB0_2:                                # %merge
	movq	f(%rip), %rdi
	callq	right
	movq	%rax, %rdi
	callq	value
	movl	$.Lfloatstr, %edi
	movb	$1, %al
	callq	printf
	movq	f(%rip), %rdi
	callq	operator
	movq	%rax, %rcx
	movl	$.Lfmtstr, %edi
	xorl	%eax, %eax
	movq	%rcx, %rsi
	callq	printf
	movq	g(%rip), %rdi
	callq	isConstant
	movl	%eax, %ecx
	movl	$.Lfmtint, %edi
	xorl	%eax, %eax
	movl	%ecx, %esi
	callq	printf
	xorl	%eax, %eax
	popq	%rbx
	ret
.Ltmp7:
	.size	main, .Ltmp7-main
	.cfi_endproc

	.globl	streq
	.align	16, 0x90
	.type	streq,@function
streq:                                  # @streq
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp9:
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movq	16(%rsp), %rdi
	callq	strcmp
	testl	%eax, %eax
	sete	%al
	addq	$24, %rsp
	ret
.Ltmp10:
	.size	streq, .Ltmp10-streq
	.cfi_endproc

	.section	.rodata.cst8,"aM",@progbits,8
	.align	8
.LCPI2_0:
	.quad	4607182418800017408     # double 1
	.text
	.globl	sym_test
	.align	16, 0x90
	.type	sym_test,@function
sym_test:                               # @sym_test
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp1415:
	.cfi_def_cfa_offset 32
.Ltmp16:
	.cfi_offset %rbx, -16
	movq	%rdi, 8(%rsp)
	movq	(%rsp), %rbx
	callq	createSymbol
	vmovsd	.LCPI2_0(%rip), %xmm0
	movq	%rax, %rdi
	callq	setSymbolValue
	movl	$.Ltmp1117, %edx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	createRoot
	movq	%rax, 8(%rsp)
	xorl	%eax, %eax
	addq	$16, %rsp
	popq	%rbx
	ret
.Ltmp18:
	.size	sym_test, .Ltmp18-sym_test
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

	.type	f,@object               # @f
	.globl	f
	.align	8
f:
	.quad	0
	.size	f, 8

	.type	g,@object               # @g
	.globl	g
	.align	8
g:
	.quad	0
	.size	g, 8

	.type	h,@object               # @h
	.globl	h
	.align	8
h:
	.quad	0
	.size	h, 8

	.type	temp,@object            # @temp
	.globl	temp
temp:
	.byte	0                       # 0x0
	.size	temp, 1

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

	.type	.Ltmp14,@object         # @tmp1
.Ltmp14:
	.asciz	"PLUS"
	.size	.Ltmp14, 5

	.type	.Ltmp26,@object         # @tmp2
.Ltmp26:
	.asciz	"A"
	.size	.Ltmp26, 2

	.type	.Ltmp35,@object         # @tmp3
.Ltmp35:
	.asciz	"A"
	.size	.Ltmp35, 2

	.type	.Ltmp4,@object          # @tmp4
.Ltmp4:
	.asciz	"Hi"
	.size	.Ltmp4, 3

	.type	.Lfmtint5,@object       # @fmtint5
.Lfmtint5:
	.asciz	"%d\n"
	.size	.Lfmtint5, 4

	.type	.Lfmtstr6,@object       # @fmtstr6
.Lfmtstr6:
	.asciz	"%s\n"
	.size	.Lfmtstr6, 4

	.type	.Lfloatstr7,@object     # @floatstr7
.Lfloatstr7:
	.asciz	"%f\n"
	.size	.Lfloatstr7, 4

	.type	.Lfmtint8,@object       # @fmtint8
.Lfmtint8:
	.asciz	"%d\n"
	.size	.Lfmtint8, 4

	.type	.Lfmtstr9,@object       # @fmtstr9
.Lfmtstr9:
	.asciz	"%s\n"
	.size	.Lfmtstr9, 4

	.type	.Lfloatstr10,@object    # @floatstr10
.Lfloatstr10:
	.asciz	"%f\n"
	.size	.Lfloatstr10, 4

	.type	.Ltmp1117,@object       # @tmp11
.Ltmp1117:
	.asciz	"PLUS"
	.size	.Ltmp1117, 5


	.section	".note.GNU-stack","",@progbits
