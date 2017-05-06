	.file	"test-expr.ll"
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
	callq	test
	xorl	%eax, %eax
	popq	%rdx
	ret
.Ltmp2:
	.size	main, .Ltmp2-main
	.cfi_endproc

	.globl	test
	.align	16, 0x90
	.type	test,@function
test:                                   # @test
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp4:
	.cfi_def_cfa_offset 32
	movl	$2, 16(%rsp)
	movl	$1, 20(%rsp)
	movl	16(%rsp), %esi
	incl	%esi
	movl	%esi, 12(%rsp)
	movl	$.Lfmtint1, %edi
	xorl	%eax, %eax
	callq	printf
	movl	$.Lfmtint1, %edi
	movl	$3, %esi
	xorl	%eax, %eax
	callq	printf
	xorl	%eax, %eax
	addq	$24, %rsp
	ret
.Ltmp5:
	.size	test, .Ltmp5-test
	.cfi_endproc

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

	.type	.Lfmtint1,@object       # @fmtint1
.Lfmtint1:
	.asciz	"%d\n"
	.size	.Lfmtint1, 4

	.type	.Lfmtstr2,@object       # @fmtstr2
.Lfmtstr2:
	.asciz	"%s\n"
	.size	.Lfmtstr2, 4

	.type	.Lfloatstr3,@object     # @floatstr3
.Lfloatstr3:
	.asciz	"%f\n"
	.size	.Lfloatstr3, 4


	.section	".note.GNU-stack","",@progbits
