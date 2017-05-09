; ModuleID = 'damo'

@a = global i8* null
@b = global i8* null
@c = global i8* null
@d = global i8* null
@f = global i8* null
@g = global i8* null
@h = global i8* null
@temp = global i1 false
@fmtint = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@tmp = private unnamed_addr constant [5 x i8] c"PLUS\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"PLUS\00"
@tmp2 = private unnamed_addr constant [2 x i8] c"A\00"
@tmp3 = private unnamed_addr constant [2 x i8] c"A\00"
@tmp4 = private unnamed_addr constant [3 x i8] c"Hi\00"
@fmtint5 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr6 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr7 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@fmtint8 = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr9 = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr10 = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@tmp11 = private unnamed_addr constant [5 x i8] c"PLUS\00"

declare i32 @printf(i8*, ...)

declare i32 @strcmp(i8*, i8*)

declare i8* @operator(i8*)

declare i8* @createSymbol()

declare i32 @isConstant(i8*)

declare i8* @createRoot(i8*, i8*, i8*)

declare i8* @setSymbolValue(i8*, double)

declare double @value(i8*)

declare i32 @isInitialized(i8*)

declare i8* @left(i8*)

declare i8* @right(i8*)

declare double @pow(double, double)

declare double @log(double)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %symbolmal = call i8* @createSymbol()
  store i8* %symbolmal, i8** @a
  %symbolmal1 = call i8* @createSymbol()
  store i8* %symbolmal1, i8** @b
  %symbolmal2 = call i8* @createSymbol()
  store i8* %symbolmal2, i8** @c
  %symbolmal3 = call i8* @createSymbol()
  store i8* %symbolmal3, i8** @d
  %symbolmal4 = call i8* @createSymbol()
  store i8* %symbolmal4, i8** @f
  %symbolmal5 = call i8* @createSymbol()
  store i8* %symbolmal5, i8** @g
  %symbolmal6 = call i8* @createSymbol()
  store i8* %symbolmal6, i8** @h
  %d = load i8** @d
  %symbolm = call i8* @setSymbolValue(i8* %d, double 1.000000e+00)
  store i8* %symbolm, i8** @d
  %b = load i8** @b
  %symbolmal7 = call i8* @createSymbol()
  %symbolm8 = call i8* @setSymbolValue(i8* %symbolmal7, double 1.000000e+00)
  %symbolm9 = call i8* @createRoot(i8* %symbolm8, i8* %b, i8* getelementptr inbounds ([5 x i8]* @tmp, i32 0, i32 0))
  store i8* %symbolm9, i8** @a
  %c = load i8** @c
  %sym_test_result = call i32 @sym_test(i8* %c)
  %g = load i8** @g
  %symbolm10 = call i8* @setSymbolValue(i8* %g, double 1.000000e+00)
  store i8* %symbolm10, i8** @g
  %h = load i8** @h
  %symbolm11 = call i8* @setSymbolValue(i8* %h, double 2.000000e+00)
  store i8* %symbolm11, i8** @h
  %g12 = load i8** @g
  %h13 = load i8** @h
  %symbolm14 = call i8* @createRoot(i8* %g12, i8* %h13, i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0))
  store i8* %symbolm14, i8** @f
  %streq_result = call i1 @streq(i8* getelementptr inbounds ([2 x i8]* @tmp3, i32 0, i32 0), i8* getelementptr inbounds ([2 x i8]* @tmp2, i32 0, i32 0))
  store i1 %streq_result, i1* @temp
  %temp = load i1* @temp
  br i1 %temp, label %then, label %else

merge:                                            ; preds = %else, %then
  %f = load i8** @f
  %symbol_right_call = call i8* @right(i8* %f)
  %symbol_value = call double @value(i8* %symbol_right_call)
  %printf15 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @floatstr, i32 0, i32 0), double %symbol_value)
  %f16 = load i8** @f
  %symbol_operator = call i8* @operator(i8* %f16)
  %printf17 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmtstr, i32 0, i32 0), i8* %symbol_operator)
  %g18 = load i8** @g
  %symbol_const = call i32 @isConstant(i8* %g18)
  %printf19 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmtint, i32 0, i32 0), i32 %symbol_const)
  ret i32 0

then:                                             ; preds = %entry
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @fmtstr, i32 0, i32 0), i8* getelementptr inbounds ([3 x i8]* @tmp4, i32 0, i32 0))
  br label %merge

else:                                             ; preds = %entry
  br label %merge
}

define i1 @streq(i8* %a, i8* %b) {
entry:
  %a1 = alloca i8*
  store i8* %a, i8** %a1
  %b2 = alloca i8*
  store i8* %b, i8** %b2
  %b3 = load i8** %b2
  %a4 = load i8** %a1
  %strcmp = call i32 @strcmp(i8* %a4, i8* %b3)
  %tmp = icmp eq i32 %strcmp, 0
  ret i1 %tmp
}

define i32 @sym_test(i8* %node) {
entry:
  %node1 = alloca i8*
  store i8* %node, i8** %node1
  %e = alloca i8*
  %e2 = load i8** %e
  %symbolmal = call i8* @createSymbol()
  %symbolm = call i8* @setSymbolValue(i8* %symbolmal, double 1.000000e+00)
  %symbolm3 = call i8* @createRoot(i8* %symbolm, i8* %e2, i8* getelementptr inbounds ([5 x i8]* @tmp11, i32 0, i32 0))
  store i8* %symbolm3, i8** %node1
  ret i32 0
}
