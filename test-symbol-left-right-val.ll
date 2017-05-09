; ModuleID = 'damo'

@a = global i8* null
@b = global i8* null
@c = global i8* null
@fmtint = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@tmp = private unnamed_addr constant [5 x i8] c"PLUS\00"

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
  %b = load i8** @b
  %symbolm = call i8* @setSymbolValue(i8* %b, double 1.000000e+00)
  store i8* %symbolm, i8** @b
  %c = load i8** @c
  %symbolm3 = call i8* @setSymbolValue(i8* %c, double 3.000000e+00)
  store i8* %symbolm3, i8** @c
  %b4 = load i8** @b
  %c5 = load i8** @c
  %symbolm6 = call i8* @createRoot(i8* %b4, i8* %c5, i8* getelementptr inbounds ([5 x i8]* @tmp, i32 0, i32 0))
  store i8* %symbolm6, i8** @a
  %a = load i8** @a
  %symbol_left_call = call i8* @left(i8* %a)
  %symbol_value = call double @value(i8* %symbol_left_call)
  %printf = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @floatstr, i32 0, i32 0), double %symbol_value)
  %a7 = load i8** @a
  %symbol_right_call = call i8* @right(i8* %a7)
  %symbol_value8 = call double @value(i8* %symbol_right_call)
  %printf9 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @floatstr, i32 0, i32 0), double %symbol_value8)
  ret i32 0
}
