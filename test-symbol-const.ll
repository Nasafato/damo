; ModuleID = 'damo'

@a = global i8* null
@b = global i8* null
@c = global i8* null
@d = global i8* null
@fmtint = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr = private unnamed_addr constant [4 x i8] c"%f\0A\00"
@tmp = private unnamed_addr constant [5 x i8] c"PLUS\00"
@tmp1 = private unnamed_addr constant [5 x i8] c"PLUS\00"

declare i32 @printf(i8*, ...)

declare i8* @createSymbol()

declare i1 @isConstant(i8*)

declare i8* @createRoot(i8*, i8*, i8*)

declare i8* @setSymbolValue(i8*, double)

declare double @value(i8*)

declare i1 @isInitialized(i8*)

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
  %d = load i8** @d
  %symbolm = call i8* @setSymbolValue(i8* %d, double 1.000000e+00)
  store i8* %symbolm, i8** @d
  %b = load i8** @b
  %c = load i8** @c
  %symbolm4 = call i8* @createRoot(i8* %b, i8* %c, i8* getelementptr inbounds ([5 x i8]* @tmp, i32 0, i32 0))
  %d5 = load i8** @d
  %symbolm6 = call i8* @createRoot(i8* %symbolm4, i8* %d5, i8* getelementptr inbounds ([5 x i8]* @tmp1, i32 0, i32 0))
  store i8* %symbolm6, i8** @a
  ret i32 0
}
