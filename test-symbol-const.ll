; ModuleID = 'damo'

@a = global i8* null
@fmtint = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@fmtstr = private unnamed_addr constant [4 x i8] c"%s\0A\00"
@floatstr = private unnamed_addr constant [4 x i8] c"%f\0A\00"

declare i32 @printf(i8*, ...)

declare i8* @createConstSymbol(double)

declare double @value(i8*)

declare double @pow(double, double)

declare double @log(double)

declare i32 @printbig(i32)

define i32 @main() {
entry:
  %symbolm = call i8* @createConstSymbol(double 1.000000e+00)
  store i8* %symbolm, i8** @a
  %a = load i8** @a
  %symbol_value = call double @value(i8* %a)
  ret i32 0
}
