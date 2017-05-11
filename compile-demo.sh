#!/bin/sh

# Path to the LLVM interpreter
LLI="lli"

# Path to the LLVM compiler
LLC="llc"

# Path to the C compiler
CC="cc"

# Path to the damo compiler.  Usually "./damo.native"
DAMO="./damo.native"

# Set time limit for all operations
ulimit -t 30

error=0
globalerror=0

SignalError() {
    if [ $error -eq 0 ] ; then
	echo "FAILED"
	error=1
    fi
    echo "  $1"
}

# Run <args>
# Report the command, run it, and report any errors
Run() {
    eval $* || {
	SignalError "$1 failed on $*"
	return 1
    }
}

Check() {
    error=0

    basename=`echo $1 | sed 's/.*\\///
                             s/.dm//'`
    reffile=`echo $1 | sed 's/.dm$//'`
    basedir="`echo $1 | sed 's/\/[^\/]*$//'`/."

    generatedfiles=""

    generatedfiles="$generatedfiles ${basename}.dml ${basename}.ll ${basename}.s" &&
    # Prepend standard library
    Run "cat" "stdlib.dm" ">" "${basename}.dml" &&
    Run "cat" $1 ">>" "${basename}.dml" &&
    # Proceed with compilation
    Run "$DAMO" "<" "${basename}.dml" ">" "${basename}.ll" &&
    Run "$LLC" "${basename}.ll" ">" "${basename}.s" &&
    Run "$CC" "-o" "${basename}.exe" "${basename}.s" "printbig.o" "symbol.o" "-lm"

    if [ $error -eq 0 ] ; then
    echo "${basename}.dm" " compiled"
    else
    echo "###### FAILED" 1>&2
    globalerror=$error
    fi

    rm -rf $generatedfiles
}

LLIFail() {
  echo "Could not find the LLVM interpreter \"$LLI\"."
  echo "Check your LLVM installation and/or modify the LLI variable in testall.sh"
  exit 1
}

which "$LLI" > /dev/null || LLIFail

if [ ! -f printbig.o ]
then
    echo "Could not find printbig.o"
    echo "Try \"make printbig.o\""
    exit 1
fi

files="demo/*.dm"

for file in $files
do
    Check $file
done

exit $globalerror
