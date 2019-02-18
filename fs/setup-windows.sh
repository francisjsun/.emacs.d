#!/bin/sh

echo "**************** begin windows setup ****************"
if ! command -v $clang 1>/dev/null
then
echo "******** downloading clang ********"
curl -L http://releases.llvm.org/7.0.1/LLVM-7.0.1-win64.exe -o ~/.emacs.d/llvm.exe
~/.emacs.d/llvm.exe
rm -v ~/.emacs.d/llvm.exe
echo "******** downloaded clang ********"
fi

echo "**************** end of windows setup ****************"

