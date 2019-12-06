#!/bin/sh
CLISP=clisp

${CLISP} -x '(progn (load #p"src-examples/build-exe-tamagochi.lisp") (ext:quit))'
