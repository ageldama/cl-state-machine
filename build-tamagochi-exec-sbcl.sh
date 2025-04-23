#!/bin/bash
SBCL=${SBCL:-run-sbcl.sh}

${SBCL} --load src-examples/build-exe-tamagochi.lisp --quit
