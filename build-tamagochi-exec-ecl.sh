#!/bin/bash
ECL=${ECL:-ecl}

${ECL} --load src-examples/build-exe-tamagochi.lisp --eval '(quit)'
