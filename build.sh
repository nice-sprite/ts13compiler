#!/bin/bash
bison -d tl13.y
flex tl13.l && gcc -g lex.yy.c tl13.tab.c -o lexer
