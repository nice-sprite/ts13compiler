#!/bin/bash
flex scanner.lex && gcc lex.yy.c -o lexer
