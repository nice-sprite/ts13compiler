%{

#include <stdio.h>
#include "tl13.tab.h"
// #define DEBUG
#ifdef DEBUG
    #define DBG(token) printf("\tDBG: %s | %s\n", #token, yytext); return(token);
#else
    #define DBG(token) return(token);
#endif

#define TAKE_ERROR_TOKEN                    \
    if (err_token) {                        \
        free(err_token); err_token = 0;     \
    }                                       \
    err_token = strdup(yytext);             \
    



int line_num = 0;
char* err_token = 0;

%}

%option noyywrap

%%
[ \r\t]s*                          {}
[1-9][0-9]*|0               { TAKE_ERROR_TOKEN; yylval.ival = atoi(yytext); 
                                if (yylval.ival < 0 || yylval.ival > 2147483647) {
                                    printf("%s outside legal integer range on line: %d", err_token, line_num);
                                    exit(-1);
                                } 
                                DBG(NUMBER); }
false|true                  { TAKE_ERROR_TOKEN; yylval.bval = strcmp("true", yytext) == 0; DBG(BOOLLIT); }
[A-Z][A-Z0-9]*              { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(IDENT);}
"("                         { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(LP);}
")"                         { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(RP);}
":="                        { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(ASGN);}
";"                         { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(SC);}
"*"|"div"|"mod"             { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(OP2);}
"+"|"-"                     { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(OP3);}
"="|"!="|"<"|">"|"<="|">="  { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(OP4);}
"if"                        { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(IF);}
"then"                      { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(THEN);}
"else"                      { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(ELSE);}
"begin"                     { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(MYBEGIN);}
"end"                       { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(END);}
"while"                     { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(WHILE);}
"do"                        { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(DO);}
"program"                   { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(PROGRAM);}
"var"                       { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(VAR);}
"as"                        { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(AS);}
"int"                       { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(INT);}
"bool"                      { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(BOOL);}
"writeInt"                  { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(WRITEINT);}
"readInt"                   { TAKE_ERROR_TOKEN; yylval.sval = strdup(yytext); DBG(READINT);}
\n                          { line_num++; }

%%

