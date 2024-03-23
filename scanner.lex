
/* 
 * Sample Scanner1: 
 * Description: Replace the string "username" from standard input 
 *              with the user's login name (e.g. lgao)
 * Usage: (1) $ flex sample1.lex
 *        (2) $ gcc lex.yy.c -lfl
 *        (3) $ ./a.out
 *            stdin> username
 *	      stdin> Ctrl-D
 * Question: What is the purpose of '%{' and '%}'?
 *           What else could be included in this section?
ident = [A-Z][A-Z0-9]*
left_paren = (
right_paren = )
 */

%{
/* need this for the call to getlogin() below */
#include <unistd.h>
#define PRINT printf("%.20s -> %.16s", yytext, yytext);
#define  INVALID   0
#define  NUMBER    1
#define  BOOLLIT   2
#define  IDENT     3
#define  LP        4
#define  RP        5
#define  ASGN      6
#define  SC        7
#define  OP2       8
#define  OP3       9
#define  OP4       10
#define  IF        11
#define  THEN      12
#define  ELSE      13
#define  MYBEGIN     14
#define  END       15
#define  WHILE     16
#define  DO        17
#define  PROGRAM   18
#define  VAR       19
#define  AS        20
#define  INT       21
#define  BOOL      22
#define  WRITEINT  23
#define  READINT   24

// int yywrap() { return(1);}

%}

%option noyywrap

%%
[ \n\r\t]s*                          {}
[1-9][0-9]*|0               {return(NUMBER);}
false|true                  {return(BOOLLIT);}
[A-Z][A-Z0-9]*              {return(IDENT);}
"("                         {return(LP);}
")"                         {return(RP);}
":="                        {return(ASGN);}
";"                         {return(SC);}
"*"|"div"|"mod"             {return(OP2);}
"+"|"-"                     {return(OP3);}
"="|"!="|"<"|">"|"<="|">="  {return(OP4);}
"if"                        {return(IF);}
"then"                      {return(THEN);}
"else"                      {return(ELSE);}
"begin"                     {return(MYBEGIN);}
"end"                       {return(END);}
"while"                     {return(WHILE);}
"do"                        {return(DO);}
"program"                   {return(PROGRAM);}
"var"                       {return(VAR);}
"as"                        {return(AS);}
"int"                       {return(INT);}
"bool"                      {return(BOOL);}
"writeInt"                  {return(WRITEINT);}
"readInt"                   {return(READINT);}

%%

int main()
{
    const char* token_name[] = {
        "INVALID  ", // 0
        "NUMBER   ", // 1
        "BOOLLIT  ", // 2
        "IDENT    ", // 3
        "LP       ", // 4
        "RP       ", // 5
        "ASGN     ", // 6
        "SC       ", // 7
        "OP2      ", // 8
        "OP3      ", // 9
        "OP4      ", // 10
        "IF       ", // 11
        "THEN     ", // 12
        "ELSE     ", // 13
        "BEGIN    ", // 14
        "END      ", // 15
        "WHILE    ", // 16
        "DO       ", // 17
        "PROGRAM  ", // 18
        "VAR      ", // 19
        "AS       ", // 20
        "INT      ", // 21
        "BOOL     ", // 22
        "WRITEINT ", // 23
        "READINT  ", // 24
    };

    int token_type = -1;
    while((token_type = yylex()) > 0) {
        printf("%.20s -> %s\n", token_name[token_type], yytext);
    }
    return 1;
}

