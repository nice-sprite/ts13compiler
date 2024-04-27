%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "ast.h"
extern int yylex();
extern int yyparse();
void yyerror(const char* s);
extern int line_num;
extern char* err_token;

#define TRACE_AST 1

#if TRACE_AST == 1
    #define TRACE(where) printf("%s\n", where);
#else
    #define TRACE(where) 
#endif

%}

%union {
    int ival;
    char bval;
    char* sval;
    struct ASTNode* node;
}

%token <sval> INT
%token <sval> BOOL
%token <ival> NUMBER
%token <bval> BOOLLIT   
%token <sval> IDENT LP RP ASGN      SC        OP2       OP3       OP4       IF        THEN      ELSE      MYBEGIN   END       WHILE     DO        PROGRAM   VAR       AS        WRITEINT  READINT   

%type <node> declarations type statementSequence statement assignment ifStatement elseClause whileStatement writeInt expression simpleExpression term factor
%%

program: | PROGRAM declarations MYBEGIN statementSequence END {
        if (ast_generate_code($2, $4)) {
            printf("\nSuccess!\n");
        } else {
            printf("\nThere was an error\n");
        }
    };

declarations: { TRACE("declarations_Empty"); }
    | VAR IDENT AS type SC declarations 
    {
        TRACE("declarations");
        $$ = ast_make_declaration($2, $4, $6);
    };

type: INT { $$ = ast_make_type(DT_INT); TRACE("type int"); }
    | BOOL { $$ = ast_make_type(DT_BOOL); TRACE("type int"); }

statementSequence: { TRACE("statementSequence_Empty");$$ = ast_make_statement_seq(0, 0); } | statement SC statementSequence { 
                $$ = ast_make_statement_seq($1, $3);
                TRACE("StatementSequence");
                 };

statement: assignment {      TRACE("statement_Assignment");   $$ = ast_make_statement(ST_ASSIGN, $1); }
         | ifStatement {     TRACE("statement_IfStatement");   $$ = ast_make_statement(ST_IFBLOCK, $1); } 
         | whileStatement {  TRACE("statement_WhileStatement");   $$ = ast_make_statement(ST_WHILEBLOCK, $1); } 
         | writeInt {        TRACE("statement_WriteInt");   $$ = ast_make_statement(ST_WRITEINT, $1); }

assignment: IDENT ASGN expression { 
        TRACE("assignment_Expression");
        $$ = ast_make_assignment_expression($1, $3);
      } 
          | IDENT ASGN READINT { 
                TRACE("assignment_ReadInt");
              $$ = ast_make_assignment_readint($1);
          }

ifStatement: IF expression THEN statementSequence elseClause END {
                TRACE("ifStatement");
           $$ = ast_make_if_block($2, $4, $5);
           }

elseClause: | ELSE statementSequence {
                TRACE("elseClause");
          $$ = ast_make_else_clause($2);

          }

whileStatement: WHILE expression DO statementSequence END {
                TRACE("whileStatement");
              $$ = ast_make_while_block($2, $4);
              }

writeInt: WRITEINT expression {
                TRACE("writeInt");
        $$ = ast_make_write_int($2);
        }

expression: simpleExpression { TRACE("expression_Simple"); $$ = ast_make_unary_expression($1); }
          | simpleExpression OP4 simpleExpression {
                TRACE("expression_Binary");
                $$ = ast_make_binary_expression($1, $2, $3);
          }

simpleExpression: term OP3 term{ $$ = ast_make_binary_simple_expression($1, $2, $3); } 
                | term { $$ = ast_make_unary_simple_expression($1); }

term: factor OP2 factor { $$ = ast_make_binary_term($1, $2, $3);    }
            | factor {    $$ = ast_make_unary_term($1);             }

factor: IDENT { $$ = ast_make_factor_ident($1); } 
      | NUMBER { 
            $$ = ast_make_factor_number($1); }
      | BOOLLIT { $$ = ast_make_factor_boollit($1); }
      | LP expression RP { $$ = ast_make_factor_parenth_expr($2); }

%%

int main()
{
    yyparse();
}

void yyerror(const char* err) {
    if (err_token) {
        printf("%s: Unexpected token `%s` on line %d\n", err, err_token, line_num +1);
    } else {
        printf("\nerror on line %d: %s\n", line_num + 1, err);
    }
    exit(-1);
}


