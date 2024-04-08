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
        ASTNode* current_decl = $2;
        while(current_decl) {
            printf("%d %s\n", current_decl->declaration.datatype, current_decl->declaration.ident);
            current_decl = current_decl->declaration.next;
        }
        printf("\nSuccess!\n");
    };

declarations: | VAR IDENT AS type SC declarations 
    {
        $$ = ast_make_declaration($2, $4, $6);
    };

type: INT { $$ = ast_make_type(DT_INT); }
    | BOOL { $$ = ast_make_type(DT_BOOL); }

statementSequence: | statement SC statementSequence { 
                 $$ = ast_make_statement_seq($1, $3);
                 };

statement: assignment {         $$ = ast_make_statement(ST_ASSIGN, $1); }
         | ifStatement {        $$ = ast_make_statement(ST_IFBLOCK, $1); } 
         | whileStatement {     $$ = ast_make_statement(ST_WHILEBLOCK, $1); } 
         | writeInt {           $$ = ast_make_statement(ST_WRITEINT, $1); }

assignment: IDENT ASGN expression { 
              $$ = ast_make_assignment_expression($1, $3);
          } 
          | IDENT ASGN READINT { 
              $$ = ast_make_assignment_readint($1);
          }

ifStatement: IF expression THEN statementSequence elseClause END {
           $$ = ast_make_if_block($2, $4, $5);
           }

elseClause: | ELSE statementSequence {
          $$ = ast_make_else_clause($2);

          }

whileStatement: WHILE expression DO statementSequence END {
              $$ = ast_make_while_block($2, $4);
              }

writeInt: WRITEINT expression {
        $$ = ast_make_write_int($2);
        }

expression: simpleExpression { $$ = ast_make_unary_expression($1); }
          | simpleExpression OP4 simpleExpression {
                $$ = ast_make_binary_expression($1, $2, $3);
          }

simpleExpression: term OP3 term{ $$ = ast_make_binary_simple_expression($1, $2, $3); } 
                | term { $$ = ast_make_unary_simple_expression($1); }

term: factor OP2 factor { $$ = ast_make_binary_term($1, $2, $3);    }
            | factor {    $$ = ast_make_unary_term($1);             }

factor: IDENT { $$ = ast_make_factor_ident($1); } 
      | NUMBER { $$ = ast_make_factor_number($1); }
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


