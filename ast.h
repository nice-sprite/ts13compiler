#ifndef AST_HEADER
#define AST_HEADER
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef char bool;

typedef enum {
    FT_IDENT,
    FT_NUMBER,
    FT_BOOLLIT,
    FT_PARENTH_EXPR,

    TT_BINARY,
    TT_FACTOR,

    SE_TERM,
    SE_BINARY_EXPR,

    ET_SIMPLE,
    ET_BINARY,

    AT_EXPRESSION,
    AT_READINT,

    ST_ASSIGN,
    ST_IFBLOCK,
    ST_WHILEBLOCK,
    ST_WRITEINT,

    DT_DECLARATION,
    TYPE,
    STATEMENT_SEQUENCE,
    STATEMENT,
    ASSIGNMENT,
    IF_BLOCK,
    ELSE_CLAUSE,
    WHILE_BLOCK,
    WRITE_INT,
} ASTNodeType;


typedef enum DataType {
    DT_INT,
    DT_BOOL,
} DataType;

typedef struct Type {
    DataType datatype;
} Type;

typedef struct Declaration{
    DataType datatype;
    const char* ident;
    struct ASTNode* next;
} Declaration;

typedef struct Factor {
    union {
        const char* ident;
        int number;
        bool bool_lit;
        struct ASTNode* expr;
    };
}Factor;

typedef enum OperationType {
    // op3
    OP_PLUS,
    OP_MINUS,

    // op2
    OP_MUL,
    OP_DIV,
    OP_MOD,

    // op4
    OP_EQ,
    OP_NEQ,
    OP_LESS,
    OP_GREATER,
    OP_LESS_EQ,
    OP_GREATER_EQ,
}OperationType;

typedef struct Term {
    struct ASTNode* factor1;
    OperationType op2;
    struct ASTNode* factor2;
}Term;

typedef struct SimpleExpression {
    struct ASTNode* term1;
    OperationType op3;
    struct ASTNode* term2;
}SimpleExpression ;

typedef struct Expression {
    struct ASTNode* expr1;
    OperationType op4;
    struct ASTNode* expr2;
}Expression ;

typedef struct ElseClause {
    struct ASTNode* statements;
}ElseClause ;

typedef struct IfBlock {
    struct ASTNode* expr;
    struct ASTNode* statement_sequence;
    struct ASTNode* else_clause;
}IfBlock;

typedef struct WhileBlock{
    struct ASTNode* expr;
    struct ASTNode* statements;
}WhileBlock;

typedef struct WriteInt {
    struct ASTNode* expr;
}WriteInt ;


typedef struct Assignment {
    char* ident;
    struct ASTNode* expr;
}Assignment;

typedef struct Statement {
    union {
        struct ASTNode* asgn;
        struct ASTNode* ifblock;
        struct ASTNode* while_block;
        struct ASTNode* writeInt;
    };
} Statement;


typedef struct StatementSequence {
    struct ASTNode*  statement;
    struct ASTNode* next;
} StatementSequence;

typedef struct Program{
    Declaration* decls;
    StatementSequence* statements;
} Program;

typedef struct ASTNode {
    ASTNodeType kind;
    union {
        Program program;
        Declaration declaration;
        StatementSequence statement_sequence;
        Statement statement;
        Assignment assignment;
        IfBlock if_block;
        ElseClause else_block;
        WhileBlock while_block;
        WriteInt write_int;
        Expression expression;
        SimpleExpression simple_expression;
        Term term;
        Factor factor;
        Type datatype;
    };
} ASTNode;

ASTNode* alloc_node(ASTNodeType type) {
    ASTNode* node = (ASTNode*)malloc(sizeof(ASTNode));
    if (!node) {
        printf("OOM");
        exit(-1);
    }
    node->kind = type;
    return node;
}

void free_node(ASTNode* node) {
    if (node) { // dont double free
        free(node);
    }
}

// functions 
ASTNode* ast_make_program(ASTNode* declarations, ASTNode* statements);

ASTNode* ast_make_declaration(const char* identifier, ASTNode* datatype, ASTNode* next_declaration) {
    ASTNode* decl_node = alloc_node(DT_DECLARATION);
    decl_node->declaration.datatype = datatype->datatype.datatype;
    decl_node->declaration.ident = strdup(identifier);
    decl_node->declaration.next = next_declaration;
    return decl_node;
}

ASTNode* ast_make_type(DataType datatype) {
    ASTNode* decl_node = alloc_node(TYPE);
    decl_node->datatype.datatype = datatype;
    return decl_node;
}

ASTNode* ast_make_statement_seq(ASTNode* statement, ASTNode* statement_sequence) {
    ASTNode* node = alloc_node(STATEMENT_SEQUENCE);
    node->statement_sequence.statement = statement;
    node->statement_sequence.next = statement_sequence;
    return node;
}

ASTNode* ast_make_statement(ASTNodeType statement_type, ASTNode* node) {
    ASTNode* snode = alloc_node(statement_type);
    switch(snode->kind) {
        case ST_ASSIGN:
            snode->statement.asgn = node;
            break;
        case ST_IFBLOCK:
            snode->statement.ifblock = node;
            break;
        case ST_WHILEBLOCK:
            snode->statement.while_block = node;
            break;
        case ST_WRITEINT:
            snode->statement.writeInt = node;
            break;
        default:
            printf("invalid statement type!!!\n");
            exit(-1);
    }
    return snode;
}

ASTNode* ast_make_assignment_expression(const char* ident, ASTNode* expr) {
    ASTNode* snode = alloc_node(AT_EXPRESSION);
    snode->assignment.ident = strdup(ident);
    snode->assignment.expr = expr;
    return snode;
}

ASTNode* ast_make_assignment_readint(const char* ident) {
    ASTNode* snode = alloc_node(AT_READINT);
    snode->assignment.ident = strdup(ident);
    return snode;
}

ASTNode* ast_make_if_block(ASTNode* expression, ASTNode* statement_sequence, ASTNode* else_clause) {
    ASTNode* snode = alloc_node(IF_BLOCK);
    snode->if_block.expr = expression;
    snode->if_block.statement_sequence = statement_sequence;
    snode->if_block.else_clause = else_clause;
    return snode;
}

ASTNode* ast_make_else_clause(ASTNode* statement_sequence) {
    ASTNode* snode = alloc_node(ELSE_CLAUSE);
    snode->else_block.statements = statement_sequence;
    return snode;
}

ASTNode* ast_make_while_block(ASTNode* expression, ASTNode* statements) {
    ASTNode* snode = alloc_node(WHILE_BLOCK);
    snode->while_block.expr = expression;
    snode->while_block.statements = statements;
    return snode;
}

ASTNode* ast_make_write_int(ASTNode* expression) {
    ASTNode* snode = alloc_node(WRITE_INT);
    snode->write_int.expr = expression;
    return snode;
}

ASTNode* ast_make_unary_expression(ASTNode* simple_expression) {
    ASTNode* snode = alloc_node(ET_SIMPLE);
    snode->expression.expr1 = simple_expression;
    return snode;
}

ASTNode* ast_make_binary_expression(ASTNode* simple_expression1, const char* op4_lexeme, ASTNode* simple_expression2) {
    ASTNode* snode = alloc_node(ET_BINARY);
    snode->expression.expr1 = simple_expression1;
    snode->expression.expr2 = simple_expression2;

    // figure out op type
    OperationType op4;
    if (strcmp(op4_lexeme, "=")) {
        op4 = OP_EQ;
    } else if(strcmp(op4_lexeme, "!=")) {
        op4 = OP_NEQ;
    } else if(strcmp(op4_lexeme, "<")) {
        op4 = OP_LESS;
    } else if (strcmp(op4_lexeme, ">")) {
        op4 = OP_GREATER;
    } else if (strcmp(op4_lexeme, "<=")) {
        op4 = OP_LESS_EQ;
    } else if (strcmp(op4_lexeme, ">=")) {
        op4 = OP_GREATER_EQ;
    } else {
        printf("invalid op4: %s", op4_lexeme);
        exit(-1);
    }

    snode->expression.op4 = op4;
    return snode;
}

ASTNode* ast_make_binary_simple_expression(ASTNode* term1, const char* op3_lexeme, ASTNode* term2) {
    ASTNode* snode = alloc_node(SE_BINARY_EXPR);
    snode->simple_expression.term1 = term1;
    snode->simple_expression.term2 = term2;
    
    // figure out op type
    OperationType op3;
    if (strcmp(op3_lexeme, "+")) {
        op3 = OP_PLUS;
    } else if(strcmp(op3_lexeme, "-")) {
        op3 = OP_MINUS;
    }

    snode->simple_expression.op3 = op3;
    return snode;
}

ASTNode* ast_make_unary_simple_expression(ASTNode* term1) {
    ASTNode* snode = alloc_node(SE_TERM);
    snode->simple_expression.term1 = term1;
    return snode;
}

ASTNode* ast_make_binary_term(ASTNode* factor1, const char* op2_lexeme, ASTNode* factor2) {
    ASTNode* snode = alloc_node(TT_BINARY);
    snode->term.factor1 = factor1;
    snode->term.factor2 = factor2;

    OperationType op2;
    if (strcmp(op2_lexeme, "*")) {
        op2 = OP_MUL;
    } else if(strcmp(op2_lexeme, "/")) {
        op2 = OP_DIV;
    } else if (strcmp(op2_lexeme, "%")) {
        op2 = OP_MOD;
    }
    snode->term.op2 = op2;
    return snode;
}

ASTNode* ast_make_unary_term(ASTNode* factor1) {
    ASTNode* snode = alloc_node(TT_FACTOR);
    snode->term.factor1 = factor1;
    return snode;
}

ASTNode* ast_make_factor_ident(const char* ident) {
    ASTNode* snode = alloc_node(FT_IDENT);
    snode->factor.ident = ident;
    return snode;
}

ASTNode* ast_make_factor_number(int number) {
    ASTNode* snode = alloc_node(FT_NUMBER);
    snode->factor.number = number;
    return snode;
}

ASTNode* ast_make_factor_boollit(bool b) {
    ASTNode* snode = alloc_node(FT_BOOLLIT);
    snode->factor.bool_lit = b;
    return snode;
}

ASTNode* ast_make_factor_parenth_expr(ASTNode* expr) {
    ASTNode* snode = alloc_node(FT_BOOLLIT);
    snode->factor.expr = expr;
    return snode;
}

#endif
