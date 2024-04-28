#ifndef AST_HEADER
#define AST_HEADER
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include "./uthash/include/uthash.h"



#ifndef DEBUG
    #define DEBUG 1
#endif

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
    IF_BLOCK,
    ELSE_CLAUSE,
    WHILE_BLOCK,
    WRITE_INT,
    AST_COUNT,
} ASTNodeType;

static const char* node_type_to_str(ASTNodeType ast_type){
    if (ast_type >= AST_COUNT) return "";
    static const char* ssss[] = {"FT_IDENT",
        "FT_NUMBER",
        "FT_BOOLLIT",
        "FT_PARENTH_EXPR",
        "TT_BINARY",
        "TT_FACTOR",
        "SE_TERM",
        "SE_BINARY_EXPR",
        "ET_SIMPLE",
        "ET_BINARY",
        "AT_EXPRESSION",
        "AT_READINT",
        "ST_ASSIGN",
        "ST_IFBLOCK",
        "ST_WHILEBLOCK",
        "ST_WRITEINT",
        "DT_DECLARATION",
        "TYPE",
        "STATEMENT_SEQUENCE",
        "STATEMENT",
        "IF_BLOCK",
        "ELSE_CLAUSE",
        "WHILE_BLOCK",
        "WRITE_INT"};
    return ssss[ast_type];
} 

typedef enum DataType {
    DT_INT,
    DT_BOOL,
} DataType;

static const char* datatype_as_str(DataType d) {
    static const char* ds_strs[] = {
        "int",
        "bool",
    };

    return ds_strs[d];
}

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

const char* op_to_str(OperationType o) {
    static const char* ops[] = 
    {
        "OP_PLUS",
        "OP_MINUS",
        "OP_MUL",
        "OP_DIV",
        "OP_MOD",
        "OP_EQ",
        "OP_NEQ",
        "OP_LESS",
        "OP_GREATER",
        "OP_LESS_EQ",
        "OP_GREATER_EQ",
    };

    return ops[o];
}

// convert from the OperationType to the corresponding character 
const char* op_to_symbol(OperationType o) {

    switch(o) {
        case OP_PLUS:
			return "+";
        case OP_MINUS:
			return "-";
        case OP_MUL:
			return "*";
        case OP_DIV:
			return "/";
        case OP_MOD:
			return "%";
        case OP_EQ:
			return "==";
        case OP_NEQ:
			return "!=";
        case OP_LESS:
			return "<";
        case OP_GREATER:
			return ">";
        case OP_LESS_EQ:
			return "<=";
        case OP_GREATER_EQ:
			return ">=";
    }

}

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
    if (!strcmp(op4_lexeme, "=")) {
        op4 = OP_EQ;
    } else if(!strcmp(op4_lexeme, "!=")) {
        op4 = OP_NEQ;
    } else if(!strcmp(op4_lexeme, "<")) {
        op4 = OP_LESS;
    } else if (!strcmp(op4_lexeme, ">")) {
        op4 = OP_GREATER;
    } else if (!strcmp(op4_lexeme, "<=")) {
        op4 = OP_LESS_EQ;
    } else if (!strcmp(op4_lexeme, ">=")) {
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
    if (!strcmp(op3_lexeme, "+")) {
        op3 = OP_PLUS;
    } else if(!strcmp(op3_lexeme, "-")) {
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
    if (!strcmp(op2_lexeme, "*")) {
        op2 = OP_MUL;
    } else if(!strcmp(op2_lexeme, "/")) {
        op2 = OP_DIV;
    } else if (!strcmp(op2_lexeme, "mod")) {
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



typedef struct _SymbolEntry {
    char symbol_name[32]; // hashtable key
    DataType data_type;
    UT_hash_handle hh; /// makes the structure hashable 
} SymbolEntry;

// global symbol table
SymbolEntry* g_symbols = 0;

// 
void sym_add(const char* symbol_name, DataType dt) {
    SymbolEntry* entry = (SymbolEntry*)malloc(sizeof(SymbolEntry));
    strcpy(entry->symbol_name, symbol_name);
    entry->data_type = dt;
    HASH_ADD_STR(g_symbols, symbol_name, entry);
}

SymbolEntry* sym_lookup(const char* key) {
    SymbolEntry *e = 0;
    HASH_FIND_STR(g_symbols, key, e);
    return e;
}


typedef struct TranspilerOutput {
    FILE* out_file;
    const char* out_file_path;

} TranspilerOutput;
 
TranspilerOutput tp_create(const char* out_file_path) {
    TranspilerOutput t;
    t.out_file = fopen(out_file_path, "w+");
    t.out_file_path = strdup(out_file_path);
    return t;
}

void tp_destroy(TranspilerOutput* t) {
    if (t->out_file) {
        fclose(t->out_file);
    }
    free((void*)t->out_file_path);
}


#if DEBUG == 0
#define TP_WRITE(TP, ...) \ 
{\ 
    fprintf(TP.out_file, __VA_ARGS__);\ 
}
#endif

#if DEBUG == 1
    #define TP_WRITE(TP, ...) {\ 
        fprintf(TP.out_file, __VA_ARGS__); \
        fflush(TP.out_file); \
        printf(__VA_ARGS__); \
    }
#endif

// support up to 31 tab depth
const char* tabs(int tab_depth) {
    char *t = malloc(32);
    memset(t, 0, 32);
    if (tab_depth < 32) {
        memset((void*)t, '\t', tab_depth);
    }
    return t;
}


char* fmt(const char* fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char* s = (char*)malloc(1024); 
    vsprintf(s, fmt, args);
    va_end(args);
    return s;
}

void ast_print_debug(ASTNode* s, int depth) {
    if (!s) return;
    printf("%s\n%s", node_type_to_str(s->kind), tabs(depth));
    switch(s->kind) {
		case FT_IDENT:
            printf("%s", (char*)s->factor.ident);
        	break;
		case FT_NUMBER:
            printf("%d", s->factor.number);
        	break;
		case FT_BOOLLIT:
            printf("%s", s->factor.bool_lit?"true":"false");
        	break;
		case FT_PARENTH_EXPR:
            ast_print_debug(s->factor.expr, depth+1);
        	break;
		case TT_BINARY:
            ast_print_debug(s->term.factor1, depth+1);
            printf(" OP2(%s)", op_to_symbol(s->term.op2)); 
            ast_print_debug( s->term.factor2, depth+1);
        	break;
		case TT_FACTOR:
            ast_print_debug( s->term.factor1, depth+1);
        	break;
		case SE_TERM:
            ast_print_debug( s->simple_expression.term1, depth+1);
        	break;
		case SE_BINARY_EXPR:
            ast_print_debug( s->simple_expression.term1, depth+1);
            printf(" OP3(%s)",  op_to_symbol(s->simple_expression.op3));
            ast_print_debug( s->simple_expression.term2, depth+1);
        	break;
		case ET_SIMPLE:
            ast_print_debug(s->expression.expr1, depth+1);
        	break;
		case ET_BINARY:
            ast_print_debug( s->expression.expr1, depth+1);
            printf(" OP4(%s)", op_to_symbol(s->expression.op4));
            ast_print_debug( s->expression.expr2, depth+1);
        	break;
		case AT_EXPRESSION:
            printf("ident(%s) \n", s->assignment.ident);
            ast_print_debug( s->assignment.expr, depth+1);
        	break;
		case AT_READINT:
            printf("ident(%s) \n", s->assignment.ident);
        	break;
		case ST_ASSIGN:
            ast_print_debug( s->statement.asgn, depth+1);
        	break;
		case ST_IFBLOCK:
            ast_print_debug( s->statement.ifblock, depth+1);
        	break;
		case ST_WHILEBLOCK:
            ast_print_debug(s->statement.while_block, depth+1);
        	break;
		case ST_WRITEINT:
            ast_print_debug( s->statement.writeInt, depth+1);
        	break;
		case IF_BLOCK:
            ast_print_debug( s->if_block.expr, depth+1); 
            ast_print_debug( s->if_block.statement_sequence, depth+1);
        	break;
		case ELSE_CLAUSE:
            ast_print_debug( s->else_block.statements, depth+1);
        	break;
		case WHILE_BLOCK:
            ast_print_debug( s->while_block.expr, depth+1); 
            ast_print_debug( s->while_block.statements, depth+1); 
        	break;
		case WRITE_INT:
            ast_print_debug( s->write_int.expr, depth+1);
        	break;
		case STATEMENT_SEQUENCE:
            ASTNode* current_statement = s;
            while(current_statement){
                ast_print_debug( current_statement->statement_sequence.statement, depth+1);
                current_statement = current_statement->statement_sequence.next;
            }
        break;
    }
}



char* gen(ASTNode* stmts, int tab_depth) {

    ASTNode* s = stmts;
    if (!s) return "";
    printf("generating kind: %s\n", node_type_to_str(s->kind));
    switch(s->kind) {
        case FT_IDENT:
            if (!sym_lookup(s->factor.ident)) {
                printf("Use of undeclared variable: %s", s->factor.ident);
                exit(-1);
            }
            return (char*)s->factor.ident;
        case FT_NUMBER:
            return fmt("%d", s->factor.number);
        case FT_BOOLLIT:
            return fmt("%s", s->factor.bool_lit?"true":"false");
        case FT_PARENTH_EXPR:
            return fmt("(%s)", gen(s->factor.expr, 0));
        case TT_BINARY:
            return fmt("%s %s %s", gen(s->term.factor1, 0), op_to_symbol(s->term.op2), gen( s->term.factor2, 0));
        case TT_FACTOR:
            return fmt("%s", gen( s->term.factor1, 0));
        case SE_TERM:
            return fmt("%s", gen( s->simple_expression.term1, 0));
        case SE_BINARY_EXPR:
            return fmt("%s %s %s",gen( s->simple_expression.term1, 0), op_to_symbol(s->simple_expression.op3), gen( s->simple_expression.term2, 0)); case ET_SIMPLE: return fmt("%s", gen(s->expression.expr1, 0));
        case ET_BINARY:
            return fmt("%s %s %s", gen( s->expression.expr1, 0), op_to_symbol(s->expression.op4), gen( s->expression.expr2, 0));
        case AT_EXPRESSION:
            if (!sym_lookup(s->assignment.ident)) {
                printf("Use of undeclared variable: %s", s->factor.ident);
                exit(-1);
            }
            return fmt("%s = %s;", s->assignment.ident, gen( s->assignment.expr, 0));
        case AT_READINT:
            SymbolEntry *e = 0; 
            if (!(e = sym_lookup(s->assignment.ident))) {
                printf("Use of undeclared variable: %s", s->factor.ident);
                exit(-1);
            } else {
                if (e->data_type != DT_INT) {
                    printf("readInt operand %s must be of type int, is currently: %s", s->factor.ident, datatype_as_str(e->data_type));
                    exit(-1);
                }
            }
            return fmt("scanf(\"%%d\", &%s);\n", s->assignment.ident);
        case ST_ASSIGN:
            return fmt("%s%s", tabs(tab_depth), gen(s->statement.asgn, tab_depth));
        case ST_IFBLOCK:
            return fmt("%s%s", tabs(tab_depth), gen( s->statement.ifblock, tab_depth));
        case ST_WHILEBLOCK:
            return fmt("%s%s", tabs(tab_depth), gen(s->statement.while_block, tab_depth));
        case ST_WRITEINT:
            return fmt("%s%s", tabs(tab_depth), gen( s->statement.writeInt, tab_depth));
        case IF_BLOCK:
            if (s->if_block.else_clause) {
                return fmt("if (%s) {\n%s%s}\n%selse {\n%s%s}", 
                          gen( s->if_block.expr, 0), 
                          gen( s->if_block.statement_sequence, tab_depth+1), 
                          tabs(tab_depth), 
                          tabs(tab_depth),
                          gen(s->if_block.else_clause, tab_depth), 
                          tabs(tab_depth));
            } else {
                return fmt("if (%s) {\n%s%s}\n", 
                          gen( s->if_block.expr, 0), 
                          gen( s->if_block.statement_sequence, tab_depth+1), 
                          tabs(tab_depth));
            }
        case ELSE_CLAUSE:
            return fmt("%s", gen( s->else_block.statements, tab_depth+1));
        case WHILE_BLOCK:
            return fmt("while (%s) {\n%s%s}", gen( s->while_block.expr, 0), gen( s->while_block.statements, tab_depth+1), tabs(tab_depth));
        case WRITE_INT:
            return fmt("printf(\"%%d\\n\", %s);", gen( s->write_int.expr, 0));
        case STATEMENT_SEQUENCE:
            char* f = (char*)"";
            ASTNode* current_statement = s;
            while(current_statement != 0){
                f = fmt("%s%s\n", f, gen( current_statement->statement_sequence.statement, tab_depth));
                current_statement = current_statement->statement_sequence.next;
            }
            return f;
        default: 
            return "unkn";
    }
}


int ast_generate_code(ASTNode* declarations, ASTNode* statement_sequence) {

#if 0
    printf("==============\n");
    ast_print_debug(statement_sequence, 0);
    return 1;
#endif 


    TranspilerOutput t = tp_create("test.c");

    TP_WRITE(t, "#include <stdio.h>\n");
    TP_WRITE(t, "typedef char bool;\n");

    TP_WRITE(t, "int main() {\n");

    // traverse declarations
    ASTNode* current_decl = declarations;
    while(current_decl) {
        sym_add(current_decl->declaration.ident, current_decl->declaration.datatype);
        TP_WRITE(t,"\t%s %s;\n", datatype_as_str(current_decl->declaration.datatype), current_decl->declaration.ident);
        current_decl = current_decl->declaration.next;
    }

    // test the hashtable 
    #if 0
    SymbolEntry* e;
    if ((e = sym_lookup("BIGGER"))) {
        printf("found symbol entry for %s: %s\n", e->symbol_name, datatype_as_str(e->data_type));
    }
    #endif

    // traverse statements
    char* statements = gen(statement_sequence, 1);
    TP_WRITE(t, "%s", statements);
    TP_WRITE(t, "}");

    return 1;
}
#endif
