/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode *stmt_sequence(void);

static TreeNode *statement(void);

static TreeNode *if_stmt(void);

static TreeNode *repeat_stmt(void);

static TreeNode *assign_stmt(void);

static TreeNode *read_stmt(void);

static TreeNode *write_stmt(void);

static TreeNode *exp(void);

static TreeNode *simple_exp(void);

static TreeNode *term(void);

static TreeNode *factor(void);

static TreeNode *while_stmt(void);

static TreeNode *return_stmt(void);

static TreeNode *var_stmt(void);

static TreeNode *func_stmt(void);

static TreeNode *lambda_exp(void);

static TreeNode *for_stmt(void);

static TreeNode *var_list(int explicit_dim);

static TreeNode *value_exp(void);

static TreeNode *multi_value_exp(void);

static TreeNode *dim_exp(int explicit_dim);

static TreeNode *params(void);

static TreeNode *call_stmt(char *name);

static void syntaxError(char *message) {
    fprintf(listing, "\n>>> ");
    fprintf(listing, "Syntax error at line %d: %s", lineno, message);
    Error = TRUE;
}

static void match(TokenType expected) {
    if (token == expected) token = getToken();
    else {
        syntaxError("match:: unexpected token -> ");
        printToken(token, tokenString);
        fprintf(listing, "expected: \n");
        printToken(expected, "");
        fprintf(listing, "      ");
    }
}

TreeNode *stmt_sequence(void) {
    TreeNode *t = statement();
    TreeNode *p = t;
    // if(token == SEMI) match(SEMI);
    while ((token != ENDFILE) && (token != END) &&
           (token != ELSE) && (token != UNTIL)) {
        TreeNode *q;
        if (token == SEMI) match(SEMI);
        if ((token == ENDFILE) || (token == END) ||
            (token == ELSE) || (token == UNTIL))
            break;
        q = statement();
        if (q != NULL) {
            if (t == NULL) t = p = q;
            else /* now p cannot be NULL either */
            {
                p->sibling = q;
                p = q;
            }
        }
    }
    return t;
}

TreeNode *statement(void) {
    TreeNode *t = NULL;
    switch (token) {
        case IF :
            t = if_stmt();
            break;
        case REPEAT :
            t = repeat_stmt();
            break;
        case ID :
            t = assign_stmt();
            break;
        case READ :
            t = read_stmt();
            break;
        case WRITE :
            t = write_stmt();
            break;
        case FUNC:
            t = func_stmt();
            break;
        case VAR:
            t = var_stmt();
            break;
        case WHILE:
            t = while_stmt();
            break;
        case FOR:
            t = for_stmt();
            break;
        case RETURN:
            t = return_stmt();
            break;
        case END:
        case ENDFILE:
            break;
        default :
            syntaxError("statement:: unexpected token -> ");
            printToken(token, tokenString);
            token = getToken();
            break;
    } /* end case */
    return t;
}

TreeNode *if_stmt(void) {
    TreeNode *t = newStmtNode(IfK);
    match(IF);
    if (t != NULL) t->child[0] = exp();
    match(THEN);
    if (t != NULL) t->child[1] = stmt_sequence();
    if (token == ELSE) {
        match(ELSE);
        if (t != NULL) t->child[2] = stmt_sequence();
    }
    match(END);
    return t;
}

TreeNode *repeat_stmt(void) {
    TreeNode *t = newStmtNode(RepeatK);
    match(REPEAT);
    if (t != NULL) t->child[0] = stmt_sequence();
    match(UNTIL);
    if (t != NULL) t->child[1] = exp();
    return t;
}

TreeNode *assign_stmt(void) {
    TreeNode *t = newStmtNode(AssignK);
    if ((t != NULL) && (token == ID))
        t->attr.name = copyString(tokenString);
    match(ID);
    if (token == LMBRACKET) {
        t->child[0] = dim_exp(1);
        if (token == ASSIGN) t->child[1] = value_exp();
    } else if (token == ASSIGN) {
        if (t != NULL) t->child[0] = value_exp();
    } else if (token == LPAREN) {
        t = call_stmt(t->attr.name);
    }

    return t;
}

TreeNode *read_stmt(void) {
    TreeNode *t = newStmtNode(ReadK);
    match(READ);
    if ((t != NULL) && (token == ID))
        t->attr.name = copyString(tokenString);
    match(ID);
    return t;
}

TreeNode *write_stmt(void) {
    TreeNode *t = newStmtNode(WriteK);
    match(WRITE);
    if (t != NULL) t->child[0] = exp();
    return t;
}

TreeNode *exp(void) {
    TreeNode *t = simple_exp();
    if ((token == LT) || (token == EQ) || (token == GT)) {
        TreeNode *p = newExpNode(OpK);
        if (p != NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
        }
        match(token);
        if (t != NULL)
            t->child[1] = simple_exp();
    }
    return t;
}

TreeNode *simple_exp(void) {
    TreeNode *t = term();
    while ((token == PLUS) || (token == MINUS) || (token == AND)) {
        TreeNode *p = newExpNode(OpK);
        if (p != NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
            match(token);
            t->child[1] = term();
        }
    }
    return t;
}

TreeNode *term(void) {
    TreeNode *t = factor();
    while ((token == TIMES) || (token == OVER)) {
        TreeNode *p = newExpNode(OpK);
        if (p != NULL) {
            p->child[0] = t;
            p->attr.op = token;
            t = p;
            match(token);
            p->child[1] = factor();
        }
    }
    return t;
}

TreeNode *factor(void) {
    TreeNode *t = NULL;
    switch (token) {
        case NUM :
            t = newExpNode(ConstK);
            if ((t != NULL) && (token == NUM))
                t->attr.val = atoi(tokenString);
            match(NUM);
            break;
        case ID :
            t = newExpNode(IdK);
            if ((t != NULL) && (token == ID))
                t->attr.name = copyString(tokenString);
            match(ID);
            if (token == LMBRACKET) {
                t->child[0] = dim_exp(1);
            } else if (token == LPAREN) t = call_stmt(t->attr.name);
            break;
        case LPAREN :
            match(LPAREN);
            t = exp();
            match(RPAREN);
            break;
        default:
            syntaxError("factor:: unexpected token -> ");
            printToken(token, tokenString);
            token = getToken();
            break;
    }
    return t;
}

TreeNode *var_stmt(void) {
    TreeNode *t = newStmtNode(VarK);
    match(VAR);
    t->child[0] = var_list(1);
    return t;
}

TreeNode *var_list(int explicit_dim) {
    TreeNode *root = NULL, *lst = NULL;
    while (token == ID) {
        TreeNode *p = newExpNode(IdK);
        if ((p != NULL) && (token == ID)) {
            p->attr.name = copyString(tokenString);
            match(ID);
            if (lst == NULL) root = p;
            else lst->sibling = p;
            lst = p;
        }
        if (token == ASSIGN) lst->child[0] = value_exp();
        else if (token == LMBRACKET) {
            lst->child[0] = dim_exp(explicit_dim);
            if (token == ASSIGN && explicit_dim) lst->child[1] = multi_value_exp();
        }
        if (token == COMMA) match(COMMA);
        else break;
    }
    return root;
}

TreeNode *value_exp(void) {
    TreeNode *q = newExpNode(ValueK);
    if (q != NULL) {
        match(ASSIGN);
        if (token == LAMBDA) q->child[0] = lambda_exp();
        else q->child[0] = exp();
    }
    return q;
}

TreeNode *multi_value_exp(void) {
    TreeNode *root = NULL, *lst = NULL;
    match(ASSIGN);
    match(LPAREN);
    while (1) {
        TreeNode *p = newExpNode(ValueK);
        if (p != NULL) p->child[0] = exp();
        if (lst == NULL) root = p;
        else lst->sibling = p;
        lst = p;
        if (token == COMMA) match(COMMA);
        else break;
    }
    match(RPAREN);
    return root;
}

TreeNode *dim_exp(int explicit_dim) {
    TreeNode *root = NULL, *lst = NULL;
    while (token == LMBRACKET) {
        match(LMBRACKET);
        TreeNode *p = newExpNode(DimK);
        if (p != NULL && explicit_dim) {
            if (token == ID) {
                TreeNode *t = newExpNode(IdK);
                if ((t != NULL) && (token == ID))
                    t->attr.name = copyString(tokenString);
                match(ID);
                p->child[0] = t;
            } else p->child[0] = simple_exp();
        }
        match(RMBRACKET);
        if (lst == NULL) root = p;
        else lst->sibling = p;
        lst = p;
    }
    return root;
}

TreeNode *while_stmt(void) {
    TreeNode *t = newStmtNode(WhileK);
    match(WHILE);
    match(LPAREN);
    if (t != NULL) {
        t->child[0] = exp();
    }
    match(RPAREN);
    if (t != NULL) {
        t->child[1] = stmt_sequence();
    }
    match(END);
    return t;
}

TreeNode *return_stmt(void) {
    TreeNode *t = newStmtNode(ReturnK);
    match(RETURN);
    if (t != NULL) t->child[0] = exp();
    return t;
}

TreeNode *func_stmt(void) {
    TreeNode *t = newStmtNode(FuncK);
    match(FUNC);
    if (t != NULL && token == ID) t->attr.name = copyString(tokenString), match(ID);
    if (t != NULL) t->child[0] = params(), t->child[1] = stmt_sequence();
    match(END);
    return t;
}

TreeNode *params(void) {
    TreeNode *t = newExpNode(ParamsK);
    match(LPAREN);
    t->child[0] = var_list(0);
    match(RPAREN);
    return t;
}

TreeNode *for_stmt(void) {
    TreeNode *t = newStmtNode(ForK);
    match(FOR);
    match(LPAREN);
    if (token == VAR) match(VAR);
    if (t != NULL) {
        t->child[0] = var_list(1);
        match(SEMI);
        t->child[1] = exp();
        match(SEMI);
        t->child[2] = assign_stmt();
        match(RPAREN);
        t->child[3] = stmt_sequence();
    }
    match(END);
    return t;
}

TreeNode *lambda_exp(void) {
    TreeNode *t = newStmtNode(FuncK);
    match(LAMBDA);
    if (t != NULL) t->attr.name = "lambda", t->child[0] = params(), match(COLON), t->child[1] = exp();
    return t;
}

TreeNode *call_stmt(char *name) {
    TreeNode *t = newStmtNode(CallK), *lst = NULL;
    if (t != NULL) t->attr.name = name;
    match(LPAREN);
    while (token != RPAREN) {
        TreeNode *p = exp();
        if (p != NULL) {
            if (lst == NULL) t->child[0] = p;
            else lst->sibling = p;
            lst = p;
        }
        if (token == COMMA) match(COMMA);
        else break;
    }
    match(RPAREN);
    return t;
}
/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode *parse(void) {
    TreeNode *t;
    token = getToken();
    t = stmt_sequence();
    if (token != ENDFILE)
        syntaxError("Code ends before file\n");
    return t;
}
