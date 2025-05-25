#include <stdio.h>
#include <stdlib.h>
#include "common.h"
#include "compiler.h"
#include "scanner.h"
#include "value.h"
#include "object.h"

#ifdef DEBUG_PRINT_CODE
    #include "debug.h"
#endif

/* Grammar Definition
     * program          -> declaration* EOF
     * declaration      -> classDeclaration | funcDeclaration | varDeclaration | statement | block;
     * classDeclaration -> "class" IDENTIFIER ( "<" IDENTIFIER )? "{" function* "}"
     * funcDeclaration  -> "fun" function
     * function         -> IDENTIFIER "(" parameters? ")" block
     * parameters       -> IDENTIFIER ( "," IDENTIFIER )*
     * varDeclaration   -> "var" IDENTIFIER ( "=" expression )? ";"
     * statement        -> exprStatement | block | ifStatement | whileStatement | forStatement | jumpStmt | returnStmt
     * ifStatement      -> "if" "(" expression ")" block ( "elseif" "(" expression ")" block )* ( "else" block ) ?
     * whileStatement   -> "while" "(" expression ")" block;
     * forStatement     -> "for" "(" ( varDeclaration | exprStatement | ";" ) expression? ";" expression? ")" block
     * jumpStmt         -> ( "break" | "continue" | "return" expression? ) ";"
     * returnStmt       -> "return" expression? ";"
     * block            -> "{" declaration* "}"
     * exprStatement    -> expression ";"
     * expression       -> assignment
     * assignment       -> (call "." )? IDENTIFIER "=" assignment | logical_or
     * ternary          -> logical_or ? ternary : ternary | logical_or
     * logical_or       -> logical_and ( "or" logical_and )*
     * logical_and      -> equality ( "and" equality )*
     * equality         -> comparison ( ("!=" | "==") comparison )*
     * comparison       -> term ( (">" | ">=" | "<" | "<=") term )*
     * term             -> factor ( ("-" | "+") factor )*
     * factor           -> unary ( ("*" | "/" | "%") unary )*
     * unary            -> ( "!" | "-" ) unary | builtInTypes
     * builtInTypes     -> array
     * array            -> "[" expression ( "," expression )* "]" ( "[" expression ( "," expression )* "]" )* | map
     * map              -> "{" expression ":" expression ( "," expression : expression )* "}" | call
     * call             -> anonFunction ( ( "(" arguments? ")" ) | ( "[" expression "]" ) | "." IDENTIFIER )*
     * arguments        -> expression ( "," expression )*
     * anonFunction     -> "fun" "(" parameters? ")" block | primary
     * primary          -> IDENTIFIER | "super" "." IDENTIFIER | "super" "(" arguments? ")" | NUMBER | STRING |
     * "true" | "false" | "nil" | "("  expression ")" | '"'ANY (${expression} )* ANY '"'
*/

     
typedef enum {
    PREC_NONE,
    PREC_ASSIGNMENT,
    PREC_OR,
    PREC_AND,
    PREC_EQUALITY,
    PREC_COMPARISON,
    PREC_TERM,
    PREC_FACTOR,
    PREC_UNARY,
    PREC_CALL,
    PREC_PRIMARY
} Precedence;

typedef struct {
    Token previous;
    Token current;
    bool hadError;
    bool panicMode;
} Parser;

typedef void (*ParseFn)();

typedef struct {
    ParseFn prefix;
    ParseFn infix;
    Precedence precedence;
} ParseRule;

static void expression();
static ParseRule* getRule(TokenType type);
static void parsePrecedence(Precedence precedence);


Parser parser;
Chunk* compilingChunk;

static Chunk* currentChunk() {
    return compilingChunk;
}

static void emitByte(uint8_t byte) {
    writeChunk(currentChunk(), byte, parser.previous.line);
}

static void emitBytes(uint8_t byte1, uint8_t byte2) {
    emitByte(byte1);
    emitByte(byte2);
}

static void emitConstant(Value value) {
    writeConstant(currentChunk(), value, parser.previous.line);
}

static void emitLongConstant(Value value) {
    writeLongConstant(currentChunk(), value, parser.previous.line);
    
}

static void endCompiler() {
    emitByte(OP_RETURN);
    #ifdef DEBUG_PRINT_CODE
        if (!parser.hadError) {
            disassembleChunk(currentChunk(), "code");
        }
    #endif
}


static void errorAt(Token* token, const char* message) {
    if (parser.panicMode) {
        return;
    }
    parser.panicMode = true;

    fprintf(stderr, "[line %d] Error", token-> line);

    if (token->type == TOKEN_EOF) {
        fprintf(stderr, " at end");
    } else if (token->type == TOKEN_ERROR) {

    } else {
        fprintf(stderr, " at '%.*s'", token->length, token->start);
    }

    fprintf(stderr, ": %s\n", message);
    parser.hadError = true;
}

static void advance() {
    parser.previous = parser.current;

    for(;;) {
        parser.current = scanToken();
        if (parser.current.type != TOKEN_ERROR) {
            break;
        }

        errorAt(&parser.current, parser.current.start);
    }
}

static void consume(TokenType type, const char* message) {
    if (parser.current.type == type) {
        advance();
        return;
    }

    errorAt(&parser.current, "");
}

static void literal() {
    int type = parser.previous.type;
    switch (parser.previous.type) {
        case TOKEN_TRUE:
            emitByte(OP_TRUE);
            break;
        case TOKEN_FALSE:
            emitByte(OP_FALSE);
            break;
        case TOKEN_NIL:
            emitByte(OP_NIL);
            break;
        default:
            break;
    }
}

static void string() {
    // +1 & -2 remove the start and end quotes
    emitConstant(TO_OBJ_VAL(copyString(parser.previous.start + 1, parser.previous.length - 2)));
}

static void number() {
    double value = strtod(parser.previous.start, NULL);
    if (value <= 255) { 
        emitConstant(TO_NUMBER_VAL(value));
    } else {
        emitLongConstant(TO_NUMBER_VAL(value));
    }
}

static void grouping() {
    expression();
    consume(TOKEN_RIGHT_PAREN, "Expecteced closing ')'.");
}

static void unary() {
    int type = parser.previous.type;
    parsePrecedence(PREC_UNARY); //match right
    switch (type) {
        case TOKEN_MINUS:
            emitByte(OP_NEGATE);
            break;
        case TOKEN_BANG:
            emitByte(OP_NOT);
            break;
        default:
            break;
    }
}

static void binary() {
    int operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence((Precedence)(rule->precedence + 1));
    switch (operatorType) {
        case TOKEN_PLUS: emitByte(OP_ADD); break;
        case TOKEN_MINUS: emitByte(OP_SUBTRACT); break;
        case TOKEN_STAR: emitByte(OP_MULTIPLY); break;
        case TOKEN_SLASH: emitByte(OP_DIVIDE); break;
        case TOKEN_AND: emitByte(OP_AND); break;
        case TOKEN_OR: emitByte(OP_OR); break;
        case TOKEN_EQUAL_EQUAL: emitByte(OP_EQUALS); break;
        case TOKEN_GREATER: emitByte(OP_GREATER); break;
        case TOKEN_LESS: emitByte(OP_LESS); break;
        case TOKEN_BANG_EQUAL: emitBytes(OP_EQUALS, OP_NOT); break;
        case TOKEN_GREATER_EQUAL:
            /*
            we could have avoid adding a new instruction for >= and <= and just reuse existing 
            instructions. since a <= b ==> !(a > b) and a >= b ==> !(a < b).
            but we hit an interesting edge case when comparing with NaN.
            according to IEEE754, NaN <= 1 = false and NaN > 1 is also false, so in our case,
            if we desugar NaN <= 1 to !(NaN > 1), we will get true which is not equivalent to NaN <= 1 (false) - according to IEEE754
            */
            emitByte(OP_GREATER_EQAULS);
            break;
        case TOKEN_LESS_EQUAL: emitByte(OP_LESS_EQUALS); break;
        default:
            break;
    }
}

static void ternary() {
    int operatorType = parser.previous.type;
    ParseRule* rule = getRule(operatorType);
    parsePrecedence(rule->precedence);
    consume(TOKEN_COLON, "Expected ':' for falseValue expression in ternary operator.");
    parsePrecedence(rule->precedence);
    emitByte(OP_TERNARY);
}

ParseRule rules[] = {
    [TOKEN_LEFT_PAREN] = {grouping, NULL, PREC_NONE},
    [TOKEN_RIGHT_PAREN] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_LEFT_SQUARE_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_RIGHT_SQUARE_BRACE] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMA] = {NULL, NULL, PREC_NONE},
    [TOKEN_DOT] = {NULL, NULL, PREC_NONE},
    [TOKEN_MINUS] = {unary, binary, PREC_TERM},
    [TOKEN_PLUS] = {NULL, binary, PREC_TERM},
    [TOKEN_SEMICOLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_COLON] = {NULL, NULL, PREC_NONE},
    [TOKEN_SLASH] = {NULL, binary, PREC_FACTOR},
    [TOKEN_STAR] = {NULL, binary, PREC_FACTOR},
    [TOKEN_MODULO] = {NULL, binary, PREC_FACTOR},
    [TOKEN_QUESTION_MARK] = {NULL, ternary, PREC_ASSIGNMENT},
    [TOKEN_BANG] = {unary, NULL, PREC_NONE},
    [TOKEN_BANG_EQUAL] = {NULL, NULL, PREC_EQUALITY},
    [TOKEN_EQUAL] = {NULL, NULL, PREC_NONE},
    [TOKEN_EQUAL_EQUAL] = {NULL, binary, PREC_EQUALITY},
    [TOKEN_GREATER] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_GREATER_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_LESS_EQUAL] = {NULL, binary, PREC_COMPARISON},
    [TOKEN_IDENTIFIER] = {NULL, NULL, PREC_NONE},
    [TOKEN_STRING] = {string, NULL, PREC_NONE},
    [TOKEN_NUMBER] = {number, NULL, PREC_NONE},
    [TOKEN_AND] = {NULL, binary, PREC_AND},
    [TOKEN_CLASS] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSE] = {NULL, NULL, PREC_NONE},
    [TOKEN_FUN] = {NULL, NULL, PREC_NONE},
    [TOKEN_FOR] = {NULL, NULL, PREC_NONE},
    [TOKEN_IF] = {NULL, NULL, PREC_NONE},
    [TOKEN_ELSEIF] = {NULL, NULL, PREC_NONE},
    [TOKEN_NIL] = {literal, NULL, PREC_NONE},
    [TOKEN_OR] = {NULL, binary, PREC_OR},
    [TOKEN_RETURN] = {NULL, NULL, PREC_NONE},
    [TOKEN_SUPER] = {NULL, NULL, PREC_NONE},
    [TOKEN_THIS] = {NULL, NULL, PREC_NONE},
    [TOKEN_TRUE] = {literal, NULL, PREC_NONE},
    [TOKEN_FALSE] = {literal, NULL, PREC_NONE},
    [TOKEN_VAR] = {NULL, NULL, PREC_NONE},
    [TOKEN_WHILE] = {NULL, NULL, PREC_NONE},
    [TOKEN_BREAK] = {NULL, NULL, PREC_NONE},
    [TOKEN_CONTINUE] = {NULL, NULL, PREC_NONE},
    [TOKEN_PRINT] = {NULL, NULL, PREC_NONE},
    [TOKEN_COMMENT] = {NULL, NULL, PREC_NONE},
    [TOKEN_ERROR] = {NULL, NULL, PREC_NONE},
    [TOKEN_EOF] = {NULL, NULL, PREC_NONE}
};

static ParseRule* getRule(TokenType type) {
    return &rules[type];
}

static void parsePrecedence(Precedence precedence) {
    advance();
    ParseRule* prefixRule = getRule(parser.previous.type);

    printf("token type: %d\n", parser.previous.type);
    printf("unary token: %.*s\n", parser.previous.length, parser.previous.start);
    if (prefixRule->prefix == NULL) {
        errorAt(&parser.previous, "Expect expression.");
        return;
    }

    prefixRule->prefix();

    while (precedence <= getRule(parser.current.type)->precedence) {
        advance();
        ParseFn infixRule = getRule(parser.previous.type)->infix;
        infixRule();
    }
}

static void expression() {
    parsePrecedence(PREC_ASSIGNMENT);
}

bool compile(Chunk* chunk, const char* source){
    parser.hadError = false;
    parser.panicMode = false;
    compilingChunk = chunk;

    initScanner(source);
    advance();
    expression();
    consume(TOKEN_EOF, "Expected end of expression.");

    endCompiler(&parser, chunk);
    return !parser.hadError;
}