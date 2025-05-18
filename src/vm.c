#include <stdlib.h>
#include <stdarg.h>
#include "common.h"
#include "vm.h"
#include "debug.h"
#include "compiler.h"

static Value readLongConstant(VM* vm) {
    #define READ_BYTE() (*vm->ip++)

    uint8_t v1 = READ_BYTE();
    uint8_t v2 = READ_BYTE();
    uint8_t v3 = READ_BYTE();

    int constantIndex = (v1 * 256 * 256) + (v2 * 256) + v3;

    return vm->chunk->constants.values[constantIndex];

    #undef READ_BYTE
}

static void resetStack(VM* vm) {
    vm->stackTop = vm->stack;
}

static void runtimeError(VM* vm, const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm->ip - vm->chunk->code - 1;
    int line = vm->chunk->lines.lines[instruction];
    fprintf(stderr, "[Line %d] in script\n", line);
    resetStack(vm);
}

static bool valuesEqual(Value a, Value b) {
    if (a.type != b.type) {
        return false;
    }
    switch (a.type) {
        case VAL_BOOL: return AS_C_BOOL(a) == AS_C_BOOL(b); break;
        case VAL_NUMBER: return AS_C_NUMBER(a) == AS_C_NUMBER(b); break;
        case VAL_NIL: return true;
        default: return false;
    }
}

static Value peek(VM* vm, int distance) {
    return vm->stackTop[-1 - distance];
}

static void set(VM* vm, int distance, Value value) {
    vm->stackTop[-1 - distance] = value;
}

/*
update the value of the pointer at position (-1 - distance) before,
and move the pointer to that position on the stack. moving the pointer backwards
like this simulates a stack pop operation.
*/
static void setAndMove(VM* vm, int distance, Value value) {
    vm->stackTop[-1 - distance] = value;
    vm->stackTop -= distance;
}

static bool isTruthy(Value value) {
    if (IS_BOOL(value)) {
        return AS_C_BOOL(value);
    }
    return value.type != VAL_NIL;
}

static void ternaryOp(VM* vm) {
    Value b = peek(vm, 0);
    Value a = peek(vm, 1);
    Value condition = peek(vm, 2);
    isTruthy(condition) > 0 ? setAndMove(vm, 2, a) : setAndMove(vm, 2, b);
}

static InterpretResult run(VM* vm) {
    #define READ_BYTE() (*vm->ip++)
    #define READ_CONSTANT() (vm->chunk->constants.values[READ_BYTE()])
    //This approach modifiy the stack top directly and only decrements it once
    #define BINARY_OP(vm, op, valueConverter) \
        do { \
            if (!IS_NUMBER(peek(vm, 0)) || !IS_NUMBER(peek(vm, 1))) {\
                runtimeError(vm, "Operands must be numbers.");\
                return INTERPRET_RUNTIME_ERROR;\
            }\
            double value = AS_C_NUMBER(peek(vm, 1)) op AS_C_NUMBER(peek(vm, 0));\
            setAndMove(vm, 1, valueConverter(value)); \
        } while (false)

    /* simple but inefficient. this approach increments and decrements the stackTop unnecessarily.
    #define BINARY_OP(vm, op) \
    do { \
        Value b = pop(vm); \
        Value a = pop(vm); \
        push(vm, a op b); \
    } while (false)
    */
        
    for(;;) {
        #ifdef DEBUG_TRACE_EXECUTION
            printf("     Program Stack ---->       ");
            printf("[");
            for (Value* slot = vm->stack; slot < vm->stackTop; slot++) {
                printValue(*slot);
                printf(", ");
            }
            printf("]\n");
            disassembleInstruction(vm->chunk, (int)(vm->ip - vm->chunk->code));
        #endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: push(vm, READ_CONSTANT()); break;
            case OP_CONSTANT_LONG: push(vm, readLongConstant(vm)); break;
            case OP_TRUE: push(vm, TO_BOOL_VAL(true)); break;
            case OP_FALSE: push(vm, TO_BOOL_VAL(false)); break;
            case OP_NIL: push(vm, TO_NIL_VAL); break;
            case OP_NEGATE: {
                // modify the stack directly instead of pop/push back.
                set(vm, 0, TO_NUMBER_VAL(-AS_C_NUMBER(peek(vm, 0))));
                break;
            }
            case OP_ADD: BINARY_OP(vm, +, TO_NUMBER_VAL); break;
            case OP_SUBTRACT: BINARY_OP(vm, -, TO_NUMBER_VAL); break;
            case OP_MULTIPLY: BINARY_OP(vm, *, TO_NUMBER_VAL); break;
            case OP_DIVIDE: BINARY_OP(vm, /, TO_NUMBER_VAL); break;
            case OP_TERNARY: ternaryOp(vm);break;
            case OP_NOT: set(vm, 0, TO_BOOL_VAL(!(isTruthy(peek(vm, 0))))); break;
            case OP_AND: {
                Value b = peek(vm, 0);
                Value a = peek(vm, 1);
                isTruthy(a) ? setAndMove(vm, 1, b) : setAndMove(vm, 1, a);
                break;
            }
            case OP_OR: {
                Value b = peek(vm, 0);
                Value a = peek(vm, 1);
                isTruthy(a) ? setAndMove(vm, 1, a) : setAndMove(vm, 1, b);
                break;
            }
            case OP_EQUALS: {
                Value b = peek(vm, 0);
                Value a = peek(vm, 1);
                setAndMove(vm, 1, TO_BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER: BINARY_OP(vm, >, TO_BOOL_VAL); break;
            case OP_LESS: BINARY_OP(vm, <, TO_BOOL_VAL); break;
            case OP_GREATER_EQAULS: BINARY_OP(vm, >=, TO_BOOL_VAL); break;
            case OP_LESS_EQUALS: BINARY_OP(vm, <=, TO_BOOL_VAL); break;
            case OP_RETURN: {
                printValue(pop(vm));
                printf("\n");
                return INTERPRET_OK;
            }
            default:
                break;
        }
    }

    #undef READ_BYTE
    #undef READ_CONSTANT
    #undef BINARY_OP
}

void initVM(VM* vm) {
    vm->chunk = NULL;
    vm->ip = NULL;
    resetStack(vm);
}

void freeVM(VM* vm) {
    
}

void push(VM* vm, Value value) {
    *vm->stackTop = value;
    vm->stackTop++;
    if (vm->stackTop - vm->stack > STACK_MAX){
        printf("VM Stack overflow.\n");
        exit(1);
    }   
}

Value pop(VM* vm) {
    vm->stackTop--;
    return *vm->stackTop;
}

InterpretResult interpret(VM* vm, char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(&chunk, source)) {
        printf("%s", "compile error");
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm->chunk = &chunk;
    vm->ip =  vm->chunk->code;

    InterpretResult result = run(vm);
    
    freeChunk(&chunk);

    return result;
}