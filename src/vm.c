#include <stdlib.h>
#include <stdarg.h>
#include "common.h"
#include "vm.h"
#include "debug.h"
#include "compiler.h"
#include "object.h"
#include "memory.h"

VM vm;

static Value readLongConstant() {
    #define READ_BYTE() (*vm.ip++)

    uint8_t v1 = READ_BYTE();
    uint8_t v2 = READ_BYTE();
    uint8_t v3 = READ_BYTE();

    int constantIndex = (v1 * 256 * 256) + (v2 * 256) + v3;

    return vm.chunk->constants.values[constantIndex];

    #undef READ_BYTE
}


static void resetStack() {
    vm.stackTop = vm.stack;
}

static void runtimeError(const char* format, ...) {
    va_list args;
    va_start(args, format);
    vfprintf(stderr, format, args);
    va_end(args);
    fputs("\n", stderr);

    size_t instruction = vm.ip - vm.chunk->code - 1;
    int line = vm.chunk->lines.lines[instruction];
    fprintf(stderr, "[Line %d] in script\n", line);
    resetStack(vm);
}

static Value peek(int distance) {
    return vm.stackTop[-1 - distance];
}

static void set(int distance, Value value) {
    vm.stackTop[-1 - distance] = value;
}

/*
update the value of the pointer at position (-1 - distance) before,
and move the pointer to that position on the stack. moving the pointer backwards
like this simulates a stack pop operation.
*/
static void setAndMove(int distance, Value value) {
    vm.stackTop[-1 - distance] = value;
    vm.stackTop -= distance;
}

static bool isTruthy(Value value) {
    if (IS_BOOL(value)) {
        return AS_C_BOOL(value);
    }
    return value.type != VAL_NIL;
}

static void ternaryOp() {
    Value falseValue = peek(0);
    Value trueValue = peek(1);
    Value condition = peek(2);
    isTruthy(condition) > 0 ? setAndMove(2, trueValue) : setAndMove(2, falseValue);
}

static void concatenate() {
    ObjString* b = AS_STRING(peek(0));
    ObjString* a = AS_STRING(peek(1));
    int length = a->length + b->length;
    char* heapChars = ALLOCATE(char, length + 1);
    memcpy(heapChars, a->chars, a->length);
    memcpy(heapChars + a->length, b->chars, b->length);
    heapChars[length] = '\0';
    ObjString* result = takeString(heapChars, length);
    setAndMove(1, TO_OBJ_VAL(result));
}

static InterpretResult run() {
    #define READ_BYTE() (*vm.ip++)
    #define READ_CONSTANT() (vm.chunk->constants.values[READ_BYTE()])
    #define READ_STRING()   (AS_STRING(READ_CONSTANT()))
    //This approach modifiy the stack top directly and only decrements it once
    #define BINARY_OP(op, valueConverter) \
        do { \
            if (!IS_NUMBER(peek(0)) || !IS_NUMBER(peek(1))) {\
                runtimeError("Operands must be numbers.");\
                return INTERPRET_RUNTIME_ERROR;\
            }\
            double value = AS_C_NUMBER(peek(1)) op AS_C_NUMBER(peek(0));\
            setAndMove(1, valueConverter(value)); \
        } while (false)

    /* simple but inefficient. this approach increments and decrements the stackTop unnecessarily.
    #define BINARY_OP(op) \
    do { \
        Value b = pop(vm); \
        Value a = pop(vm); \
        push(a op b); \
    } while (false)
    */
        
    for(;;) {
        #ifdef DEBUG_TRACE_EXECUTION
            printf("     Program Stack ---->       ");
            printf("[");
            for (Value* slot = vm.stack; slot < vm.stackTop; slot++) {
                printValue(*slot);
                printf(", ");
            }
            printf("]\n");
            disassembleInstruction(vm.chunk, (int)(vm.ip - vm.chunk->code));
        #endif
        uint8_t instruction;
        switch (instruction = READ_BYTE()) {
            case OP_CONSTANT: push(READ_CONSTANT()); break;
            case OP_CONSTANT_LONG: push(readLongConstant(vm)); break;
            case OP_TRUE: push(TO_BOOL_VAL(true)); break;
            case OP_FALSE: push(TO_BOOL_VAL(false)); break;
            case OP_NIL: push(NIL_VAL); break;
            case OP_NEGATE: {
                // modify the stack directly instead of pop/push back.
                set(0, TO_NUMBER_VAL(-AS_C_NUMBER(peek(0))));
                break;
            }
            case OP_ADD: {
                if (IS_STRING(peek(0)) && IS_STRING(peek(1))) {
                    concatenate(vm);
                } else {
                    BINARY_OP(+, TO_NUMBER_VAL);
                } 
                break;
            }
            case OP_SUBTRACT: BINARY_OP(-, TO_NUMBER_VAL); break;
            case OP_MULTIPLY: BINARY_OP(*, TO_NUMBER_VAL); break;
            case OP_DIVIDE: BINARY_OP(/, TO_NUMBER_VAL); break;
            case OP_TERNARY: ternaryOp(vm);break;
            case OP_NOT: set(0, TO_BOOL_VAL(!(isTruthy(peek(0))))); break;
            case OP_AND: {
                Value b = peek(0);
                Value a = peek(1);
                isTruthy(a) ? setAndMove(1, b) : setAndMove(1, a);
                break;
            }
            case OP_OR: {
                Value b = peek(0);
                Value a = peek(1);
                isTruthy(a) ? setAndMove(1, a) : setAndMove(1, b);
                break;
            }
            case OP_EQUALS: {
                Value b = peek(0);
                Value a = peek(1);
                setAndMove(1, TO_BOOL_VAL(valuesEqual(a, b)));
                break;
            }
            case OP_GREATER: BINARY_OP(>, TO_BOOL_VAL); break;
            case OP_LESS: BINARY_OP(<, TO_BOOL_VAL); break;
            case OP_GREATER_EQAULS: BINARY_OP(>=, TO_BOOL_VAL); break;
            case OP_LESS_EQUALS: BINARY_OP(<=, TO_BOOL_VAL); break;
            case OP_DEFINE_GLOBAL: {
                ObjString* varName =  READ_STRING();
                tableSet(&vm.globals, varName, peek(0));
                pop();
                break;
            }
            case OP_GET_GLOBAL: {
                ObjString* varName =  READ_STRING();
                Value value;
                if (!tableGet(&vm.globals, varName, &value)) {
                    runtimeError("Undefined variable '%s'.", varName->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                push(value);
                break;
            }
            case OP_SET_GLOBAL: {
                ObjString* name =  READ_STRING();
                if (tableSet(&vm.globals, name, peek(0))) {
                    tableDelete(&vm.globals, name);
                    runtimeError("Undeclared variable '%s'.", name->chars);
                    return INTERPRET_RUNTIME_ERROR;
                }
                break;
            }
            case OP_POP: pop(); break;
            case OP_PRINT: {
                printValue(pop());
                printf("\n");
                break;;
            }
            case OP_RETURN: {
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

void initVM() {
    vm.chunk = NULL;
    vm.ip = NULL;
    resetStack(vm);
    vm.objects = NULL;
    initTable(&vm.strings);
    initTable(&vm.globals);
}

void freeVM() {
    freeObjects();
}

void push(Value value) {
    *vm.stackTop = value;
    vm.stackTop++;
    if (vm.stackTop - vm.stack > STACK_MAX){
        printf("VM Stack overflow.\n");
        exit(1);
    }   
}

Value pop() {
    vm.stackTop--;
    return *vm.stackTop;
}

InterpretResult interpret(char* source) {
    Chunk chunk;
    initChunk(&chunk);

    if (!compile(&chunk, source)) {
        printf("%s", "compile error");
        freeChunk(&chunk);
        return INTERPRET_COMPILE_ERROR;
    }

    vm.chunk = &chunk;
    vm.ip =  vm.chunk->code;

    InterpretResult result = run(vm);
    
    freeChunk(&chunk);

    return result;
}

#undef READ_CONSTANT
#undef READ_STRING
#undef BINARY_OP