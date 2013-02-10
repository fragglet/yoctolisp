
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

typedef enum {
	YLISP_CELL, YLISP_STRING, YLISP_NUMBER, YLISP_BOOLEAN, YLISP_SYMBOL,
	YLISP_CONTEXT, YLISP_FUNCTION, YLISP_BUILTIN,
} YLispValueType;

enum {
	KWD_IF, KWD_COND, KWD_QUOTE, KWD_LAMBDA, KWD_LET, KWD_DEFINE,
	NUM_KEYWORDS
};

typedef struct _YLispValue *(*YLispBuiltin)(struct _YLispValue *args);

typedef struct _YLispValue {
	YLispValueType type;
	union {
		unsigned int i;
		char *s;
		struct {
			struct _YLispValue *cdr, *car;
		} cell;
		struct _YLispValue *symname;
		struct {
			struct _YLispValue *parent, *vars;
		} context;
		struct {
			struct _YLispValue *context, *code;
		} func;
		YLispBuiltin builtin;
	} v;
	struct _YLispValue *next;
} YLispValue;
#define CAR(val) ((val)->v.cell.car)
#define CDR(val) ((val)->v.cell.cdr)

typedef enum {
	TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN, TOKEN_LITERAL, TOKEN_QUOTE,
	TOKEN_ERROR, TOKEN_EOF
} YLispToken;

typedef struct {
	char *buffer;
	unsigned int position;
	YLispValue *value;
} YLispLexer;

static const char *keyword_names[] = {
	"if", "cond", "quote", "lambda", "let", "define"
};

static YLispValue *values = NULL;  // Linked list

static YLispValue *keywords[NUM_KEYWORDS];

// Symbol table; dynamically resizing.
static YLispValue **symbols = NULL;
static unsigned int num_symbols = 0;

static YLispValue *root_context;

static YLispValue *ylisp_value(YLispValueType type)
{
	YLispValue *result = calloc(1, sizeof(YLispValue));
	assert(result != NULL);
	result->type = type;
	result->next = values; values = result;
	return result;
}

static YLispValue *ylisp_number(YLispValueType type, unsigned int val)
{
	YLispValue *result = ylisp_value(type);
	result->v.i = val;
	return result;
}

static YLispValue *ylisp_cons(YLispValue *car, YLispValue *cdr)
{
	YLispValue *result = ylisp_value(YLISP_CELL);
	CAR(result) = car; CDR(result) = cdr;
	return result;
}

void ylisp_print(YLispValue *value);

static void print_list(YLispValue *value, char *inner)
{
	printf("(%s", inner);
	while (value != NULL) {
		ylisp_print(CAR(value));
		value = CDR(value);
		if (value != NULL) {
			printf(" ");
		}
	}
	printf(")");
}

void ylisp_print(YLispValue *value)
{
	if (value == NULL) {
		printf("()");
	} else switch (value->type) {
		case YLISP_CELL:
			print_list(value, "");
			break;
		case YLISP_STRING:
			printf("\"%s\"", value->v.s);
			break;
		case YLISP_NUMBER:
			printf("%i", value->v.i);
			break;
		case YLISP_BOOLEAN:
			printf("#%c", value->v.i ? 't' : 'f');
			break;
		case YLISP_SYMBOL:
			printf("%s", value->v.symname->v.s);
			break;
		case YLISP_CONTEXT:
			printf("<context>");
			break;
		case YLISP_FUNCTION:
			print_list(value->v.func.code, "lambda ");
			break;
		case YLISP_BUILTIN:
			printf("<builtin>");
			break;
	}
}

static YLispValue *string_from_data(const char *data, size_t data_len)
{
	YLispValue *value;
	char *s = malloc(data_len + 1); assert(s != NULL);
	memcpy(s, data, data_len); s[data_len] = '\0';

	value = ylisp_value(YLISP_STRING);
	value->v.s = s;
	return value;
}

YLispValue *ylisp_symbol_for_name(const char *name, size_t name_len)
{
	YLispValue *result, *symname;
	unsigned int i;

	for (i = 0; i < num_symbols; ++i) {
		symname = symbols[i]->v.symname;
		if (strlen(symname->v.s) == name_len
		 && memcmp(symname->v.s, name, name_len) == 0) {
			return symbols[i];
		}
	}

	result = ylisp_value(YLISP_SYMBOL);
	result->v.symname = string_from_data(name, name_len);

	symbols = realloc(symbols, sizeof(*symbols) * (num_symbols + 1));
	assert(symbols != NULL);
	return symbols[num_symbols++] = result;
}

void ylisp_init_lexer(YLispLexer *lexer, char *buf)
{
	lexer->buffer = buf;
	lexer->position = 0;
}

static int is_sym_char(char c)
{
	return !isspace(c) && strchr("()[]{}\",'`;#|\\", c) == NULL;
}

#define c lexer->buffer[lexer->position]

static YLispToken ylisp_read_string(YLispLexer *lexer)
{
	unsigned int start = lexer->position;

	while (c != '\0' && c != '"') {
		++lexer->position;
	}
	if (c == '\0')
		return TOKEN_ERROR;

	lexer->value = string_from_data(lexer->buffer + start,
	                                lexer->position - start);
	return TOKEN_LITERAL;
}

YLispToken ylisp_read_token(YLispLexer *lexer)
{
	while (c != '\0') {
		if (c == ';') {
			do {
				++lexer->position;
				if (c == '\0')
					return TOKEN_ERROR;
			} while (c != '\n');
		} else if (!isspace(c)) {
			break;
		}
		++lexer->position;
	}

	if (c == '\0')
		return TOKEN_EOF;

	switch (lexer->buffer[lexer->position++]) {
		case '(': return TOKEN_OPEN_PAREN;
		case ')': return TOKEN_CLOSE_PAREN;
		case '\'': return TOKEN_QUOTE;
		case '#':
			lexer->value = ylisp_value(YLISP_BOOLEAN);
			lexer->value->v.i = c == 't';
			if (c != 't' && c != 'f')
				return TOKEN_ERROR;
			++lexer->position;
			return TOKEN_LITERAL;
		case '"': return ylisp_read_string(lexer);
		default: --lexer->position; break;
	}

	if (isdigit(c)) {
		lexer->value = ylisp_value(YLISP_NUMBER);
		lexer->value->v.i = 0;
		while (c != '\0' && isdigit(c)) {
			lexer->value->v.i = lexer->value->v.i * 10 + (c - '0');
			++lexer->position;
		}
		return TOKEN_LITERAL;
	} else if (is_sym_char(c)) {
		unsigned int start = lexer->position;
		while (c != '\0' && is_sym_char(c))
			++lexer->position;
		lexer->value = ylisp_symbol_for_name(lexer->buffer + start,
		                                     lexer->position - start);
		return TOKEN_LITERAL;
	} else {
		return TOKEN_ERROR;
	}
}

#undef c

static YLispValue *parse_from_token(YLispLexer *lexer, YLispToken token);

static YLispValue *parse_list(YLispLexer *lexer)
{
	YLispValue *result, **rover=&result;

	for (;;) {
		YLispToken token = ylisp_read_token(lexer);
		if (token == TOKEN_EOF || token == TOKEN_ERROR)
			return NULL;
		if (token == TOKEN_CLOSE_PAREN)
			break;
		*rover = ylisp_value(YLISP_CELL);
		CAR(*rover) = parse_from_token(lexer, token);
		rover = &CDR(*rover);
	}

	*rover = NULL;  // end of list
	return result;
}

static YLispValue *parse_quoted(YLispLexer *lexer)
{
	YLispToken token = ylisp_read_token(lexer);

	return ylisp_cons(keywords[KWD_QUOTE],
	    ylisp_cons(parse_from_token(lexer, token), NULL));
}

static YLispValue *parse_from_token(YLispLexer *lexer, YLispToken token)
{
	switch (token) {
		case TOKEN_OPEN_PAREN:
			return parse_list(lexer);
		case TOKEN_QUOTE:
			return parse_quoted(lexer);
		case TOKEN_LITERAL:
			return lexer->value;
		case TOKEN_EOF:
		case TOKEN_ERROR:
		case TOKEN_CLOSE_PAREN:
			break;
	}
	return NULL;
}

YLispValue *ylisp_parse(char *input)
{
	YLispLexer lexer;
	YLispToken token;
	ylisp_init_lexer(&lexer, input);
	token = ylisp_read_token(&lexer);
	return parse_from_token(&lexer, token);
}

YLispValue *ylisp_eval(YLispValue *context, YLispValue *code);

static YLispValue *eval_variable(YLispValue *context, YLispValue *var)
{
	YLispValue *v;

	while (context != NULL) {
		for (v = context->v.context.vars; v != NULL; v = CDR(v)) {
			if (CAR(CAR(v)) == var) {
				return CDR(CAR(v));
			}
		}
		context = context->v.context.parent;
	}

	fprintf(stderr, "Undefined variable: %s\n",
	        var->v.symname->v.s);
	exit(-1);
}

static YLispValue *set_variable(YLispValue *context, YLispValue *name,
                                YLispValue *value)
{
	YLispValue *v;
	for (v = context->v.context.vars; v != NULL; v = CDR(v)) {
		if (CAR(CAR(v)) == name) {
			CDR(CAR(v)) = value;
			return value;
		}
	}
	context->v.context.vars = ylisp_cons(ylisp_cons(name, value),
	                                     context->v.context.vars);
	return value;
}

static YLispValue *run_function_body(YLispValue *context, YLispValue *code)
{
	YLispValue *result = NULL;
	while (code != NULL) {
		result = ylisp_eval(context, CAR(code));
		code = CDR(code);
	}
	return result;
}

static YLispValue *eval_func_call(YLispValue *context, YLispValue *code)
{
	YLispValue *func = ylisp_eval(context, CAR(code));

	if (func->type == YLISP_BUILTIN) {
		YLispValue *args = NULL, **a = &args, *c;
		for (c = CDR(code); c != NULL; c = CDR(c)) {
			*a = ylisp_value(YLISP_CELL);
			CAR(*a) = ylisp_eval(context, CAR(c));
			a = &CDR(*a);
		}
		*a = NULL;
		return func->v.builtin(args);
	} else if (func->type == YLISP_FUNCTION) {
		YLispValue *n, *c, *newcontext = ylisp_value(YLISP_CONTEXT);
		newcontext->v.context.parent = func->v.func.context;
		newcontext->v.context.vars = NULL;
		c = CDR(code);
		for (n = CAR(func->v.func.code); n != NULL; n = CDR(n)) {
			set_variable(newcontext, CAR(n),
			             ylisp_eval(context, CAR(c)));
			c = CDR(c);
		}
		return run_function_body(newcontext, CDR(func->v.func.code));
	} else {
		fprintf(stderr, "Invalid function call\n");
		exit(-1);
	}
}

static YLispValue *eval_list(YLispValue *context, YLispValue *code)
{
	YLispValue *first = CAR(code);

	// Special cases:
	if (first == keywords[KWD_QUOTE]) {
		return CAR(CDR(code));
	} else if (first == keywords[KWD_IF]) {
		YLispValue *val = ylisp_eval(context, CAR(CDR(code)));
		assert(val != NULL && (val->type == YLISP_NUMBER
		                    || val->type == YLISP_BOOLEAN));
		if (val->v.i) {
			return ylisp_eval(context, CAR(CDR(CDR(code))));
		} else if (CDR(CDR(CDR(code))) != NULL) {
			return ylisp_eval(context, CAR(CDR(CDR(CDR(code)))));
		}
	} else if (first == keywords[KWD_LAMBDA]) {
		YLispValue *result = ylisp_value(YLISP_FUNCTION);
		result->v.func.context = context;
		result->v.func.code = CDR(code);
		return result;
	} else if (first == keywords[KWD_DEFINE]) {
		return set_variable(root_context, CAR(CDR(code)),
		                    ylisp_eval(context, CAR(CDR(CDR(code)))));
	}

	return eval_func_call(context, code);
}

YLispValue *ylisp_eval(YLispValue *context, YLispValue *code)
{
	switch (code->type) {
		// Variable
		case YLISP_SYMBOL:
			return eval_variable(context, code);
		// Function call or other.
		case YLISP_CELL:
			return eval_list(context, code);
		// Literals.
		default:
			break;
	}
	return code;
}

static YLispValue *builtin_add(YLispValue *args)
{
	unsigned int result = 0;
	for (; args != NULL; args = CDR(args)) {
		result += CAR(args)->v.i;
	}
	return ylisp_number(YLISP_NUMBER, result);
}

static YLispValue *builtin_sub(YLispValue *args)
{
	unsigned int result = CAR(args)->v.i;
	for (args = CDR(args); args != NULL; args = CDR(args)) {
		result -= CAR(args)->v.i;
	}
	return ylisp_number(YLISP_NUMBER, result);
}

static YLispValue *builtin_mul(YLispValue *args)
{
	unsigned int result = 1;
	for (; args != NULL; args = CDR(args)) {
		result *= CAR(args)->v.i;
	}
	return ylisp_number(YLISP_NUMBER, result);
}

static YLispValue *builtin_div(YLispValue *args)
{
	unsigned int result = CAR(args)->v.i;
	for (args = CDR(args); args != NULL; args = CDR(args)) {
		result /= CAR(args)->v.i;
	}
	return ylisp_number(YLISP_NUMBER, result);
}

static YLispValue *builtin_eq(YLispValue *args)
{
	unsigned int expected = CAR(args)->v.i;
	unsigned int result = 1;
	for (args = CDR(args); args != NULL; args = CDR(args)) {
		if (CAR(args)->v.i != expected) {
			result = 0;
			break;
		}
	}
	return ylisp_number(YLISP_BOOLEAN, result);
}

static YLispValue *builtin_and(YLispValue *args)
{
	unsigned int result = 1;
	for (; args != NULL; args = CDR(args)) {
		if (!CAR(args)->v.i) {
			result = 0;
			break;
		}
	}
	return ylisp_number(YLISP_BOOLEAN, result);
}

static YLispValue *builtin_or(YLispValue *args)
{
	unsigned int result = 0;
	for (; args != NULL; args = CDR(args)) {
		if (CAR(args)->v.i) {
			result = 1;
			break;
		}
	}
	return ylisp_number(YLISP_BOOLEAN, result);
}

static YLispValue *builtin_car(YLispValue *args) { return CAR(CAR(args)); }
static YLispValue *builtin_cdr(YLispValue *args) { return CDR(CAR(args)); }

static YLispValue *builtin_cons(YLispValue *args)
{
	return ylisp_cons(CAR(args), CAR(CDR(args)));
}

static void define_builtin(char *name, YLispBuiltin callback)
{
	YLispValue *builtin = ylisp_value(YLISP_BUILTIN);
	builtin->v.builtin = callback;
	set_variable(root_context, ylisp_symbol_for_name(name, strlen(name)),
	             builtin);
}

void ylisp_init(void)
{
	unsigned int i;
	for (i = 0; i < NUM_KEYWORDS; ++i) {
		keywords[i] = ylisp_symbol_for_name(
		    keyword_names[i], strlen(keyword_names[i]));
	}
	root_context = ylisp_value(YLISP_CONTEXT);
	define_builtin("+", builtin_add);
	define_builtin("-", builtin_sub);
	define_builtin("*", builtin_mul);
	define_builtin("/", builtin_div);
	define_builtin("=", builtin_eq);
	define_builtin("and", builtin_and);
	define_builtin("or", builtin_or);
	define_builtin("car", builtin_car);
	define_builtin("cdr", builtin_cdr);
	define_builtin("cons", builtin_cons);
}

int main(int argc, char *argv[])
{
	YLispValue *code, *value;
	ylisp_init();
	code = ylisp_parse(argv[1]);
	value = ylisp_eval(root_context, code);
	ylisp_print(value);
	printf("\n");
	return 0;
}

