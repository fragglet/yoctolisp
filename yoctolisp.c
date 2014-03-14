/*

Copyright (c) 2013 Simon Howard

Permission to use, copy, modify, and/or distribute this software
for any purpose with or without fee is hereby granted, provided
that the above copyright notice and this permission notice appear
in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL
WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE
AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR
CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM
LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT,
NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN
CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

 */

// Yoctolisp: a miniature lisp interpreter written in a weekend.

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

typedef enum {
	YLISP_CELL, YLISP_STRING, YLISP_NUMBER, YLISP_BOOLEAN, YLISP_SYMBOL,
	YLISP_CONTEXT, YLISP_FUNCTION, YLISP_BUILTIN, YLISP_MOVED
} YLispValueType;

enum {
	KWD_IF, KWD_COND, KWD_QUOTE, KWD_LAMBDA, KWD_LET, KWD_DEFINE,
	KWD_BEGIN, NUM_KEYWORDS
};

typedef struct _YLispValue *(*YLispBuiltin)(struct _YLispValue *args);

typedef struct _YLispValue {
	unsigned int type;
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
		struct _YLispValue *newlocation;
		YLispBuiltin builtin;
	} v;
} YLispValue;
#define CAR(val) ((val)->v.cell.car)
#define CDR(val) ((val)->v.cell.cdr)

typedef enum {
	TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN, TOKEN_LITERAL, TOKEN_QUOTE,
	TOKEN_ERROR, TOKEN_EOF, TOKEN_PERIOD
} YLispToken;

typedef struct {
	char *buffer;
	unsigned int position;
	YLispValue *value;
} YLispLexer;

typedef struct _GCPinnedVariable {
	YLispValue **variable;
	struct _GCPinnedVariable *next;
} GCPinnedVariable;

static const char *keyword_names[] = {
	"if", "cond", "quote", "lambda", "let", "define", "begin"
};

static void run_gc(void);

static YLispValue *zone;  // Array of YLispValue structs
static int zone_allocated, zone_length;
static int last_heap_size = 0;

static YLispValue *keywords[NUM_KEYWORDS];

// Symbol table; dynamically resizing.
static YLispValue **symbols = NULL;
static unsigned int num_symbols = 0;

static YLispValue **top_of_stack;
static YLispValue *root_context;

// A deferred call, passed up the stack (only one ever exists)
static YLispValue deferred_call;

static void init_zone(int size)
{
	zone_allocated = 1;
	zone_length = size;  // initial default
	zone = calloc(sizeof(YLispValue), zone_length);
	assert(zone != NULL);
}

static YLispValue *ylisp_value(YLispValueType type)
{
	YLispValue *result;
	if (zone_allocated >= zone_length)
		run_gc();
	result = &zone[zone_allocated++];
	memset(result, 0, sizeof(YLispValue));
	result->type = type;
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
		if (value->type != YLISP_CELL) {
			printf(". ");
			ylisp_print(value);
			break;
		}
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

static int ylisp_equal(YLispValue *v1, YLispValue *v2)
{
	if (v1 == NULL || v2 == NULL) {
		return v1 == v2;
	}
	if (v1->type != v2->type) {
		return 0;
	}
	switch (v1->type) {
		case YLISP_STRING:
			return !strcmp(v1->v.s, v2->v.s);
		case YLISP_NUMBER:
		case YLISP_BOOLEAN:
			return v1->v.i == v2->v.i;
		default:
			return v1 == v2;
	}
}

static int ylisp_lt(YLispValue *v1, YLispValue *v2)
{
	if (v1 == NULL || v2 == NULL || v1->type != v2->type) {
		return 0;
	}
	switch (v1->type) {
		case YLISP_STRING:
			return strcmp(v1->v.s, v2->v.s) < 0;
		case YLISP_NUMBER:
		case YLISP_BOOLEAN:
			return v1->v.i < v2->v.i;
		default:
			return 0;
	}
}

static void copy_value(YLispValue **value)
{
	YLispValue *newvalue;
	if (*value == NULL) {
		return;
	}
	// Already moved to new space?
	if ((*value)->type == YLISP_MOVED) {
		*value = (*value)->v.newlocation;
		return;
	}
	// Copy to new space and overwrite original
	assert(zone_allocated < zone_length);
	newvalue = &zone[zone_allocated++];
	*newvalue = **value;
	(*value)->type = YLISP_MOVED;
	(*value)->v.newlocation = newvalue;
	*value = newvalue;

	switch (newvalue->type) {
		case YLISP_CELL:
			// CDR first to keep lists contiguous in memory:
			copy_value(&newvalue->v.cell.cdr);
			copy_value(&newvalue->v.cell.car);
			break;
		case YLISP_SYMBOL:
			copy_value(&newvalue->v.symname);
			break;
		case YLISP_CONTEXT:
			copy_value(&newvalue->v.context.parent);
			copy_value(&newvalue->v.context.vars);
			break;
		case YLISP_FUNCTION:
			copy_value(&newvalue->v.func.context);
			copy_value(&newvalue->v.func.code);
			break;
		default: break;
	}
}

static void copy_roots(YLispValue *oldzone, unsigned int oldzone_size)
{
	YLispValue **ptr;
	YLispValue *marker;
	unsigned int i;
	for (i = 0; i < NUM_KEYWORDS; ++i) {
		copy_value(&keywords[i]);
	}
	// Stack is littered with YLispValue pointers. Walk up it and
	// fix/mark them. This is deep evil magic.
	if (&marker < top_of_stack) {
		for (ptr = &marker; ptr < top_of_stack; ++ptr) {
			if (*ptr > oldzone && *ptr < oldzone + oldzone_size) {
				copy_value(ptr);
			}
		}
	} else {
		for (ptr = &marker; ptr > top_of_stack; --ptr) {
			if (*ptr > oldzone && *ptr < oldzone + oldzone_size) {
				copy_value(ptr);
			}
		}
	}
	copy_value(&root_context);
}

static void gc_fixup_symbols(void)
{
	unsigned int i, new_length;
	// Some of the symbols in symbols[] may have been garbage collected.
	// Fix symbol pointers and drop any symbols not moved during GC.
	for (i = 0, new_length = 0; i < num_symbols; ++i) {
		if (symbols[i]->type == YLISP_MOVED) {
			symbols[new_length++] = symbols[i]->v.newlocation;
		}
	}
	num_symbols = new_length;
}

static void free_old_zone(YLispValue *oldzone, unsigned int oldzone_size)
{
	unsigned int i;
	// Most values that have been copied over to the new space have
	// been set to YLISP_MOVED; what's left is garbage.
	for (i = 0; i < oldzone_size; ++i) {
		switch (oldzone[i].type) {
			case YLISP_STRING:
				free(oldzone[i].v.s);
				break;
			default:
				break;
		}
	}

	free(oldzone);
}

static void run_gc(void)
{
	YLispValue *oldzone = zone;
	unsigned int oldzone_size = zone_length, newzone_size;
	oldzone = zone; oldzone_size = zone_length;
	// Calculate new zone size. We need at least as much space for
	// the current zone.
	newzone_size = last_heap_size * 2;
	if (newzone_size < zone_allocated + 1)
		newzone_size = zone_allocated * 2;
	init_zone(newzone_size);   // start a new zone
	copy_roots(oldzone, oldzone_size);
	gc_fixup_symbols();
	free_old_zone(oldzone, oldzone_size);
	//printf("GC: Post-GC heap size: %i cells\n", zone_allocated);
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
	YLispValue *result, *symname, *symstr;
	unsigned int i;

	for (i = 0; i < num_symbols; ++i) {
		symname = symbols[i]->v.symname;
		if (strlen(symname->v.s) == name_len
		 && memcmp(symname->v.s, name, name_len) == 0) {
			return symbols[i];
		}
	}

	symstr = string_from_data(name, name_len);
	result = ylisp_value(YLISP_SYMBOL);
	result->v.symname = symstr;

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
	++lexer->position;
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
		case '.': return TOKEN_PERIOD;
		case '(': return TOKEN_OPEN_PAREN;
		case ')': return TOKEN_CLOSE_PAREN;
		case '\'': return TOKEN_QUOTE;
		case '#':
			lexer->value = ylisp_number(YLISP_BOOLEAN, c == 't');
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
	YLispValue *result = NULL, *tail;

	for (;;) {
		YLispToken token = ylisp_read_token(lexer);
		YLispValue *newtail;

		if (token == TOKEN_EOF || token == TOKEN_ERROR)
			return NULL;
		if (token == TOKEN_CLOSE_PAREN)
			break;
		// (x y . z) syntax:
		if (token == TOKEN_PERIOD) {
			token = ylisp_read_token(lexer);
			newtail = parse_from_token(lexer, token);
			token = ylisp_read_token(lexer);
			if (token != TOKEN_CLOSE_PAREN)
				return NULL;
			CDR(tail) = newtail;
			return result;
		}
		newtail = ylisp_value(YLISP_CELL);
		CAR(newtail) = parse_from_token(lexer, token);
		CDR(newtail) = NULL;
		if (result == NULL) {
			result = newtail;
		} else {
			CDR(tail) = newtail;
		}
		tail = newtail;
	}

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
		default:
			break;
	}
	return NULL;
}

YLispValue *ylisp_parse(YLispLexer *lexer)
{
	YLispToken token = ylisp_read_token(lexer);
	return parse_from_token(lexer, token);
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
	abort();
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

static YLispValue *defer_eval(YLispValue *context, YLispValue *code)
{
	if (code->type != YLISP_CELL) {
		return ylisp_eval(context, code);
	}

	deferred_call.v.func.context = context;
	deferred_call.v.func.code = code;
	return &deferred_call;
}

static YLispValue *run_function_body(YLispValue *context, YLispValue *code)
{
	for (; code != NULL; code = CDR(code)) {
		if (CDR(code) == NULL) {
			return defer_eval(context, CAR(code));
		} else {
			ylisp_eval(context, CAR(code));
		}
	}
	return NULL;
}

static YLispValue *eval_func_args(YLispValue *context, YLispValue *code)
{
	YLispValue *args = NULL, *c, *tail = NULL;
	for (c = code; c != NULL; c = CDR(c)) {
		YLispValue *newtail = ylisp_value(YLISP_CELL);
		CAR(newtail) = ylisp_eval(context, CAR(c));
		if (tail == NULL) {
			args = newtail;
			CDR(newtail) = NULL;
		} else {
			CDR(tail) = newtail;
		}
		tail = newtail;
	}
	return args;
}

static YLispValue *eval_func_call(YLispValue *context, YLispValue *code)
{
	YLispValue *func = ylisp_eval(context, CAR(code));

	if (func->type == YLISP_BUILTIN) {
		YLispValue *result, *args = eval_func_args(context, CDR(code));
		result = func->v.builtin(args);
		return result;
	} else if (func->type == YLISP_FUNCTION) {
		YLispValue *n, *c, *result;
		YLispValue *newcontext = ylisp_value(YLISP_CONTEXT);
		newcontext->v.context.parent = func->v.func.context;
		newcontext->v.context.vars = NULL;
		c = CDR(code);
		for (n = CAR(func->v.func.code); n != NULL; n = CDR(n)) {
			// varargs:
			if (n->type == YLISP_SYMBOL) {
				set_variable(newcontext, n,
				             eval_func_args(context, c));
				break;
			}
			set_variable(newcontext, CAR(n),
			             ylisp_eval(context, CAR(c)));
			c = CDR(c);
		}
		result = run_function_body(newcontext, CDR(func->v.func.code));
		return result;
	} else {
		fprintf(stderr, "Invalid function call\n");
		abort();
	}
}

static YLispValue *eval_list_inner(YLispValue *context, YLispValue *code)
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
			return defer_eval(context, CAR(CDR(CDR(code))));
		} else if (CDR(CDR(CDR(code))) != NULL) {
			return defer_eval(context, CAR(CDR(CDR(CDR(code)))));
		}
	} else if (first == keywords[KWD_COND]) {
		YLispValue *tests, *val;
		for (tests = CDR(code); tests != NULL; tests = CDR(tests)) {
			val = ylisp_eval(context, CAR(CAR(tests)));
			assert(val != NULL && (val->type == YLISP_NUMBER
			                    || val->type == YLISP_BOOLEAN));
			if (val->v.i)
				return run_function_body(context,
				                         CDR(CAR(tests)));
		}
		return NULL;
	} else if (first == keywords[KWD_LAMBDA]) {
		YLispValue *result = ylisp_value(YLISP_FUNCTION);
		result->v.func.context = context;
		result->v.func.code = CDR(code);
		return result;
	} else if (first == keywords[KWD_DEFINE]) {
		// (define (func x) ...
		if (CAR(CDR(code))->type == YLISP_CELL) {
			YLispValue *func = ylisp_value(YLISP_FUNCTION);
			func->v.func.context = context;
			func->v.func.code = ylisp_cons(CDR(CAR(CDR(code))),
			                               CDR(CDR(code)));
			return set_variable(root_context, CAR(CAR(CDR(code))),
			                    func);
		} else {
			return set_variable(root_context, CAR(CDR(code)),
		                    ylisp_eval(context, CAR(CDR(CDR(code)))));
		}
	} else if (first == keywords[KWD_LET]) {
		YLispValue *newcontext = ylisp_value(YLISP_CONTEXT);
		YLispValue *vars, *result;
		newcontext->v.context.parent = context;
		for (vars = CAR(CDR(code)); vars != NULL; vars = CDR(vars)) {
			set_variable(newcontext, CAR(CAR(vars)),
			             ylisp_eval(newcontext,
			                        CAR(CDR(CAR(vars)))));
		}
		result = run_function_body(newcontext, CDR(CDR(code)));
		return result;
	} else if (first == keywords[KWD_BEGIN]) {
		return run_function_body(context, CDR(code));
	}

	return eval_func_call(context, code);
}

static YLispValue *eval_list(YLispValue *context, YLispValue *code)
{
	YLispValue *result = eval_list_inner(context, code);

	// Tail call optimization: expand deferred call.
	while (result == &deferred_call) {
		context = deferred_call.v.func.context;
		code = deferred_call.v.func.code;
		result = eval_list_inner(context, code);
	}

	return result;
}

YLispValue *ylisp_eval(YLispValue *context, YLispValue *code)
{
	switch (code->type) {
		case YLISP_SYMBOL:  // Variable
			return eval_variable(context, code);
		case YLISP_CELL:    // Function call or other.
			return eval_list(context, code);
		default:            // Literals.
			break;
	}
	return code;
}

static YLispValue *builtin_add(YLispValue *args)
{
	return ylisp_number(YLISP_NUMBER,
	                    CAR(args)->v.i + CAR(CDR(args))->v.i);
}

static YLispValue *builtin_sub(YLispValue *args)
{
	return ylisp_number(YLISP_NUMBER,
	                    CAR(args)->v.i - CAR(CDR(args))->v.i);
}

static YLispValue *builtin_mul(YLispValue *args)
{
	return ylisp_number(YLISP_NUMBER,
	                    CAR(args)->v.i * CAR(CDR(args))->v.i);
}

static YLispValue *builtin_div(YLispValue *args)
{
	return ylisp_number(YLISP_NUMBER,
	                    CAR(args)->v.i / CAR(CDR(args))->v.i);
}

static YLispValue *builtin_eq(YLispValue *args)
{
	return ylisp_number(YLISP_BOOLEAN,
	                    ylisp_equal(CAR(args), CAR(CDR(args))));
}

static YLispValue *builtin_lt(YLispValue *args)
{
	return ylisp_number(YLISP_BOOLEAN,
	                    ylisp_lt(CAR(args), CAR(CDR(args))));
}

static YLispValue *builtin_and(YLispValue *args)
{
	return ylisp_number(YLISP_BOOLEAN,
	                    CAR(args)->v.i && CAR(CDR(args))->v.i);
}

static YLispValue *builtin_or(YLispValue *args)
{
	return ylisp_number(YLISP_BOOLEAN,
	                    CAR(args)->v.i || CAR(CDR(args))->v.i);
}

static YLispValue *builtin_not(YLispValue *args)
{
	return  ylisp_number(YLISP_BOOLEAN, !CAR(args)->v.i);
}

static YLispValue *builtin_car(YLispValue *args) { return CAR(CAR(args)); }
static YLispValue *builtin_cdr(YLispValue *args) { return CDR(CAR(args)); }

static YLispValue *builtin_cons(YLispValue *args)
{
	return ylisp_cons(CAR(args), CAR(CDR(args)));
}

static YLispValue *builtin_read(YLispValue *args)
{
	// TODO: Make this not suck
	YLispLexer lexer;
	char buf[256];
	printf("> "); fflush(stdout);
	fgets(buf, sizeof(buf), stdin); buf[sizeof(buf) - 1] = '\0';
	ylisp_init_lexer(&lexer, buf);
	return ylisp_parse(&lexer);
}

static YLispValue *builtin_eval(YLispValue *args)
{
	return ylisp_eval(root_context, CAR(args));
}

static YLispValue *builtin_print(YLispValue *args)
{
	ylisp_print(CAR(args)); printf("\n");
	return NULL;
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
	init_zone(1024);  // initial size
	for (i = 0; i < NUM_KEYWORDS; ++i) {
		keywords[i] = ylisp_symbol_for_name(
		    keyword_names[i], strlen(keyword_names[i]));
	}
	root_context = ylisp_value(YLISP_CONTEXT);
	define_builtin("builtin-add", builtin_add);
	define_builtin("builtin-sub", builtin_sub);
	define_builtin("builtin-mul", builtin_mul);
	define_builtin("builtin-div", builtin_div);
	define_builtin("builtin-lt", builtin_lt);
	define_builtin("builtin-eq", builtin_eq);
	define_builtin("builtin-and", builtin_and);
	define_builtin("builtin-or", builtin_or);
	define_builtin("not", builtin_not);
	define_builtin("car", builtin_car);
	define_builtin("cdr", builtin_cdr);
	define_builtin("cons", builtin_cons);
	define_builtin("read", builtin_read);
	define_builtin("eval", builtin_eval);
	define_builtin("print", builtin_print);
}

static char *read_file(char *filename)
{
	char *result;
	unsigned int file_size;
	FILE *fs = fopen(filename, "r");
	if (fs == NULL) {
		perror("fopen");
		exit(-1);
	}
	fseek(fs, 0, SEEK_END);
	file_size = ftell(fs);
	fseek(fs, 0, SEEK_SET);
	result = malloc(file_size + 1); assert(result != NULL);
	fread(result, 1, file_size, fs);
	result[file_size] = '\0';
	return result;
}

static void process_file(char *filename)
{
	YLispValue *code = NULL;
	YLispLexer lexer;
	char *infile;

	infile = read_file(filename);
	ylisp_init_lexer(&lexer, infile);

	while ((code = ylisp_parse(&lexer)) != NULL) {
		//ylisp_print(code); printf("\n");
		ylisp_eval(root_context, code);
	}
}

int main(int argc, char *argv[])
{
	YLispValue *marker;

	if (argc < 2) {
		printf("Usage: %s <filename>\n", argv[0]);
		exit(-1);
	}

	top_of_stack = &marker;
	ylisp_init();

	process_file("stdlib.l");
	process_file(argv[1]);

	return 0;
}

