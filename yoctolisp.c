
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <stdint.h>
#include <string.h>
#include <assert.h>

typedef enum {
	YLISP_CELL, YLISP_STRING, YLISP_NUMBER, YLISP_BOOLEAN, YLISP_SYMBOL,
} YLispValueType;

typedef struct _YLispValue {
	YLispValueType type;
	union {
		unsigned int i;
		unsigned char *s;
		struct {
			struct _YLispValue *cdr, *car;
		} cell;
		struct _YLispValue *symname;
	} v;
	struct _YLispValue *next;
} YLispValue;
#define CAR(val) ((val)->v.cell.car)
#define CDR(val) ((val)->v.cell.cdr)

typedef enum {
	TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN, TOKEN_SYMBOL, TOKEN_QUOTE,
	TOKEN_NUMBER, TOKEN_STRING, TOKEN_BOOLEAN, TOKEN_ERROR, TOKEN_EOF
} YLispToken;

typedef struct {
	unsigned char *buffer;
	unsigned int position;
	YLispValue *value;
} YLispLexer;

static YLispValue *values = NULL;  // Linked list

// Symbol table; dynamically resizing.
static YLispValue **symbols = NULL;
static unsigned int num_symbols = 0;

YLispValue *ylisp_value(YLispValueType type)
{
	YLispValue *result = malloc(sizeof(YLispValue));
	assert(result != NULL);
	result->type = type;
	result->next = values; values = result;
	return result;
}

static YLispValue *ylisp_cons(YLispValue *car, YLispValue *cdr)
{
	YLispValue *result = ylisp_value(YLISP_CELL);
	CAR(result) = car; CDR(result) = cdr;
	return result;
}

void ylisp_print(YLispValue *value);

static void print_list(YLispValue *value)
{
	printf("(");
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
		printf("nil");
	} else switch (value->type) {
		case YLISP_CELL:
			print_list(value);
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
	}
}

static YLispValue *string_from_data(unsigned char *data, size_t data_len)
{
	YLispValue *value;
	unsigned char *s = malloc(data_len + 1); assert(s != NULL);
	memcpy(s, data, data_len); s[data_len] = '\0';

	value = ylisp_value(YLISP_STRING);
	value->v.s = s;
	return value;
}

YLispValue *ylisp_symbol_for_name(unsigned char *name, size_t name_len)
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

void ylisp_init_lexer(YLispLexer *lexer, unsigned char *buf)
{
	lexer->buffer = buf;
	lexer->position = 0;
}

static int is_sym_char(unsigned char c)
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
	return TOKEN_STRING;
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
			return TOKEN_BOOLEAN;
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
		return TOKEN_NUMBER;
	} else if (is_sym_char(c)) {
		unsigned int start = lexer->position;
		while (c != '\0' && is_sym_char(c))
			++lexer->position;
		lexer->value = ylisp_symbol_for_name(lexer->buffer + start,
		                                     lexer->position - start);
		return TOKEN_SYMBOL;
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

	return ylisp_cons(ylisp_symbol_for_name("quote", 5),
	    ylisp_cons(parse_from_token(lexer, token), NULL));
}

static YLispValue *parse_from_token(YLispLexer *lexer, YLispToken token)
{
	switch (token) {
		case TOKEN_OPEN_PAREN:
			return parse_list(lexer);
		case TOKEN_QUOTE:
			return parse_quoted(lexer);
		case TOKEN_NUMBER:
		case TOKEN_STRING:
		case TOKEN_BOOLEAN:
		case TOKEN_SYMBOL:
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

int main(int argc, char *argv[])
{
	YLispValue *value = ylisp_parse(argv[1]);
	ylisp_print(value);
	printf("\n");
	return 0;
}

