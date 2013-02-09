
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

typedef enum {
	TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN, TOKEN_SYMBOL, TOKEN_QUOTE,
	TOKEN_NUMBER, TOKEN_STRING, TOKEN_BOOLEAN, TOKEN_ERROR,
} YLispToken;

typedef struct {
	unsigned char *buffer;
	size_t buffer_len;
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

void ylisp_value_print(YLispValue *value)
{
	switch (value->type) {
		case YLISP_CELL: break;
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

void ylisp_init_lexer(YLispLexer *lexer, unsigned char *buf, size_t buf_len)
{
	lexer->buffer = buf;
	lexer->buffer_len = buf_len;
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
	unsigned char *s;

	while (lexer->position < lexer->buffer_len && c != '"') {
		++lexer->position;
	}
	if (lexer->position >= lexer->buffer_len)
		return TOKEN_ERROR;

	lexer->value = string_from_data(lexer->buffer + start,
	                                lexer->position - start);
	return TOKEN_STRING;
}

YLispToken ylisp_read_token(YLispLexer *lexer)
{
	while (lexer->position < lexer->buffer_len) {
		if (c == ';') {
			do {
				if (++lexer->position >= lexer->buffer_len)
					return TOKEN_ERROR;
			} while (c != '\n');
		} else if (!isspace(c)) {
			break;
		}
		++lexer->position;
	}

	if (lexer->position >= lexer->buffer_len)
		return TOKEN_ERROR;

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
		while (lexer->position < lexer->buffer_len && isdigit(c)) {
			lexer->value->v.i = lexer->value->v.i * 10 + (c - '0');
			++lexer->position;
		}
		return TOKEN_NUMBER;
	} else if (is_sym_char(c)) {
		unsigned int start = lexer->position;
		while (lexer->position < lexer->buffer_len && is_sym_char(c))
			++lexer->position;
		lexer->value = ylisp_symbol_for_name(lexer->buffer + start,
		                                     lexer->position - start);
		return TOKEN_SYMBOL;
	} else {
		return TOKEN_ERROR;
	}
}

#undef c

int main(int argc, char *argv[])
{
	YLispLexer lexer;
	YLispToken token;
	ylisp_init_lexer(&lexer, argv[1], strlen(argv[1]));
	while ((token = ylisp_read_token(&lexer)) != TOKEN_ERROR) {
		switch (token) {
			case TOKEN_OPEN_PAREN: printf("("); break;
			case TOKEN_CLOSE_PAREN: printf(")"); break;
			case TOKEN_QUOTE: printf("'"); break;
			case TOKEN_NUMBER:
			case TOKEN_STRING:
			case TOKEN_BOOLEAN:
			case TOKEN_SYMBOL:
				ylisp_value_print(lexer.value);
				break;
		}
		printf("\n");
	}
}

