# Lisp in a weekend

Working as a professional software engineer can be tiring: you're often
working on a small part of a larger system, making small changes and
fixing bugs. Over time you also learn to rigidly follow best practises:
write code that is efficient, document your functions, always check
error codes, etc. It's easy to forget the fun of just programming and
the satisfaction of creating something.

I set out to try to reclaim some of that fun by setting myself a
challenge: implement a Lisp interpreter in a weekend. How much could I
achieve? I wasn't sure but I figured it should be achievable. The nice
thing about Lisp is the lack of syntax, meaning that parsing of the
program text isn't difficult. But there are still lots of other things
to take into account. I was also curious as to how small (in terms of
lines of code) a Lisp interpreter could be.

There are several languages I could have chosen to do this, and some
would have been easier than others. A Lisp interpreter in Python, for
example, would probably have been fairly trivial. In the end I opted
for C, just for the challenge - I'd have to implement my own garbage
collector rather than "cheating" by using a higher level language!

In the end, I was quite happy with the result. I started on the Friday
night and by the end of Sunday I had a complete, functioning
interpreter. It includes most of the
[staple Lisp features](http://www.paulgraham.com/diff.html): a
read-eval-print loop, first-class functions with lexical closures,
garbage collection and recursion with tail call optimization. The
source code is less than 900 lines, though that doesn't include a
separate file with a small "standard library" of common Lisp
functions.

I cut a lot of corners to save time. The code is much more sparsely
commented than my normal standard and error reporting is non-existent.
In spite of that, it's still fairly readable. There's very little in
the way of input checking: for example, syntax errors will often cause
the interpreter to crash due to a null pointer dereference.

Below is an annotated timeline of my experience, linking to the commits
I made over that weekend. I've made a few changes to the code since then
but in general it's largely unchanged.

I name the resulting language Yoctolisp (yocto- is the SI prefix for
10<sup>-24</sup>, and all the preceding "small" prefixes have already
been used). It's reminiscent of Scheme, mostly because Scheme is the
only dialect of Lisp that I know to any practical extent.

## Friday

Lisp programs are made up of S-expressions, so the first step to
writing an interpreter is to write some code to parse S-expressions.

### commit [ebaff9](https://github.com/fragglet/yoctolisp/commit/ebaff9)

#### S-expression lexing

The first step to writing a parser is to write a Lexer to tokenize the
input, so that's what I've done here. First we need some data
structures.
```cpp
	typedef enum {
		TOKEN_OPEN_PAREN, TOKEN_CLOSE_PAREN, TOKEN_SYMBOL, TOKEN_QUOTE,
		TOKEN_NUMBER, TOKEN_STRING, TOKEN_BOOLEAN, TOKEN_ERROR,
	} YLispToken;
```
These are the token types I start off by defining (these [change](#e86411)
a bit later). It's then relatively simple to hand-roll a lexer.
YLispLexer is a structure holding the current state of the lexer,
which is initialized by `ylisp_init_lexer`. Then
`ylisp_read_token` reads the next token from the input data. We can
tell the type of the token we've found by looking at the first
character we encounter - for example '#' means we've found a boolean
constant, and so on.

One thing where Lisp is quite different to other languages is in
symbol names. Languages like C only allow a very limited subset of
names for variables (alphanumeric and underscore). Scheme allows any
characters *except* for [a small
subset](http://docs.racket-lang.org/guide/symbols.html). I abuse C's
string functions to implement this in a compact way.
```cpp
	static int is_sym_char(unsigned char c)
	{
		return !isspace(c) && strchr("()[]{}\",'`;#|\\", c) == NULL;
	}
```
The lexing has a couple of big holes. Firstly, negative numbers aren't
supported. Secondly, strings don't support escape characters. A proper
lexer ought to do both these things.

#### Data structures

One thing that's needed already at this early stage is a data
structure to represent the various Lisp data types. These are:
integer, string, symbol, and cons cell. There were two possible ways I
could have approached this. One is to use a "base" and "child"
structures and cast between them, which would work like this:
```cpp
	typedef struct {
		...
	} YLispValue;
	
	typedef struct {
		YLispValue parent;
		char *s;
	} YLispString;
```
The other is to use a single union data structure to hold all the
different types. I debated both approaches, and ultimately went with
the latter. The former approach is undoubtedly a better approach, and
would be what I would probably use if I were trying to write "proper"
maintainable code. But it means having to constantly cast between
types and ultimately more complicated code. Here's the union-based
structure I went with.
```cpp
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
```
The "next" pointer is there for garbage collection. I defined
`ylisp_value` as a convenience function to allocate new `YLispValue`s of a
particular type:
```cpp
	YLispValue *ylisp_value(YLispValueType type)
	{
		YLispValue *result = malloc(sizeof(YLispValue));
		assert(result != NULL);
		result->type = type;
		result->next = values; values = result;
		return result;
	}
```
As well as allocating and initializing a new `YLispValue`, it also adds
the new value onto a linked list of allocated values. Later on I'll
use this for the sweep stage of the garbage collector.

One other thing: some will rightly criticize the use of a
heap-allocated structure just to hold a simple integer value. Proper
virtual machines use a trick called
[pointer tagging](http://nikic.github.com/2012/02/02/Pointer-magic-for-efficient-dynamic-value-representations.html)
that makes use of the fact that heap-allocated pointers are aligned on
word boundaries and therefore have their lower bits set to zero.
Again, for simplicity I'm avoiding special cases.

### commit [d53138](https://github.com/fragglet/yoctolisp/commit/d53138)

#### Parsing

Having written the lexer, the next stage is to actually parse
S-expressions. The function `ylisp_parse` is the entrypoint into
parsing: it takes a string as input and returns a parsed expression.
Parsing S-expressions is pretty easy: `parse_from_token` interprets a
new `YLispValue` from the lexer's input, branching off to `parse_list`
when an open parenthesis is found. This then recurses back to
`parse_from_token` to read the individual items in the list.
Special attention is needed when parsing the quote symbol. This needs
to be translated into a Lisp quote expression, for example:
```lisp
	'(1 2 3 4)
```
Becomes:
```lisp
	(quote (1 2 3 4))
```
#### Printing

To test out the parsing code, I wrote a simple "main" function that
would parse an S-expression given on the command line. But to test it
properly required some code to print out the parsed expression. I
already wrote some code in the previous commit to do some basic
printing of `YLispValue` structures, but expanded this into
`ylisp_print`, which will recursively print S-expressions. This
function becomes a lot more important later when I want a
[read-eval-print loop](http://en.wikipedia.org/wiki/Read%E2%80%93eval%E2%80%93print_loop).

### commit [1addbb](https://github.com/fragglet/yoctolisp/commit/1addbb)

In this commit I add some much-needed convenience functions and
macros. Firstly I add `CAR()` and `CDR()` for accessing those fields
in a cons cell. This proves essential later when evaluation uses
long chains of them. Defining them as preprocessor macros allows them
to be used as lvalues in assignments, which is a further convenience.

I also add `ylisp_cons` which performs the equivalent to Lisp's `cons`
function, allocating and assigning a new cons cell.

<a name="e86411">

### commit [e86411](https://github.com/fragglet/yoctolisp/commit/e86411)

When I first wrote the lexer, I defined separate token types for
symbols, numbers, strings and booleans. These can be collapsed down
into a single type, `TOKEN_LITERAL`, as the end result is the same
regardless - a literal value has been read.


## Saturday

Having a functioning S-expression parser to work with, on Saturday I
wrote the evaluation code. This was all contained in a single commit,
and probably should have been split across several commits. The end
result, though, was code capable of evaluating basic expressions.

### commit [a5f8f6](https://github.com/fragglet/yoctolisp/commit/a5f8f6)

#### Keywords

Certain symbols are interpreted by Lisp as "keywords" - examples are
`if`, `cond`, and `define`. It's helpful to be able to access the
symbols representing these keywords without having to do an expensive
lookup. I therefore define an enum of the keywords I want; these get
looked up on startup and stored in an array.
```cpp
	enum {
		KWD_IF, KWD_COND, KWD_QUOTE, KWD_LAMBDA, KWD_LET, KWD_DEFINE,
		NUM_KEYWORDS
	};

	static const char *keyword_names[] = {
		"if", "cond", "quote", "lambda", "let", "define"
	};
	
	static YLispValue *keywords[NUM_KEYWORDS];
```
It's then trivial to check for particular keywords, like on this line:
```cpp
	} else if (first == keywords[KWD_IF]) {
```
#### Variable contexts

In imperative languages like C, it's not possible to access the
variables of a function after it has returned. But in Lisp, it's
possible to create a function within a function that can access the
variables of its parent even after that parent has returned. For
example:
```lisp
	(define create-adder
	  (lambda (x)
	    (lambda (y)
	      (+ x y))))
	
	(define add-1 (create-adder 1))

	(print (add-1 2))
```
To achieve this, the variables of the parent function have to be
stored in a context - a heap object that is garbage collected. So I
define `YLISP_CONTEXT` as a new type of `YLispValue`, with these
fields:
```cpp
	struct {
		struct _YLispValue *parent, *vars;
	} context;
```
The `parent` field points to another context that is the parent of
this one (or `NULL` for the top-level context). More interesting is
`vars`, which stores a list of cons cells, one for each variable
in that context. The `car` is the symbol representing the variable
name, while the `cdr` is the value. So for example, if `a = 1` and
`b = "hello world"`, then `vars` is:
```lisp
	((a . 1) (b . "hello world"))
```
To look up a variable's value from its name (symbol), `eval_variable`
simply walks the list of variables, then walks up to the parent
context and repeats until the variable is found.

This is obviously very inefficient: to be efficient, at the very least
we should use some kind of fast mapping structure rather than
performing a linear scan of the variable list. It would be easier to
optimize variable lookups if the code was being properly compiled into
an internal representation. As we're interpreting S-expressions
directly though, it's difficult to do better.
The [original Lisp paper](http://www-formal.stanford.edu/jmc/recursive.html)
did something similar (association lists).

If we were writing a compiler, it would be possible to do much more
efficient optimizations: we could analyze the variables used within
each function and construct a carefully packed memory layout to store
each variable (conceptually the same as how a C compiler defines a
stack frame). However, Yoctolisp is an interpreter and (aside from
parsing S-expressions) does no kind of analysis of program structure
at all! Because of this, the options available for doing this
efficiently are very limited.

#### Functions

Having defined how variables are stored, I define functions as another
type of `YLispValue` (`YLISP_FUNCTION`). Functions have a pointer to
the context where they were defined, and the code to execute
when they are called:
```cpp
	struct {
		struct _YLispValue *context, *code;
	} func;
```
A typical Lisp lambda expression looks like:
```lisp
	(lambda (x y z) (+ x y z))
```
The code part that's needed to be executed is simply the `cdr` of
this. So the C code to handle a Lambda expression looks like this:
```cpp
	} else if (first == keywords[KWD_LAMBDA]) {
		YLispValue *result = ylisp_value(YLISP_FUNCTION);
		result->v.func.context = context;
		result->v.func.code = CDR(code);
		return result;
	}
```
When we want to invoke a function, we create a new context object,
point its parent to the one from the function, populate its variables
from the arguments given to it, and then start executing the
statements from the function body (see `eval_func_call`).

#### Evaluation

The main entry point to evaluating an expression is `ylisp_eval`. This
takes two arguments: a context in which evaluation is occurring, and
the actual code to execute. There are three possibilities for what we
might do:

1. We have a symbol, in which case we need to do a variable lookup
2. We have a literal value, like `123` or `"hello world"`. These
   just evaluate to themselves.
3. We have a cons cell (ie. a list), in which case we need to do
   something more complex.

The `eval_list` function handles evaluation of list expressions. There
are special case checks for handling `if`, `quote`, `lambda` and
`define`, which must be treated specially. The handling for each of
these is fairly simple. There are other keywords that aren't handled
yet. If it isn't a keyword, the list is interpreted as a function
call, and invoked as previously described.

#### Built-ins

To be able to do anything interesting, there needs to be a way to call
into native code. To do this, I first define a function signature for
native C functions:
```cpp
	typedef struct _YLispValue *(*YLispBuiltin)(struct _YLispValue *args);
```
A native function simply takes a linked list of `YLispValue`s. For
example, here's the definition for `cons`:
```cpp
	static YLispValue *builtin_cons(YLispValue *args)
	{
		return ylisp_cons(CAR(args), CAR(CDR(args)));
	}
```
Then I define a new `YLispValue` type (`YLISP_BUILTIN`) to hold a
pointer to a native function. The function call code (`eval_func_call`)
invokes the native function by evaluating the arguments, building
up the linked list and calling it. The built-in functions get added
into the root context at startup.

## Sunday

At this point I have the basic foundations in place and can evaluate
simple expressions by providing them on the command line. From this
point on, what remains is a matter of fleshing out what's in place and
polishing.

### commit [6fc255](https://github.com/fragglet/yoctolisp/commit/6fc255)

#### Garbage collection

A simple garbage collector works by marking all the active objects,
then freeing the objects that have not been marked. This is
essentially the approach I took here. A counter (`values_alloc_count`)
tracks the number of allocated objects and runs the garbage collector
when a threshold number is reached (I arbitrarily chose 100).

The code so far has all been written with the assumption that a
garbage collector would eventually be present, so I haven't made any
effort to track the allocated `YLispValue` structures. This adds extra
complexity because it is necessary to be able to reason about when the
garbage collector can run: if a variable references an allocated
`YLispValue`, that variable must be marked when the collector
runs, or the value might be freed.

The approach I took was to invoke the collector at the start of
`ylisp_eval`. It is then a matter of verifying each location where it
is invoked, and the evaluation code is the only part that needs to be
checked.

There are several locations where variables must be "pinned" - their
values marked when the garbage collector is run. A simple example is
that the context for a function must be pinned while it executes. I
therefore declare two functions to start and stop tracking a variable:
```cpp
	static void pin_variable(YLispValue **variable)
	static void unpin_variable(YLispValue **variable)
```
There are several locations where these pin/unpin pairs are needed.
Getting this right is particularly tricky, as a missing pin can mean
a value being freed unexpectedly. I eventually had to track down a
couple of bugs related to this before I got it right. To make bugs
more obvious, values are `memset()` to clear their contents before
they are freed.

The garbage collector is very crude. One problem is that it just
uses C's native allocator and doesn't make any attempt at managing
memory. Classic Lisp used
[copying garbage collection](https://en.wikipedia.org/wiki/Cheney%27s_algorithm
schemes, which have the advantage that surviving memory is compacted
as part of the process. If we wanted to be really elaborate we'd use
generational garbage collection, which can help to reduce pauses, but
that's more than is probably feasible in a weekend.


### commit [c5cc40](https://github.com/fragglet/yoctolisp/commit/c5cc40)

#### Read-eval-print

A read-eval-print loop is a program that reads an expression from the
console, evaluates it and prints the result. The pieces are now all in
place to do this. This commit adds the `read`, `eval` and `print`
builtin functions, which are essentially wrappers around the
equivalent C functions. The program is still executing expressions
from the command line at this point, so a simple demonstration is:
```
	$ ./yoctolisp "(print (eval (read)))"
	> (+ 1 2 3)
	6
	$
```
### commit [865363](https://github.com/fragglet/yoctolisp/commit/865363)

#### Tail-call optimization

Functional programming languages like Lisp prefer recursion to
iteration. Iterating over lists, for example, is performed by a
function calling itself until the end of the list is reached:
```lisp
	(define (foldl func acc list)
	  (if (null? list)
	    acc
	    (foldl func
	           (func acc (car list))
	           (cdr list))))
```
In an imperative language like C this would be a problem, as a long
list could cause the stack to overflow. At the very least it could
cause large amounts of temporary memory to be used until the function
has completed. For this reason, Lisp-like languages implement
*tail-call optimization*. If a function ends in a function call (ie.
it returns the result of a function call), then its stack frame can be
replaced by that call.

This optimization is important enough that some versions of Lisp (eg.
Scheme) require that implementations include it. It was therefore
desireable to include it in Yoctolisp.

In a compiled implementation it would be possible to analyze the
program structure during the compilation phase and detect tail calls.
Because we are just evaluating S-expressions it's more difficult.
Instead, I came up with an alternate scheme where the last function
call in a function can be "deferred".

I define a new type of `YLispValue` (`YLISP_DEFERRED`) that can be
returned when evaluating a list. This is only used internally to the
evaluation code and never returned from `ylisp_eval`. The function
body code is then changed so that the last expression to be evaluated
is deferred:
```cpp
	if (CDR(code) == NULL) {
		return defer_eval(context, CAR(code));
	} else {
		ylisp_eval(context, CAR(code));
	}
```
The list evaluation code is then wrapped so that deferred calls are
unwrapped and re-evaluated, until a real value is returned.

Several days after I finished the project I discovered a simpler way
of doing this. We only ever pass the deferred call up the call chain,
so only one `YLISP_DEFERRED` object can ever exist. It's therefore
possible to avoid allocating a new `YLispValue` each time and just
use a static one that gets reused
(see commit [cec04b](https://github.com/fragglet/yoctolisp/commit/cec04b)).

### commit [9f1936](https://github.com/fragglet/yoctolisp/commit/9f1936)

Up until now, all expressions are still being evaluated from the
command line arguments. This changes the program to read from a file
specified on the command line instead. This requires some changes to
the way the parsing code works, so that multiple S-expressions can be
read. I also add a simple program to run a read-eval-print loop.

### commit [47dfb1](https://github.com/fragglet/yoctolisp/commit/47dfb1)

This fixes a bug in the string lexing code that I've missed until now;
most of the expressions I've been using as test input so far have just
been using integers. I only realised that the string code was broken
when I tried to write a "hello world" example!

### commit [ad78dc](https://github.com/fragglet/yoctolisp/commit/ad78dc)

The builtin comparison function (=) so far only handles integers. This
commit extends it to work properly with other types; it's particularly
important to be able to compare against empty lists so that recursive
functions can be written for processing lists.

### commit [6031b3](https://github.com/fragglet/yoctolisp/commit/6031b3)

This fixes a subtle bug due to a missing GC pin, caused by the
tail-call optimization. It shows how tricky it is to write a good
garbage collector. I add some conditional code to display values
as they are freed, to assist with debugging future GC bugs.

### commit [f39910](https://github.com/fragglet/yoctolisp/commit/f39910)

This commit adds less-than, greater-than and negation builtin
functions, and also adds handling for the `begin` keyword. The latter
turns out to be easy to handle as the function call body code can
simply be reused.

After the weekend was over I later came back and
[simplified](https://github.com/fragglet/yoctolisp/commit/14f4d72)
how the comparison operators are implemented. I moved much of the
logic for the comparison operators into Lisp code: all comparisons
can be defined in terms of the `=` and `<` comparisons.

### commit [6238da](https://github.com/fragglet/yoctolisp/commit/6238da)

Following the previous commit, I add support for the `let` keyword.
This works essentially like an inner function; a new context is
created inside the current context, and the body of the `let`
expression evaluated like a function body.

The addition of the `begin` and `let` keywords means that the
read-eval-print loop example can terminate when an empty line is
typed.

### commit [9508f3](https://github.com/fragglet/yoctolisp/commit/9508f3)

At this point the interpreter has now reached a level of completeness
that it is possible to write fairly complex programs. There are lots
of standard functions still missing, but rather than implement them as
built-ins, they can be implemented from the primitives already
available. I add a standard library file read on startup and
containing implementations of some well-known Lisp functions.

### commit [1405b4](https://github.com/fragglet/yoctolisp/commit/1405b4)

Defining functions requires using a lambda expression, for example:
```lisp
	(define map
	  (lambda (callback list)
	    ...
```
Having started writing a significant number of functions with the
introduction of the standard library, this is becoming increasingly
tedious. I therefore add support for Scheme's function declaration
syntax, ie.:
```lisp
	(define (map callback list)
	  ...
```
### commit [7d9592](https://github.com/fragglet/yoctolisp/commit/7d9592)

#### Variadic functions

The function processing code assumes that functions have a fixed
number of arguments, but there are some Lisp functions that operate on
a variable number of arguments. An example is `sum`:
```lisp
	(sum 1 2 3 4 5)
```
Functions take variable lists of arguments by using the "dot"
notation to specify a variable. For example, this is what the
definition of `sum` looks like:
```lisp
	(define (sum . list) (foldl + 0 list))
```
This commit adds support for this syntax, and opens the door for more
complicated functions to be added to the standard library.

### commit [d5e454](https://github.com/fragglet/yoctolisp/commit/d5e454)

Following the previous commit, this adds some more functions to the
standard library. It also adds `foldl`, which allows various functions
to be defined tersely.

### commit [7021cb](https://github.com/fragglet/yoctolisp/commit/7021cb)

I finish off the weekend's work by implementing `cond`, which I
defined as a keyword much earlier but never implemented.


