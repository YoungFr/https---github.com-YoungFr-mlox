package token

type TokenType int

const (
	LPAREN TokenType = iota // (
	RPAREN                  // )
	LBRACE                  // {
	RBRACE                  // }
	COM                     // ,
	DOT                     // .
	ADD                     // +
	SUB                     // -
	MUL                     // *
	DIV                     // /
	SEM                     // ;
	NOT                     // !
	NEQ                     // !=
	ASG                     // =
	EQL                     // ==
	GTR                     // >
	GEQ                     // >=
	LSS                     // <
	LEQ                     // <=
	STR                     // string
	NUM                     // number
	IDE                     // identifier
	IF                      // if
	OR                      // or
	AND                     // and
	EOF                     // EOF
	FOR                     // for
	FUN                     // fun
	NIL                     // nil
	VAR                     // var
	ELSE                    // else
	THIS                    // this
	TRUE                    // true
	CLASS                   // class
	FALSE                   // false
	PRINT                   // print
	SUPER                   // super
	WHILE                   // while
	RETURN                  // return
)

var tokens = [...]string{
	LPAREN: "LEFT_PAREN",    // (
	RPAREN: "RIGHT_PAREN",   // )
	LBRACE: "LEFT_BRACE",    // {
	RBRACE: "RIGHT_BRACE",   // }
	COM:    "COMMA",         // ,
	DOT:    "DOT",           // .
	ADD:    "ADD",           // +
	SUB:    "SUB",           // -
	MUL:    "MUL",           // *
	DIV:    "DIV",           // /
	SEM:    "SEMICOLON",     // ;
	NOT:    "NOT",           // !
	NEQ:    "NOT_EQUAL",     // !=
	ASG:    "ASSIGN",        // =
	EQL:    "EQUAL",         // ==
	GTR:    "GREATER",       // >
	GEQ:    "GREATER_EQUAL", // >=
	LSS:    "LESS",          // <
	LEQ:    "LESS_EQUAL",    // <=
	STR:    "STRING",        // string
	NUM:    "NUMBER",        // number
	IDE:    "IDENTIFIER",    // identifier
	IF:     "IF",            // if
	OR:     "OR",            // or
	AND:    "AND",           // and
	EOF:    "EOF",           // EOF
	FOR:    "FOR",           // for
	FUN:    "FUNCTION",      // fun
	NIL:    "NIL",           // nil
	VAR:    "VARIABLE",      // var
	ELSE:   "ELSE",          // else
	THIS:   "THIS",          // this
	TRUE:   "TRUE",          // true
	CLASS:  "CLASS",         // class
	FALSE:  "FALSE",         // false
	PRINT:  "PRINT",         // print
	SUPER:  "SUPER",         // super
	WHILE:  "WHILE",         // while
	RETURN: "RETURN",        // return
}
