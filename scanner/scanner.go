package scanner

import (
	"errors"
	"strconv"

	"github.com/youngfr/mlox/token"
)

type Scanner struct {
	// The source code is just a simple string.
	source string

	// tokens list
	tokens []token.Token

	// The start points to the first character in the lexeme being scanned.
	start int

	// The current points at the character currently being considered.
	current int

	// The line tracks what source line current is on.
	line int
}

func NewScanner(source string) *Scanner {
	return &Scanner{
		source:  source,
		tokens:  make([]token.Token, 0),
		start:   0,
		current: 0,
		line:    1,
	}
}

func (sc *Scanner) isAtEnd() bool {
	return sc.current >= len(sc.source)
}

func (sc *Scanner) ScanTokens() ([]token.Token, error) {
	for !sc.isAtEnd() {
		// We are at the beginning of the next lexeme.
		sc.start = sc.current
		if err := sc.scanToken(); err != nil {
			return nil, err
		}
	}
	sc.tokens = append(sc.tokens, token.NewToken(token.EOF, "", nil, sc.line))
	return sc.tokens, nil
}

// The advance method consumes the next character in the source code and returns it.
func (sc *Scanner) advance() byte {
	sc.current++
	return sc.source[sc.current-1]
}

func (sc *Scanner) addToken(ttype token.TokenType, literal interface{}) {
	var lexeme string = sc.source[sc.start:sc.current]
	sc.tokens = append(sc.tokens, token.NewToken(ttype, lexeme, literal, sc.line))
}

func (sc *Scanner) scanToken() error {
	switch c := sc.advance(); c {

	// A single character long lexeme.
	case '(':
		sc.addToken(token.LPAREN, nil)
	case ')':
		sc.addToken(token.RPAREN, nil)
	case '{':
		sc.addToken(token.LBRACE, nil)
	case '}':
		sc.addToken(token.RBRACE, nil)
	case ',':
		sc.addToken(token.COM, nil)
	case '.':
		sc.addToken(token.DOT, nil)
	case '+':
		sc.addToken(token.ADD, nil)
	case '-':
		sc.addToken(token.SUB, nil)
	case '*':
		sc.addToken(token.MUL, nil)
	case ';':
		sc.addToken(token.SEM, nil)

	// For '!', '=', '<' and '>', we need to look at the second
	// character to determine if we're on "!=", "==", "<=" and ">="
	// or merely '!', '=', '<' and '>'.
	case '!':
		if sc.match('=') {
			sc.addToken(token.NEQ, nil)
		} else {
			sc.addToken(token.NOT, nil)
		}
	case '=':
		if sc.match('=') {
			sc.addToken(token.EQL, nil)
		} else {
			sc.addToken(token.ASG, nil)
		}
	case '<':
		if sc.match('=') {
			sc.addToken(token.LEQ, nil)
		} else {
			sc.addToken(token.LSS, nil)
		}
	case '>':
		if sc.match('=') {
			sc.addToken(token.GEQ, nil)
		} else {
			sc.addToken(token.GTR, nil)
		}

	// A single-line comment or a division operator.
	case '/':
		if sc.match('/') {
			// A single-line comment goes until the end of the line.
			for sc.peek() != '\n' && !sc.isAtEnd() {
				sc.advance()
			}
		} else {
			// A division operator.
			sc.addToken(token.DIV, nil)
		}

	// For a '\n', we need increment the line counter.
	case '\n':
		sc.line++

	// Ignore whitespace.
	case ' ', '\r', '\t':

	// string literals
	case '"':
		if err := sc.string(); err != nil {
			return err
		}

	// number literals, variables and keywords
	default:
		switch {
		case isDigit(c):
			sc.number()
		case isAlpha(c):
			sc.identifier()
		default:
			// error: Unexpected character
			return errors.New("Ln " + strconv.Itoa(sc.line) + ": Unexpected character.")
		}
	}

	return nil
}

// The match method acts like a conditional advance method.
func (sc *Scanner) match(expected byte) bool {
	if sc.isAtEnd() {
		return false
	}
	if sc.source[sc.current] != expected {
		return false
	}
	sc.current++
	return true
}

// one character of lookahead
func (sc *Scanner) peek() byte {
	if sc.current >= len(sc.source) {
		return 0
	}
	return sc.source[sc.current]
}

// two characters of lookahead
func (sc *Scanner) peekNext() byte {
	if sc.current+1 >= len(sc.source) {
		return 0
	}
	return sc.source[sc.current+1]
}

func (sc *Scanner) string() error {
	for sc.peek() != '"' && !sc.isAtEnd() {
		// Lox supports multi-line strings.
		if sc.peek() == '\n' {
			sc.line++
		}
		sc.advance()
	}
	if sc.isAtEnd() {
		// error: Unterminated string
		return errors.New("Ln " + strconv.Itoa(sc.line) + ": Unterminated string.")
	}
	// The closing '"'
	sc.advance()
	// Trimming the surrounding quotes.
	str := sc.source[sc.start+1 : sc.current-1]
	sc.addToken(token.STR, str)
	return nil
}

// All numbers in Lox are floating point at runtime,
// and literals like .12345 and 123456. are invalid.
func (sc *Scanner) number() {
	for isDigit(sc.peek()) {
		sc.advance()
	}
	// Looking for a fractional part.
	if sc.peek() == '.' && isDigit(sc.peekNext()) {
		// consumes the '.'
		sc.advance()
		for isDigit(sc.peek()) {
			sc.advance()
		}
	}
	lexeme := sc.source[sc.start:sc.current]
	num, _ := strconv.ParseFloat(lexeme, 64)
	sc.addToken(token.NUM, num)
}

func (sc *Scanner) identifier() {
	for isAlphaDigit(sc.peek()) {
		sc.advance()
	}
	lexeme := sc.source[sc.start:sc.current]
	if ttype, ok := keywords[lexeme]; ok {
		// the keyword's token type
		sc.addToken(ttype, nil)
	} else {
		// a regular user-defined identifier
		sc.addToken(token.IDE, nil)
	}
}
