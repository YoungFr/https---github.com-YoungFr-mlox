package token

import (
	"encoding/json"
	"fmt"
	"strconv"
)

type Token struct {
	Ttype TokenType

	// A Lexeme is a smallest sequence that still represent
	// something meaningful. A lexeme is only a substring of
	// the source code.
	Lexeme string

	// The living runtime object of number and string literals.
	Literal interface{}

	// The Line tracks what source line token is on.
	Line int
}

func NewToken(ttype TokenType, lexeme string, literal any, line int) Token {
	return Token{
		Ttype:   ttype,
		Lexeme:  lexeme,
		Literal: literal,
		Line:    line,
	}
}

func (t Token) String() string {
	return fmt.Sprintf("Token(%s %s Ln %s)",
		tokens[int(t.Ttype)], t.Lexeme, strconv.Itoa(t.Line))
}

var _ = fmt.Stringer(NewToken(NUM, "123.45", 123.45, 5))

func (t Token) MarshalJSON() ([]byte, error) {
	if t.Literal != nil {
		return json.Marshal(map[string]interface{}{
			"lexeme":  t.Lexeme,
			"literal": t.Literal,
			"toktype": tokens[int(t.Ttype)],
		})
	} else {
		return json.Marshal(map[string]interface{}{
			"lexeme":  t.Lexeme,
			"toktype": tokens[int(t.Ttype)],
		})
	}
}

var _ = json.Marshaler(NewToken(NUM, "123.45", 123.45, 5))
