package interpreter

import "github.com/youngfr/mlox/token"

type environment struct {
	values map[string]any
}

func (e *environment) def(name string, value any) {
	e.values[name] = value
}

func (e *environment) get(name token.Token) any {
	if v, ok := e.values[name.Lexeme]; ok {
		return v
	} // else -> runtime error: undefined variable
	return nil
}
