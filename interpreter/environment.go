package interpreter

import "github.com/youngfr/mlox/token"

type environment struct {
	enclosing *environment
	values    map[string]any
}

func NewEnvironment(enclosing *environment) *environment {
	return &environment{
		enclosing: enclosing,
		values:    make(map[string]any),
	}
}

func (e *environment) def(name string, value any) {
	e.values[name] = value
}

func (e *environment) get(name token.Token) any {
	if v, ok := e.values[name.Lexeme]; ok {
		return v
	}
	if e.enclosing != nil {
		return e.enclosing.get(name)
	}
	panic("undefined variable: " + name.Lexeme)
}

func (e *environment) asg(name token.Token, value any) {
	if _, ok := e.values[name.Lexeme]; ok {
		e.values[name.Lexeme] = value
		return
	}
	if e.enclosing != nil {
		e.enclosing.asg(name, value)
		return
	}
	panic("undefined variable: " + name.Lexeme)
}
