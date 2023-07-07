package interpreter

import "github.com/youngfr/mlox/parser"

type LoxCallable interface {
	arity() int
	call(interpreter *Interpreter, arguments []any) any
}

type LoxFunction struct {
	declaration *parser.Function
}

func (lf *LoxFunction) arity() int {
	return len(lf.declaration.Params)
}

func (lf *LoxFunction) call(interpreter *Interpreter, arguments []any) any {
	env := NewEnvironment(nil)
	for i := 0; i < len(lf.declaration.Params); i++ {
		env.def(lf.declaration.Params[i].Lexeme, arguments[i])
	}
	interpreter.execBlock(lf.declaration.Body, env)
	return nil
}

func (lf *LoxFunction) String() string {
	return "<fn " + lf.declaration.Name.Lexeme + ">"
}

func NewLoxFunction(declaration *parser.Function) *LoxFunction {
	return &LoxFunction{
		declaration: declaration,
	}
}
