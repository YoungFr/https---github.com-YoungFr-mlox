package interpreter

import (
	"fmt"

	"github.com/youngfr/mlox/parser"
	"github.com/youngfr/mlox/token"
)

type Interpreter struct {
	env *environment
}

func NewInterpreter() *Interpreter {
	return &Interpreter{
		env: NewEnvironment(nil),
	}
}

func (i *Interpreter) Interpret(statements []parser.Stmt) {
	for _, stmt := range statements {
		i.exec(stmt)
	}
}

// executing statement
func (i *Interpreter) exec(stmt parser.Stmt) {
	stmt.Accept(i)
}

// The *Interpreter should implement the StmtVisitor interface.
var _ = parser.StmtVisitor(NewInterpreter())

// The implementation of the StmtVisitor interface.

func (i *Interpreter) VisitorExpressionStmt(e *parser.Expression) any {
	// expression statement like `a;` `1+2;` `true;` ...
	// We evaluate the expression but discard its value.
	i.eval(e.ExprStmtExpr)
	return nil
}

func (i *Interpreter) VisitorPrintStmt(p *parser.Print) any {
	// print statement like `print a;` `print 1+2;` ...
	// We evaluate the expression and prints its value.
	fmt.Println(i.eval(p.Expression))
	return nil
}

func (i *Interpreter) VisitorVarStmt(v *parser.Var) any {
	// variable declaration statement like `var a;` `var b = 1;` ...
	var value any
	if v.Initializer != nil {
		value = i.eval(v.Initializer)
	}
	i.env.def(v.Name.Lexeme, value)
	return nil
}

func (i *Interpreter) VisitorBlockStmt(b *parser.Block) any {
	i.execBlock(b.Statements, NewEnvironment(i.env))
	return nil
}

func (i *Interpreter) execBlock(statements []parser.Stmt, env *environment) {
	prevEnv := i.env
	defer func() {
		i.env = prevEnv
	}()
	i.env = env
	for _, statement := range statements {
		i.exec(statement)
	}
}

// evaluating expression
func (i *Interpreter) eval(expr parser.Expr) any {
	return expr.Accept(i)
}

// The *Interpreter should implement the ExprVisitor interface.
var _ = parser.ExprVisitor(NewInterpreter())

// The implementation of the ExprVisitor interface.

func (i *Interpreter) VisitorLiteralExpr(l *parser.Literal) any {
	// literal expression like `1` `true` `"str"` ...
	return l.Value
}

func (i *Interpreter) VisitorUnaryExpr(u *parser.Unary) any {
	// unary expression like `-2` `--2` `!(1 < 2)` `!!true` ...
	ropreand := i.eval(u.Ropreand)
	switch u.Operator.Ttype {
	case token.SUB:
		if f, ok := ropreand.(float64); ok {
			return -f
		} // else -> runtime error: unary operator '-' can only be used for numbers
	case token.NOT:
		// In lox language, only the nil and false are false.
		if ropreand == nil {
			return true
		}
		if b, ok := ropreand.(bool); ok {
			return !b
		}
		return false
	}
	return nil
}

func (i *Interpreter) VisitorBinaryExpr(b *parser.Binary) any {
	// binary expression like `1+2` `2*3` `4/5` `2>1` `3==4` `true!=false` ...
	lopreand := i.eval(b.Lopreand)
	roperand := i.eval(b.Ropreand)
	switch b.Operator.Ttype {
	case token.ADD:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf + rf
		}
		ls, okl := lopreand.(string)
		rs, okr := roperand.(string)
		if okl && okr {
			return ls + rs
		}
		// finally -> runtime error:
		// all opreands of the binary operator '+' must be numbers or strings
	case token.SUB:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf - rf
		}
		// else -> runtime error:
		// all opreands of the binary operator '-' must be numbers
	case token.MUL:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf * rf
		}
		// else -> runtime error:
		// all opreands of the binary operator '*' must be numbers
	case token.DIV:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf / rf
		}
		// else -> runtime error:
		// all opreands of the binary operator '/' must be numbers
	case token.GTR:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf > rf
		}
		ls, okl := lopreand.(string)
		rs, okr := roperand.(string)
		if okl && okr {
			return ls > rs
		}
		// finally -> runtime error:
		// all opreands of the binary operator '>' must be numbers or strings
	case token.GEQ:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf >= rf
		}
		ls, okl := lopreand.(string)
		rs, okr := roperand.(string)
		if okl && okr {
			return ls >= rs
		}
		// finally -> runtime error:
		// all opreands of the binary operator '>=' must be numbers or strings
	case token.LSS:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf < rf
		}
		ls, okl := lopreand.(string)
		rs, okr := roperand.(string)
		if okl && okr {
			return ls < rs
		}
		// finally -> runtime error:
		// all opreands of the binary operator '<' must be numbers or strings
	case token.LEQ:
		lf, okl := lopreand.(float64)
		rf, okr := roperand.(float64)
		if okl && okr {
			return lf <= rf
		}
		ls, okl := lopreand.(string)
		rs, okr := roperand.(string)
		if okl && okr {
			return ls <= rs
		}
		// finally -> runtime error:
		// all opreands of the binary operator '<=' must be numbers or strings
	case token.EQL:
		return isEqual(lopreand, roperand)
	case token.NEQ:
		return !isEqual(lopreand, roperand)
	}
	return nil
}

func isEqual(a any, b any) bool {
	if a == nil && b == nil {
		return true
	}
	if a == nil {
		return false
	}
	return a == b
}

func (i *Interpreter) VisitorGroupExpr(g *parser.Group) any {
	// group expression like `(1)` `(1+2)` `(1!=2)` ...
	return i.eval(g.Expression)
}

func (i *Interpreter) VisitorVariableExpr(v *parser.Variable) any {
	// variable expression like `a` `ans` `s` `res` ...
	return i.env.get(v.Name)
}

func (i *Interpreter) VisitorAssignExpr(a *parser.Assign) any {
	value := i.eval(a.Value)
	i.env.asg(a.Name, value)
	return value
}
