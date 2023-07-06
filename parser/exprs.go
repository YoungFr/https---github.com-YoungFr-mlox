package parser

import (
	"github.com/youngfr/mlox/token"
)

// All expressions should implement the Expr interface.
type Expr interface {
	Accept(ExprVisitor) any
}

// Expression Visitor
type ExprVisitor interface {
	VisitorLiteralExpr(*Literal) any
	VisitorUnaryExpr(*Unary) any
	VisitorBinaryExpr(*Binary) any
	VisitorGroupExpr(*Group) any
	VisitorVariableExpr(*Variable) any
	VisitorAssignExpr(*Assign) any
	VisitorLogicalExpr(*Logical) any
}

// >>>>>>>>>>>>>>>>>>>> literal expression >>>>>>>>>>>>>>>>>>>>

type Literal struct {
	Value any `json:"literal_value"`
}

func (l *Literal) Accept(ev ExprVisitor) any {
	return ev.VisitorLiteralExpr(l)
}

func NewLiteral(value any) *Literal {
	return &Literal{Value: value}
}

// number literal 12.34
var _ = Expr(NewLiteral(12.34))

// string literal "str"
var _ = Expr(NewLiteral("str"))

// <<<<<<<<<<<<<<<<<<<< literal expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> unary expression >>>>>>>>>>>>>>>>>>>>

type Unary struct {
	Operator token.Token `json:"unary_operator"`
	Ropreand Expr        `json:"unary_ropreand"`
}

func (u *Unary) Accept(ev ExprVisitor) any {
	return ev.VisitorUnaryExpr(u)
}

func NewUnary(operator token.Token, ropreand Expr) *Unary {
	return &Unary{
		Operator: operator,
		Ropreand: ropreand,
	}
}

// !true
var _ = Expr(NewUnary(token.NewToken(token.NOT, "!", nil, 1), NewLiteral(true)))

// -1.25
var _ = Expr(NewUnary(token.NewToken(token.SUB, "-", nil, 1), NewLiteral(1.25)))

// <<<<<<<<<<<<<<<<<<<< unary expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> binary expression >>>>>>>>>>>>>>>>>>>>

type Binary struct {
	Lopreand Expr        `json:"binary_lopreand"`
	Operator token.Token `json:"binary_operator"`
	Ropreand Expr        `json:"binary_ropreand"`
}

func (b *Binary) Accept(ev ExprVisitor) any {
	return ev.VisitorBinaryExpr(b)
}

func NewBinary(lopreand Expr, operator token.Token, ropreand Expr) *Binary {
	return &Binary{
		Lopreand: lopreand,
		Operator: operator,
		Ropreand: ropreand,
	}
}

// 1 + 2
var _ = Expr(NewBinary(NewLiteral(1), token.NewToken(token.ADD, "+", nil, 1), NewLiteral(2)))

// 1 / 2
var _ = Expr(NewBinary(NewLiteral(1), token.NewToken(token.DIV, "/", nil, 1), NewLiteral(2)))

// 1 > 2
var _ = Expr(NewBinary(NewLiteral(1), token.NewToken(token.GTR, ">", nil, 1), NewLiteral(2)))

// <<<<<<<<<<<<<<<<<<<< binary expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> group expression >>>>>>>>>>>>>>>>>>>>

type Group struct {
	Expression Expr `json:"group_expression"`
}

func (g *Group) Accept(ev ExprVisitor) any {
	return ev.VisitorGroupExpr(g)
}

func NewGroup(expression Expr) *Group {
	return &Group{Expression: expression}
}

// (-1.25)
var _ = Expr(NewGroup(NewUnary(token.NewToken(token.SUB, "-", nil, 1), NewLiteral(1.25))))

// <<<<<<<<<<<<<<<<<<<< group expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> variable expression >>>>>>>>>>>>>>>>>>>>

type Variable struct {
	Name token.Token `json:"variable_name"`
}

func (v *Variable) Accept(ev ExprVisitor) any {
	return ev.VisitorVariableExpr(v)
}

func NewVariable(name token.Token) *Variable {
	return &Variable{Name: name}
}

// a_variable
var _ = Expr(NewVariable(token.NewToken(token.IDE, "a_variable", nil, 1)))

// <<<<<<<<<<<<<<<<<<<< variable expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> assign expression >>>>>>>>>>>>>>>>>>>>

type Assign struct {
	Name  token.Token `json:"assign_name"`
	Value Expr        `json:"assign_value"`
}

func (a *Assign) Accept(ev ExprVisitor) any {
	return ev.VisitorAssignExpr(a)
}

func NewAssign(name token.Token, value Expr) *Assign {
	return &Assign{
		Name:  name,
		Value: value,
	}
}

// a = 5
var _ = Expr(NewAssign(token.NewToken(token.IDE, "a", nil, 1), NewLiteral(5)))

// <<<<<<<<<<<<<<<<<<<< assign expression <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> logical expression >>>>>>>>>>>>>>>>>>>>

type Logical struct {
	Lopreand Expr        `json:"logical_opreand"`
	Operator token.Token `json:"logical_operator"`
	Ropreand Expr        `json:"logical_ropreand"`
}

func (l *Logical) Accept(ev ExprVisitor) any {
	return ev.VisitorLogicalExpr(l)
}

func NewLogical(lopreand Expr, operator token.Token, ropreand Expr) *Logical {
	return &Logical{
		Lopreand: lopreand,
		Operator: operator,
		Ropreand: ropreand,
	}
}

// 1 <= 2 or 7 != 8
var _ = Expr(NewLogical(
	Expr(NewBinary(NewLiteral(1), token.NewToken(token.LSS, "<=", nil, 1), NewLiteral(2))),
	token.NewToken(token.OR, "or", nil, 1),
	Expr(NewBinary(NewLiteral(7), token.NewToken(token.NEQ, "!=", nil, 1), NewLiteral(8))),
))

// <<<<<<<<<<<<<<<<<<<< logical expression <<<<<<<<<<<<<<<<<<<<
