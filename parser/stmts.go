package parser

import "github.com/youngfr/mlox/token"

// All statements implement the Stmt interface.
type Stmt interface {
	Accept(StmtVisitor) any
}

// Statement Visitor
type StmtVisitor interface {
	VisitorPrintStmt(*Print) any
	VisitorExpressionStmt(*Expression) any
	VisitorVarStmt(*Var) any
	VisitorBlockStmt(*Block) any
}

// >>>>>>>>>>>>>>>>>>>> print statement >>>>>>>>>>>>>>>>>>>>

type Print struct {
	Expression Expr `json:"printstmt_expr"`
}

func (p *Print) Accept(sv StmtVisitor) any {
	return sv.VisitorPrintStmt(p)
}

func NewPrint(expression Expr) *Print {
	return &Print{Expression: expression}
}

// print !true;
var _ = Stmt(NewPrint(NewUnary(token.NewToken(token.NOT, "!", nil, 1), NewLiteral(true))))

// <<<<<<<<<<<<<<<<<<<< print statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> expression statement >>>>>>>>>>>>>>>>>>>>

type Expression struct {
	ExprStmtExpr Expr `json:"exprstmt_expr"`
}

func (e *Expression) Accept(sv StmtVisitor) any {
	return sv.VisitorExpressionStmt(e)
}

func NewExpression(expression Expr) *Expression {
	return &Expression{ExprStmtExpr: expression}
}

// -5;
var _ = Stmt(NewExpression(NewUnary(token.NewToken(token.SUB, "-", nil, 1), NewLiteral((5)))))

// <<<<<<<<<<<<<<<<<<<< expression statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> variable declaration statement >>>>>>>>>>>>>>>>>>>>

type Var struct {
	Name        token.Token `json:"variable_name"`
	Initializer Expr        `json:"initial_value"`
}

func (v *Var) Accept(sv StmtVisitor) any {
	return sv.VisitorVarStmt(v)
}

func NewVar(name token.Token, initializer Expr) *Var {
	return &Var{
		Name:        name,
		Initializer: initializer,
	}
}

// var a = 5;
var _ = Stmt(NewVar(token.NewToken(token.IDE, "a", nil, 1), NewLiteral(5)))

// <<<<<<<<<<<<<<<<<<<< variable declaration statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> block statement >>>>>>>>>>>>>>>>>>>>

type Block struct {
	Statements []Stmt `json:"block_statements"`
}

func (b *Block) Accept(sv StmtVisitor) any {
	return sv.VisitorBlockStmt(b)
}

func NewBlock(statements []Stmt) *Block {
	ss := make([]Stmt, len(statements))
	copy(ss, statements)
	return &Block{Statements: ss}
}

// {
//     var a = 1;
// 	   print a;
// }
var _ = Stmt(NewBlock([]Stmt{
	Stmt(NewVar(token.NewToken(token.IDE, "a", nil, 1), NewLiteral(1))),
	Stmt(NewPrint(Expr(NewVariable(token.NewToken(token.IDE, "a", nil, 2))))),
}))

// <<<<<<<<<<<<<<<<<<<< block statement <<<<<<<<<<<<<<<<<<<<
