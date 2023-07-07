package parser

import "github.com/youngfr/mlox/token"

// All statement should implement the Stmt interface.
type Stmt interface {
	Accept(StmtVisitor) any
}

// Statement Visitor
type StmtVisitor interface {
	VisitorPrintStmt(*Print) any
	VisitorExpressionStmt(*Expression) any
	VisitorVarStmt(*Var) any
	VisitorBlockStmt(*Block) any
	VisitorIfStmt(*If) any
	VisitorWhileStmt(*While) any
	VisitorFunctionStmt(*Function) any
	// VisitorReturnStmt(*Return) any
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
//     print a;
// }
var _ = Stmt(NewBlock([]Stmt{
	Stmt(NewVar(token.NewToken(token.IDE, "a", nil, 1), NewLiteral(1))),
	Stmt(NewPrint(Expr(NewVariable(token.NewToken(token.IDE, "a", nil, 2))))),
}))

// <<<<<<<<<<<<<<<<<<<< block statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> if statement >>>>>>>>>>>>>>>>>>>>

type If struct {
	Condition  Expr `json:"if_condition"`
	ThenBranch Stmt `json:"then_branch"`
	ElseBranch Stmt `json:"else_branch"`
}

func (i *If) Accept(sv StmtVisitor) any {
	return sv.VisitorIfStmt(i)
}

func NewIf(condition Expr, thenBranch Stmt, elseBranch Stmt) *If {
	return &If{
		Condition:  condition,
		ThenBranch: thenBranch,
		ElseBranch: elseBranch,
	}
}

// if (true) {
//     print 1;
// } else {
//     print 0;
// }
var _ = Stmt(NewIf(
	Expr(NewLiteral(true)),
	Stmt(NewPrint(NewLiteral(1))),
	Stmt(NewPrint(NewLiteral(0))),
))

// <<<<<<<<<<<<<<<<<<<< if statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> while statement >>>>>>>>>>>>>>>>>>>>

type While struct {
	Condition Expr `json:"while_condition"`
	LoopBody  Stmt `json:"while_loop_body"`
}

func (w *While) Accept(sv StmtVisitor) any {
	return sv.VisitorWhileStmt(w)
}

func NewWhile(condition Expr, loopBody Stmt) *While {
	return &While{
		Condition: condition,
		LoopBody:  loopBody,
	}
}

// while (true) {
//     print 888;
// }
var _ = Stmt(NewWhile(
	Expr(NewLiteral(true)),
	Stmt(NewPrint(NewLiteral(888))),
))

// <<<<<<<<<<<<<<<<<<<< while statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> function statement >>>>>>>>>>>>>>>>>>>>

type Function struct {
	Name   token.Token   `json:"name"`
	Params []token.Token `json:"parameters"`
	Body   []Stmt        `json:"function_body"`
}

func (f *Function) Accept(sv StmtVisitor) any {
	return sv.VisitorFunctionStmt(f)
}

func NewFunction(name token.Token, params []token.Token, body []Stmt) *Function {
	p := make([]token.Token, len(params))
	copy(p, params)
	b := make([]Stmt, len(body))
	copy(b, body)
	return &Function{
		Name:   name,
		Params: p,
		Body:   b,
	}
}

// <<<<<<<<<<<<<<<<<<<< function statement <<<<<<<<<<<<<<<<<<<<

// >>>>>>>>>>>>>>>>>>>> return statement >>>>>>>>>>>>>>>>>>>>

// type Return struct {
// 	Keyword token.Token `json:"keyword"`
// 	Value   Expr        `json:"value"`
// }

// func (r *Return) Accept(sv StmtVisitor) any {
// 	return sv.VisitorReturnStmt(r)
// }

// func NewReturn(keyword token.Token, value Expr) *Return {
// 	return &Return{
// 		Keyword: keyword,
// 		Value:   value,
// 	}
// }

// <<<<<<<<<<<<<<<<<<<< return statement <<<<<<<<<<<<<<<<<<<<
