package parser

import (
	"github.com/youngfr/mlox/token"
)

type Parser struct {
	tokens  []token.Token
	current int
}

func NewParser(tokens []token.Token) *Parser {
	t := make([]token.Token, len(tokens))
	copy(t, tokens)
	return &Parser{
		tokens:  t,
		current: 0,
	}
}

// >>>>>>>>>>>>>>> helper functions >>>>>>>>>>>>>>>

func (p *Parser) peek() token.Token {
	return p.tokens[p.current]
}

func (p *Parser) isAtEnd() bool {
	return p.peek().Ttype == token.EOF
}

func (p *Parser) prev() token.Token {
	return p.tokens[p.current-1]
}

func (p *Parser) advance() token.Token {
	if !p.isAtEnd() {
		p.current++
	}
	return p.prev()
}

func (p *Parser) check(ttype token.TokenType) bool {
	if p.isAtEnd() {
		return false
	}
	return p.peek().Ttype == ttype
}

func (p *Parser) match(ttypes ...token.TokenType) bool {
	for _, ttype := range ttypes {
		if p.check(ttype) {
			p.advance()
			return true
		}
	}
	return false
}

// <<<<<<<<<<<<<<< helper functions <<<<<<<<<<<<<<<

func (p *Parser) Parse() []Stmt {
	statements := make([]Stmt, 0)
	for !p.isAtEnd() {
		statements = append(statements, p.decl())
	}
	return statements
}

// declaration -> funDecl | varDecl | statement ;
func (p *Parser) decl() Stmt {
	if p.match(token.FUN) {
		return p.funDecl("function")
	}
	if p.match(token.VAR) {
		return p.varDecl()
	}
	return p.statement()
}

// function   -> IDENTIFIER "(" parameters? ")" block ;
// parameters -> IDENTIFIER ( "," IDENTIFIER )* ;
func (p *Parser) funDecl(kind string) Stmt {
	var name token.Token
	if p.check(token.IDE) {
		name = p.advance()
	}
	if p.check(token.LPAREN) {
		p.advance()
	}
	parameters := make([]token.Token, 0)
	if !p.check(token.RPAREN) {
		if p.check(token.IDE) {
			parameters = append(parameters, p.advance())
		}
		for p.match(token.COM) {
			if p.check(token.IDE) {
				parameters = append(parameters, p.advance())
			}
		}
	}
	if p.check(token.RPAREN) {
		p.advance()
	}
	if p.check(token.LBRACE) {
		p.advance()
	}
	body := p.block()
	return NewFunction(name, parameters, body)
}

// varDecl -> "var" IDENTIFIER ( "=" expression )? ";" ;
func (p *Parser) varDecl() Stmt {
	var name token.Token
	if p.check(token.IDE) {
		name = p.advance()
	}
	var initializer Expr
	if p.match(token.ASG) {
		initializer = p.expression()
	}
	if p.check(token.SEM) {
		p.advance()
	}
	return NewVar(name, initializer)
}

// statement -> printStmt | block | ifStmt | whileStmt | forStmt | exprStmt | returnStmt ;
func (p *Parser) statement() Stmt {
	if p.match(token.PRINT) {
		return p.printStatement()
	}
	if p.match(token.LBRACE) {
		return NewBlock(p.block())
	}
	if p.match(token.IF) {
		return p.ifStatement()
	}
	if p.match(token.WHILE) {
		return p.whileStatement()
	}
	if p.match(token.FOR) {
		return p.forStatement()
	}
	if p.match(token.RETURN) {
		return p.returnStatement()
	}
	return p.expressionStatement()
}

// printStmt -> "print" expression ";" ;
func (p *Parser) printStatement() Stmt {
	value := p.expression()
	if p.check(token.SEM) {
		p.advance()
	}
	return NewPrint(value)
}

// block -> "{" declaration* "}" ;
func (p *Parser) block() []Stmt {
	statements := make([]Stmt, 0)
	for !p.check(token.RBRACE) && !p.isAtEnd() {
		statements = append(statements, p.decl())
	}
	if p.check(token.RBRACE) {
		p.advance()
	}
	return statements
}

// ifStmt -> "if" "(" expression ")" statement ( "else" statement )? ;
func (p *Parser) ifStatement() Stmt {
	if p.check(token.LPAREN) {
		p.advance()
	}
	condition := p.expression()
	if p.check(token.RPAREN) {
		p.advance()
	}
	thenBranch := p.statement()
	var elseBranch Stmt
	if p.match(token.ELSE) {
		elseBranch = p.statement()
	}
	return NewIf(condition, thenBranch, elseBranch)
}

// whileStmt -> "while" "(" expression ")" statement ;
func (p *Parser) whileStatement() Stmt {
	if p.check(token.LPAREN) {
		p.advance()
	}
	condition := p.expression()
	if p.check(token.RPAREN) {
		p.advance()
	}
	loopBody := p.statement()
	return NewWhile(condition, loopBody)
}

// forStmt -> "for" "(" ( varDecl | exprStmt | ";" ) expression? ";" expression? ")" statement ;
//
//	{
//	    initializer -> (varDecl | exprStmt | ";")
//	    while (condition expression | true) {
//	        statement
//	        increment expression statement
//	    }
//	}
func (p *Parser) forStatement() Stmt {
	if p.check(token.LPAREN) {
		p.advance()
	}
	var initializer Stmt
	if p.match(token.SEM) {
		initializer = nil
	} else if p.match(token.VAR) {
		initializer = p.varDecl()
	} else {
		initializer = p.expressionStatement()
	}
	var condition Expr
	if !p.check(token.SEM) {
		condition = p.expression()
	}
	if p.check(token.SEM) {
		p.advance()
	}
	var increment Expr
	if !p.check(token.RPAREN) {
		increment = p.expression()
	}
	if p.check(token.RPAREN) {
		p.advance()
	}
	loopBody := p.statement()
	if increment != nil {
		loopBody = NewBlock([]Stmt{
			loopBody,
			Stmt(NewExpression(increment)),
		})
	}
	if condition == nil {
		condition = NewLiteral(true)
	}
	loopBody = NewWhile(condition, loopBody)
	if initializer != nil {
		loopBody = NewBlock([]Stmt{
			Stmt(initializer),
			Stmt(loopBody),
		})
	}
	return loopBody
}

// returnStmt -> "return" expression? ";" ;
func (p *Parser) returnStatement() Stmt {
	keyword := p.prev()
	var value Expr
	if !p.check(token.SEM) {
		value = p.expression()
	}
	if p.check(token.SEM) {
		p.advance()
	}
	return NewReturn(keyword, value)
}

// exprStmt -> expression ";" ;
func (p *Parser) expressionStatement() Stmt {
	value := p.expression()
	if p.check(token.SEM) {
		p.advance()
	}
	return NewExpression(value)
}

// expression -> assignment
func (p *Parser) expression() Expr {
	return p.assignment()
}

// assignment -> IDENTIFIER "=" assignment | logic_or ;
func (p *Parser) assignment() Expr {
	expr := p.or()
	if p.match(token.ASG) {
		// assignmentTarget := p.prev()
		value := p.assignment()
		if variableExpr, ok := expr.(*Variable); ok {
			name := variableExpr.Name
			return NewAssign(name, value)
		}
	}
	return expr
}

// logic_or -> logic_and ( "or" logic_and )* ;
func (p *Parser) or() Expr {
	lopreand := p.and()
	for p.match(token.OR) {
		operator := p.prev()
		ropreand := p.and()
		lopreand = NewLogical(lopreand, operator, ropreand)
	}
	return lopreand
}

// logic_and -> equality ( "and" equality )* ;
func (p *Parser) and() Expr {
	lopreand := p.equality()
	for p.match(token.AND) {
		operator := p.prev()
		ropreand := p.equality()
		lopreand = NewLogical(lopreand, operator, ropreand)
	}
	return lopreand
}

// equality -> comparison ( ( "!=" | "==" ) comparison )? ;
func (p *Parser) equality() Expr {
	lopreand := p.comparison()
	if p.match(token.EQL, token.NEQ) {
		operator := p.prev()
		roperand := p.comparison()
		return NewBinary(lopreand, operator, roperand)
	}
	return lopreand
}

// comparison -> term ( ( ">" | ">=" | "<" | "<=" ) term )? ;
func (p *Parser) comparison() Expr {
	lopreand := p.term()
	if p.match(token.GTR, token.GEQ, token.LSS, token.LEQ) {
		operator := p.prev()
		roperand := p.term()
		return NewBinary(lopreand, operator, roperand)
	}
	return lopreand
}

// term -> factor ( ( "-" | "+" ) factor )* ;
func (p *Parser) term() Expr {
	lopreand := p.factor()
	for p.match(token.ADD, token.SUB) {
		operator := p.prev()
		roperand := p.factor()
		lopreand = NewBinary(lopreand, operator, roperand)
	}
	return lopreand
}

// factor -> unary ( ( "/" | "*" ) unary )* ;
func (p *Parser) factor() Expr {
	lopreand := p.unary()
	for p.match(token.MUL, token.DIV) {
		operator := p.prev()
		roperand := p.unary()
		lopreand = NewBinary(lopreand, operator, roperand)
	}
	return lopreand
}

// unary -> ( "!" | "-" ) unary | call ;
func (p *Parser) unary() Expr {
	if p.match(token.NOT, token.SUB) {
		operator := p.prev()
		roperand := p.unary()
		return NewUnary(operator, roperand)
	}
	return p.call()
}

// call -> primary ( "(" arguments? ")" | "." IDENTIFIER )* ;
func (p *Parser) call() Expr {
	expr := p.primary()
	for {
		if p.match(token.LPAREN) {
			expr = p.finishCall(expr)
		} else {
			break
		}
	}
	return expr
}

// arguments -> expression ( "," expression )* ;
func (p *Parser) finishCall(callee Expr) Expr {
	arguments := make([]Expr, 0)
	if !p.check(token.RPAREN) {
		arguments = append(arguments, p.expression())
		for p.match(token.COM) {
			arguments = append(arguments, p.expression())
		}
	}
	var paren token.Token
	if p.check(token.RPAREN) {
		paren = p.advance()
	}
	return NewCall(callee, paren, arguments)
}

// primary -> "true" | "false" | "nil" | NUMBER | STRING | IDENTIFIER | "(" expression ")" ;
func (p *Parser) primary() Expr {
	if p.match(token.NIL) {
		return NewLiteral(nil)
	}
	if p.match(token.TRUE) {
		return NewLiteral(true)
	}
	if p.match(token.FALSE) {
		return NewLiteral(false)
	}
	if p.match(token.NUM, token.STR) {
		return NewLiteral(p.prev().Literal)
	}
	if p.match(token.IDE) {
		return NewVariable(p.prev())
	}
	if p.match(token.LPAREN) {
		expr := p.expression()
		if p.check(token.RPAREN) {
			p.advance()
		}
		return NewGroup(expr)
	}
	return nil
}
