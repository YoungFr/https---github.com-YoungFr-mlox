package parser

import (
	"github.com/youngfr/mlox/token"
)

type Parser struct {
	tokens  []token.Token
	current int
}

func NewParser(tokens []token.Token) *Parser {
	n := len(tokens)
	t := make([]token.Token, n)
	for i := 0; i < n; i++ {
		t[i] = tokens[i]
	}
	return &Parser{
		tokens:  t,
		current: 0,
	}
}

func (p *Parser) Parse() []Stmt {
	statements := make([]Stmt, 0)
	for !p.isAtEnd() {
		statements = append(statements, p.decl())
	}
	return statements
}

// declaration -> varDecl | statement ;
func (p *Parser) decl() Stmt {
	// a variable declaration statement
	if p.match(token.VAR) {
		return p.varDecl()
	}
	// other statements
	return p.statement()
}

// varDecl -> "var" IDENTIFIER ( "=" expression )? ";" ;
func (p *Parser) varDecl() Stmt {
	// variable's name
	var name token.Token
	if p.check(token.IDE) {
		name = p.advance()
	} // else -> error: expect variable name
	// variable's initializer
	var initializer Expr
	if p.match(token.ASG) {
		initializer = p.expression()
	}
	// terminated semicolon
	if p.check(token.SEM) {
		p.advance()
	} // else -> error: expect ';' after variable declaration statement
	return NewVar(name, initializer)
}

// statement -> exprStmt | printStmt | block ;
func (p *Parser) statement() Stmt {
	// print -> printStmt
	if p.match(token.PRINT) {
		return p.printStatement()
	}
	// { -> block
	if p.match(token.LBRACE) {
		return NewBlock(p.block())
	}
	return p.expressionStatement()
}

// printStmt -> "print" expression ";" ;
func (p *Parser) printStatement() Stmt {
	value := p.expression()
	// terminated semicolon
	if p.check(token.SEM) {
		p.advance()
	} // else -> error: expect ';' after print statement
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
	} // else -> error: expect '}' after block statement
	return statements
}

// exprStmt -> expression ";" ;
func (p *Parser) expressionStatement() Stmt {
	value := p.expression()
	// terminated semicolon
	if p.check(token.SEM) {
		p.advance()
	} // else -> error: expect ';' after expression statement
	return NewExpression(value)
}

// expression -> assignment
func (p *Parser) expression() Expr {
	return p.assignment()
}

// assignment -> IDENTIFIER "=" assignment | equality ;
func (p *Parser) assignment() Expr {
	expr := p.equality()
	if p.match(token.ASG) {
		// assignmentTarget := p.prev()
		value := p.assignment()
		if variableExpr, ok := expr.(*Variable); ok {
			name := variableExpr.Name
			return NewAssign(name, value)
		} // else -> error: invalid assignment target.
	}
	return expr
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

// unary -> ( "!" | "-" ) unary | primary;
func (p *Parser) unary() Expr {
	if p.match(token.NOT, token.SUB) {
		operator := p.prev()
		roperand := p.unary()
		return NewUnary(operator, roperand)
	}
	return p.primary()
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
		} // else -> error: expect ')' after group expression
		return NewGroup(expr)
	}
	return nil
}
