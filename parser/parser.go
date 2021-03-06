package parser

import (
	"fmt"
	"strconv"

	"github.com/showbufire/monkey/ast"
	"github.com/showbufire/monkey/lexer"
	"github.com/showbufire/monkey/token"
)

type (
	prefixParseFn func() ast.Expression
	infixParseFn  func(ast.Expression) ast.Expression
)

const (
	_ int = iota
	LOWEST
	EQUALS      // ==
	LESSGREATER // > or <
	SUM         // +
	PRODUCT     // *
	PREFIX      // -X or !X
	CALL        // myFunction(X)
)

var precedences = map[token.TokenType]int{
	token.EQ:       EQUALS,
	token.NOT_EQ:   EQUALS,
	token.LT:       LESSGREATER,
	token.GT:       LESSGREATER,
	token.PLUS:     SUM,
	token.MINUS:    SUM,
	token.ASTERISK: PRODUCT,
	token.SLASH:    PRODUCT,
	token.LPAREN:   CALL,
}

type Parser struct {
	l      *lexer.Lexer
	errors []string

	curToken  token.Token
	peekToken token.Token

	prefixParseFns map[token.TokenType]prefixParseFn
	infixParseFns  map[token.TokenType]infixParseFn
}

func New(l *lexer.Lexer) *Parser {
	p := &Parser{
		l:      l,
		errors: []string{},
	}

	p.nextToken()
	p.nextToken()

	p.prefixParseFns = make(map[token.TokenType]prefixParseFn)
	p.prefixParseFns[token.IDENT] = p.parseIdentifier
	p.prefixParseFns[token.INT] = p.parseIntegerLiteral
	p.prefixParseFns[token.BANG] = p.parsePrefixExpression
	p.prefixParseFns[token.MINUS] = p.parsePrefixExpression
	p.prefixParseFns[token.TRUE] = p.parseBooleanLiteral
	p.prefixParseFns[token.FALSE] = p.parseBooleanLiteral
	p.prefixParseFns[token.LPAREN] = p.parseGroupedExpression
	p.prefixParseFns[token.IF] = p.parseIfExpression
	p.prefixParseFns[token.FUNCTION] = p.parseFunctionLiteral

	p.infixParseFns = make(map[token.TokenType]infixParseFn)
	p.infixParseFns[token.EQ] = p.parseInfixExpression
	p.infixParseFns[token.NOT_EQ] = p.parseInfixExpression
	p.infixParseFns[token.LT] = p.parseInfixExpression
	p.infixParseFns[token.GT] = p.parseInfixExpression
	p.infixParseFns[token.PLUS] = p.parseInfixExpression
	p.infixParseFns[token.MINUS] = p.parseInfixExpression
	p.infixParseFns[token.ASTERISK] = p.parseInfixExpression
	p.infixParseFns[token.SLASH] = p.parseInfixExpression
	p.infixParseFns[token.LPAREN] = p.parseCallExpression

	return p
}

func (p *Parser) Errors() []string {
	return p.errors
}

func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *ast.Program {
	program := &ast.Program{}
	program.Statements = []ast.Statement{}

	for p.curToken.Type != token.EOF {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
		p.nextToken()
	}
	return program
}

func (p *Parser) parseStatement() ast.Statement {
	switch p.curToken.Type {
	case token.LET:
		return p.parseLetStatement()
	case token.RETURN:
		return p.parseReturnStatement()
	default:
		return p.parseExpressionStatement()
	}
}

func (p *Parser) parseExpressionStatement() *ast.ExpressionStatement {
	stmt := &ast.ExpressionStatement{Token: p.curToken}
	stmt.Expression = p.parseExpression(LOWEST)
	if p.peekTokenIs(token.SEMICOLON) {
		p.nextToken()
	}
	return stmt
}

func (p *Parser) noPrefixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no prefix parse function for %s", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) noInfixParseFnError(t token.TokenType) {
	msg := fmt.Sprintf("no infix parse function for %s", t)
	p.errors = append(p.errors, msg)
}

func (p *Parser) parseGroupedExpression() ast.Expression {
	p.nextToken()
	exp := p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return exp
}

func (p *Parser) parseExpression(precedence int) ast.Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()
	for !p.peekTokenIs(token.SEMICOLON) && !p.peekTokenIs(token.EOF) {
		if pr, ok := precedences[p.peekToken.Type]; ok && precedence < pr {
			infix := p.infixParseFns[p.peekToken.Type]
			if infix == nil {
				p.noInfixParseFnError(p.peekToken.Type)
				return nil
			}
			p.nextToken()
			leftExp = infix(leftExp)
		} else {
			break
		}
	}

	return leftExp
}

func (p *Parser) parsePrefixExpression() ast.Expression {
	pe := &ast.PrefixExpression{
		Token:    p.curToken,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	pe.Right = p.parseExpression(PREFIX)
	return pe
}

func (p *Parser) parseInfixExpression(leftExp ast.Expression) ast.Expression {
	ie := &ast.InfixExpression{
		Token:    p.curToken,
		Left:     leftExp,
		Operator: p.curToken.Literal,
	}
	p.nextToken()
	ie.Right = p.parseExpression(precedences[ie.Token.Type])
	return ie
}

func (p *Parser) parseIfExpression() ast.Expression {
	ie := &ast.IfExpression{
		Token: p.curToken,
	}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	p.nextToken()
	ie.Condition = p.parseExpression(LOWEST)
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	ie.Consequence = p.parseBlockStatement()
	if p.peekTokenIs(token.ELSE) {
		p.nextToken()
		if !p.expectPeek(token.LBRACE) {
			return nil
		}
		ie.Alternative = p.parseBlockStatement()
	}

	return ie
}

func (p *Parser) parseBlockStatement() *ast.BlockStatement {
	be := &ast.BlockStatement{
		Token: p.curToken,
	}
	be.Statements = []ast.Statement{}
	for !p.peekTokenIs(token.RBRACE) && !p.peekTokenIs(token.EOF) {
		p.nextToken()
		if stmt := p.parseStatement(); stmt != nil {
			be.Statements = append(be.Statements, stmt)
		}
	}
	if !p.expectPeek(token.RBRACE) {
		return nil
	}
	return be
}

func (p *Parser) parseIdentifier() ast.Expression {
	return &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
}

func (p *Parser) parseIntegerLiteral() ast.Expression {
	lit := &ast.IntegerLiteral{Token: p.curToken}
	value, err := strconv.ParseInt(p.curToken.Literal, 0, 64)
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as integer", p.curToken.Literal)
		p.errors = append(p.errors, msg)
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseBooleanLiteral() ast.Expression {
	bl := &ast.BooleanLiteral{Token: p.curToken}
	value, err := strconv.ParseBool(bl.TokenLiteral())
	if err != nil {
		msg := fmt.Sprintf("could not parse %q as bool", p.curToken)
		p.errors = append(p.errors, msg)
		return nil
	}
	bl.Value = value
	return bl
}

func (p *Parser) parseFunctionLiteral() ast.Expression {
	fl := &ast.FunctionLiteral{Token: p.curToken}
	if !p.expectPeek(token.LPAREN) {
		return nil
	}
	fl.Parameters = p.parseParameters()
	if !p.expectPeek(token.LBRACE) {
		return nil
	}
	fl.Body = p.parseBlockStatement()
	return fl
}

func (p *Parser) parseParameters() []*ast.Identifier {
	params := []*ast.Identifier{}
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return params
	}
	p.nextToken()
	params = append(params, p.parseIdentifier().(*ast.Identifier))
	for !p.peekTokenIs(token.RPAREN) && !p.peekTokenIs(token.EOF) {
		if !p.expectPeek(token.COMMA) {
			return nil
		}
		p.nextToken()
		params = append(params, p.parseIdentifier().(*ast.Identifier))
	}
	if !p.expectPeek(token.RPAREN) {
		return nil
	}
	return params
}

func (p *Parser) parseCallExpression(left ast.Expression) ast.Expression {
	ce := &ast.CallExpression{
		Token:    p.curToken,
		Function: left,
	}
	ce.Arguments = []ast.Expression{}
	if p.peekTokenIs(token.RPAREN) {
		p.nextToken()
		return ce
	}
	p.nextToken()
	ce.Arguments = append(ce.Arguments, p.parseExpression(LOWEST))
	for !p.peekTokenIs(token.RPAREN) && !p.peekTokenIs(token.EOF) {
		if !p.expectPeek(token.COMMA) {
			return nil
		}
		p.nextToken()
		ce.Arguments = append(ce.Arguments, p.parseExpression(LOWEST))
	}
	p.expectPeek(token.RPAREN)
	return ce
}

func (p *Parser) parseReturnStatement() *ast.ReturnStatement {
	stmt := &ast.ReturnStatement{Token: p.curToken}
	p.nextToken()
	stmt.ReturnValue = p.parseExpression(LOWEST)
	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}
	return stmt
}

func (p *Parser) parseLetStatement() *ast.LetStatement {
	stmt := &ast.LetStatement{Token: p.curToken}

	if !p.expectPeek(token.IDENT) {
		return nil
	}
	stmt.Name = &ast.Identifier{
		Token: p.curToken,
		Value: p.curToken.Literal,
	}
	if !p.expectPeek(token.ASSIGN) {
		return nil
	}
	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)

	if !p.expectPeek(token.SEMICOLON) {
		return nil
	}
	return stmt
}

func (p *Parser) curTokenIs(t token.TokenType) bool {
	return p.curToken.Type == t
}

func (p *Parser) peekTokenIs(t token.TokenType) bool {
	return p.peekToken.Type == t
}

func (p *Parser) peekError(t token.TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %v instead",
		t, p.peekToken)
	p.errors = append(p.errors, msg)
}

func (p *Parser) expectPeek(t token.TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}
