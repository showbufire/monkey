package parser

import (
	"fmt"
	"testing"

	"github.com/showbufire/monkey/ast"
	"github.com/showbufire/monkey/lexer"
)

func TestLetStatements(t *testing.T) {
	input := `
let x = 5;
let y = 10;
let foobar = true;
`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p, input)
	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements. got = %d", len(program.Statements))
	}
	tests := []struct {
		expectedIdentifier string
		expectedValue      interface{}
	}{
		{"x", 5},
		{"y", 10},
		{"foobar", true},
	}
	for i, tt := range tests {
		stmt := program.Statements[i]
		if !testLetStatement(t, stmt, tt.expectedIdentifier, tt.expectedValue) {
			return
		}
	}
}

func checkParserErrors(t *testing.T, p *Parser, input string) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}
	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.Errorf("parser input is %s", input)
	t.FailNow()
}

func testLetStatement(t *testing.T, s ast.Statement, name string, value interface{}) bool {
	if s.TokenLiteral() != "let" {
		t.Errorf("s.TokenLiteral not 'let'. got=%q", s.TokenLiteral())
		return false
	}
	letStmt, ok := s.(*ast.LetStatement)
	if !ok {
		t.Errorf("s not *ast.LetStatement. got=%T", s)
		return false
	}
	if letStmt.Name.Value != name {
		t.Errorf("letStmt.Name.Value not '%s'. got=%s", name, letStmt.Name.Value)
		return false
	}
	if letStmt.Name.TokenLiteral() != name {
		t.Errorf("letStmt.Name.TokenLiteral not '%s'. got='%s'", name, letStmt.Name)
		return false
	}
	if !testLiteralExpression(t, letStmt.Value, value) {
		return false
	}
	return true
}

func TestReturnStatement(t *testing.T) {
	input := `
return 5;
return 10;
return false;
`
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 3 {
		t.Fatalf("program.Statements does not contain 3 statements. got=%d", len(program.Statements))
	}

	expectedValues := []interface{}{5, 10, false}

	for i, stmt := range program.Statements {
		returnStmt, ok := stmt.(*ast.ReturnStatement)

		if !ok {
			t.Fatalf("stmt not *ast.returnStatement. got=%T", stmt)
		}
		if returnStmt.TokenLiteral() != "return" {
			t.Fatalf("returnStmt.TokenLiteral not 'return', got %q", returnStmt.TokenLiteral())
			return
		}
		if !testLiteralExpression(t, returnStmt.ReturnValue, expectedValues[i]) {
			return
		}
	}
}

func TestIdentifierExpression(t *testing.T) {
	input := "foobar;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	testIdentifier(t, stmt.Expression, "foobar")
}

func TestIntegerLiteralExpression(t *testing.T) {
	input := "5;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	testIntegeralLiteral(t, stmt.Expression, 5)
}

func TestBooleanLiteralExpression(t *testing.T) {
	input := "false;"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	testBooleanLiteral(t, stmt.Expression, false)
}

func TestParsingPrefixExpression(t *testing.T) {
	prefixTests := []struct {
		input        string
		operator     string
		integerValue interface{}
	}{
		{"!5;", "!", 5},
		{"-15;", "-", 15},
		{"!10", "!", 10},
		{"!true", "!", true},
		{"!false", "!", false},
	}

	for _, tt := range prefixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p, tt.input)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain %d statements. got=%d\n", 1, len(program.Statements))
		}
		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
		}
		exp, ok := stmt.Expression.(*ast.PrefixExpression)
		if !ok {
			t.Fatalf("stmt is not ast.PrefixExpression. got=%T", stmt.Expression)
		}
		if exp.Operator != tt.operator {
			t.Fatalf("exp.Operator is not '%s'. got=%s",
				tt.operator, exp.Operator)
		}
		if !testLiteralExpression(t, exp.Right, tt.integerValue) {
			return
		}
	}
}

func testIntegeralLiteral(t *testing.T, il ast.Expression, value int64) bool {
	integ, ok := il.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("il not *ast.IntegerLiteral. got=%T", il)
		return false
	}
	if integ.Value != value {
		t.Errorf("integ.Value not %d. got=%d", value, integ.Value)
		return false
	}
	if integ.TokenLiteral() != fmt.Sprintf("%d", value) {
		t.Errorf("integ.TokenLiteral not %d. got=%s",
			value, integ.TokenLiteral())
		return false
	}
	return true
}

func testIdentifier(t *testing.T, exp ast.Expression, value string) bool {
	i, ok := exp.(*ast.Identifier)
	if !ok {
		t.Errorf("exp not *ast.Identifier. got=%T", exp)
		return false
	}
	if i.Value != value {
		t.Errorf("i.Value not %s. got=%s", value, i.Value)
		return false
	}
	if i.TokenLiteral() != value {
		t.Errorf("i.TokenLiteral not %s. got=%s", value, i.TokenLiteral())
		return false
	}
	return true
}

func testBooleanLiteral(t *testing.T, exp ast.Expression, value bool) bool {
	bl, ok := exp.(*ast.BooleanLiteral)
	if !ok {
		t.Errorf("exp not *ast.BooleanLiteral. got=%T", exp)
		return false
	}
	if bl.Value != value {
		t.Errorf("bl.Value not %v. got=%v", value, bl.Value)
		return false
	}
	return true
}

func testLiteralExpression(
	t *testing.T,
	exp ast.Expression,
	expected interface{},
) bool {
	switch v := expected.(type) {
	case int:
		return testIntegeralLiteral(t, exp, int64(v))
	case int64:
		return testIntegeralLiteral(t, exp, v)
	case string:
		return testIdentifier(t, exp, v)
	case bool:
		return testBooleanLiteral(t, exp, v)
	}
	return false
}

func testInfixExpression(
	t *testing.T,
	exp ast.Expression,
	left interface{},
	operator string,
	right interface{},
) bool {
	opExp, ok := exp.(*ast.InfixExpression)
	if !ok {
		t.Errorf("exp not InfixExpression. got=%T", exp)
		return false
	}
	if !testLiteralExpression(t, opExp.Left, left) {
		return false
	}
	if operator != opExp.Operator {
		t.Errorf("operator not %s. got=%s", operator, opExp.Operator)
		return false
	}
	if !testLiteralExpression(t, opExp.Right, right) {
		return false
	}
	return true
}

func TestParsingInfixExpressions(t *testing.T) {
	infixTests := []struct {
		input      string
		leftValue  interface{}
		operator   string
		rightValue interface{}
	}{
		{"5 + 5;", 5, "+", 5},
		{"5 - 5;", 5, "-", 5},
		{"5 * 5;", 5, "*", 5},
		{"5 / 5;", 5, "/", 5},
		{"5 > 5;", 5, ">", 5},
		{"5 < 5;", 5, "<", 5},
		{"5 == 5;", 5, "==", 5},
		{"5 != 5;", 5, "!=", 5},
		{"true == true", true, "==", true},
		{"false != true", false, "!=", true},
	}

	for _, tt := range infixTests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p, tt.input)

		if len(program.Statements) != 1 {
			t.Fatalf("program.Statements does not contain %d statements. got=%d", 1, len(program.Statements))
		}

		stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
		if !ok {
			t.Fatalf("program.Statements[0] is not an expression statement. got=%T", program.Statements[0])
		}
		if !testInfixExpression(t, stmt.Expression, tt.leftValue, tt.operator, tt.rightValue) {
			return
		}
	}
}

func TestOperatorPrecedenceParsing(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{
			"-a * b",
			"((-a) * b)",
		},
		{
			"!-a",
			"(!(-a))",
		},
		{
			"a + b + c",
			"((a + b) + c)",
		},
		{
			"a + b - c",
			"((a + b) - c)",
		},
		{
			"a * b * c",
			"((a * b) * c)",
		},
		{
			"a * b / c",
			"((a * b) / c)",
		},
		{
			"a + b / c",
			"(a + (b / c))",
		},
		{
			"a + b * c + d / e - f",
			"(((a + (b * c)) + (d / e)) - f)",
		},
		{
			"3 + 4; -5 * 5",
			"(3 + 4)((-5) * 5)",
		},
		{
			"5 < 4 != 3 > 4",
			"((5 < 4) != (3 > 4))",
		},
		{
			"3 + 4 * 5 == 3 * 1 + 4 * 5",
			"((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
		},
		{
			"3 > 5 == false",
			"((3 > 5) == false)",
		},
		{
			"3 < 5 == true",
			"((3 < 5) == true)",
		},
		{
			"(3 + 4) * 5 == 3 * (4 * 5)",
			"(((3 + 4) * 5) == (3 * (4 * 5)))",
		},
	}

	for _, tt := range tests {
		l := lexer.New(tt.input)
		p := New(l)
		program := p.ParseProgram()
		checkParserErrors(t, p, tt.input)

		actual := program.String()
		if actual != tt.expected {
			t.Errorf("expected = %q, got=%q", tt.expected, actual)
		}
	}
}

func TestIfExpression(t *testing.T) {
	input := "if (x < y) { x }"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	ie, ok := stmt.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("stmt.Expression not ast.IfExpression. got=%T", stmt.Expression)
	}

	if !testInfixExpression(t, ie.Condition, "x", "<", "y") {
		return
	}

	if len(ie.Consequence.Statements) != 1 {
		t.Fatalf("#ie.Consequence.Statements not 1. got=%d", len(ie.Consequence.Statements))
	}
	ce, ok := ie.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Consequence.Statements[0] not ExpressionStatement. got=%T", ie.Consequence.Statements[0])
	}
	testLiteralExpression(t, ce.Expression, "x")
}

func TestIfElseExpression(t *testing.T) {
	input := "if (x < y) { x } else { x; y; }"

	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	ie, ok := stmt.Expression.(*ast.IfExpression)
	if !ok {
		t.Fatalf("stmt.Expression not ast.IfExpression. got=%T", stmt.Expression)
	}

	if !testInfixExpression(t, ie.Condition, "x", "<", "y") {
		return
	}

	if len(ie.Consequence.Statements) != 1 {
		t.Fatalf("#ie.Consequence.Statements not 1. got=%d", len(ie.Consequence.Statements))
	}
	ce, ok := ie.Consequence.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Consequence.Statements[0] not ExpressionStatement. got=%T", ie.Consequence.Statements[0])
	}
	testLiteralExpression(t, ce.Expression, "x")

	if ie.Alternative.Statements == nil {
		t.Fatalf("#ie.Consequence.Statements is nil")
	}

	if len(ie.Alternative.Statements) != 2 {
		t.Fatalf("#ie.Consequence.Statements not 2. got=%d", len(ie.Alternative.Statements))
	}
	ae1, ok := ie.Alternative.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Consequence.Statements[0] not ExpressionStatement. got=%T", ie.Alternative.Statements[0])
	}
	testLiteralExpression(t, ae1.Expression, "x")

	ae2, ok := ie.Alternative.Statements[1].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("Consequence.Statements[1] not ExpressionStatement. got=%T", ie.Alternative.Statements[1])
	}
	testLiteralExpression(t, ae2.Expression, "y")
}

func TestFunctionLiteralParsing(t *testing.T) {
	input := `fn(x, y) { x + y; }`
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	fl, ok := stmt.Expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("stmt.Expression not function literal. got=%T",
			stmt.Expression)
	}
	if fl.TokenLiteral() != "fn" {
		t.Fatalf("fl.TokenLiteral not fn. got=%s", fl.TokenLiteral())
	}
	if len(fl.Parameters) != 2 {
		t.Fatalf("#fl.Parameters not 2. got=%d", len(fl.Parameters))
	}
	if !testLiteralExpression(t, fl.Parameters[0], "x") {
		return
	}
	if !testLiteralExpression(t, fl.Parameters[1], "y") {
		return
	}
	if len(fl.Body.Statements) != 1 {
		t.Fatalf("#fl.Body.Statements not 1, got=%d",
			len(fl.Body.Statements))
	}
	bs, ok := fl.Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("fl.Body.Statements[0] not expression statement. got=%T", fl.Body.Statements[0])
	}
	testInfixExpression(t, bs.Expression, "x", "+", "y")
}

func TestFunctionLiteralNoParamParsing(t *testing.T) {
	input := `fn() { x + y; }`
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	fl, ok := stmt.Expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("stmt.Expression not function literal. got=%T",
			stmt.Expression)
	}
	if fl.TokenLiteral() != "fn" {
		t.Fatalf("fl.TokenLiteral not fn. got=%s", fl.TokenLiteral())
	}
	if len(fl.Parameters) != 0 {
		t.Fatalf("#fl.Parameters not 0. got=%d", len(fl.Parameters))
	}
	if len(fl.Body.Statements) != 1 {
		t.Fatalf("#fl.Body.Statements not 1, got=%d",
			len(fl.Body.Statements))
	}
	bs, ok := fl.Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("fl.Body.Statements[0] not expression statement. got=%T", fl.Body.Statements[0])
	}
	testInfixExpression(t, bs.Expression, "x", "+", "y")
}

func TestFunctionLiteralSingleParamParsing(t *testing.T) {
	input := `fn(x) { x + y; }`
	l := lexer.New(input)
	p := New(l)
	program := p.ParseProgram()
	checkParserErrors(t, p, input)

	if len(program.Statements) != 1 {
		t.Fatalf("program has not enough statements. got=%d",
			len(program.Statements))
	}
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}
	fl, ok := stmt.Expression.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("stmt.Expression not function literal. got=%T",
			stmt.Expression)
	}
	if fl.TokenLiteral() != "fn" {
		t.Fatalf("fl.TokenLiteral not fn. got=%s", fl.TokenLiteral())
	}
	if len(fl.Parameters) != 1 {
		t.Fatalf("#fl.Parameters not 1. got=%d", len(fl.Parameters))
	}
	if !testLiteralExpression(t, fl.Parameters[0], "x") {
		return
	}
	if len(fl.Body.Statements) != 1 {
		t.Fatalf("#fl.Body.Statements not 1, got=%d",
			len(fl.Body.Statements))
	}
	bs, ok := fl.Body.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("fl.Body.Statements[0] not expression statement. got=%T", fl.Body.Statements[0])
	}
	testInfixExpression(t, bs.Expression, "x", "+", "y")
}

func TestCallExpression(t *testing.T) {
	input := "add(1, 2 * 3, 4 + 5);"
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p, input)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	ce, ok := stmt.Expression.(*ast.CallExpression)
	if !ok {
		t.Fatalf("stmt.Expression is not ast.CallExpression. got=%T",
			stmt.Expression)
	}
	if !testLiteralExpression(t, ce.Function, "add") {
		return
	}
	if len(ce.Arguments) != 3 {
		t.Fatalf("#ce.Arguments is not 3. got=%d", len(ce.Arguments))
	}
	if !testLiteralExpression(t, ce.Arguments[0], 1) {
		return
	}
	if !testInfixExpression(t, ce.Arguments[1], 2, "*", 3) {
		return
	}
	if !testInfixExpression(t, ce.Arguments[2], 4, "+", 5) {
		return
	}
}

func TestFunctionLiteralCallExpression(t *testing.T) {
	input := "fn(x){x}(x);"
	l := lexer.New(input)
	p := New(l)

	program := p.ParseProgram()
	checkParserErrors(t, p, input)
	stmt, ok := program.Statements[0].(*ast.ExpressionStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not ast.ExpressionStatement. got=%T", program.Statements[0])
	}

	ce, ok := stmt.Expression.(*ast.CallExpression)
	if !ok {
		t.Fatalf("stmt.Expression is not ast.CallExpression. got=%T",
			stmt.Expression)
	}
	fl, ok := ce.Function.(*ast.FunctionLiteral)
	if !ok {
		t.Fatalf("ce.Function is not ast.FunctionLiteral. got=%T",
			ce.Function)
	}
	if fl.String() != "fn(x){x}" {
		t.Fatalf("fl.String not fn(x){x}. got=%s", fl.String())
	}
	if len(ce.Arguments) != 1 {
		t.Fatalf("#ce.Arguments is not 1. got=%d", len(ce.Arguments))
	}
	if !testLiteralExpression(t, ce.Arguments[0], "x") {
		return
	}
}
