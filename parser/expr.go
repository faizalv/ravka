package parser

import (
	"errors"

	"ravka/ast"
	"ravka/runtime"
	"ravka/token"
)

type ExprParsingType int

const (
	ExprIdent ExprParsingType = iota
	ExprSimplest
)

type ExprMasterInterface interface {
	ParseExpr() (ast.Expr, error)
	ValidateBinaryExpr(node ast.Node) (ast.Expr, error)
	SetStmtHandler(stmtHandler func() (ast.Stmt, error))
	ParseDirectly(nodeType ExprParsingType) (ast.Expr, error)
}

type ExprMaster struct {
	parseRuntime runtime.ParseInterface
	stmtHandler  func() (ast.Stmt, error)
}

func NewExprMaster(runtime runtime.ContainerInterface) *ExprMaster {
	return &ExprMaster{
		parseRuntime: runtime.GetParseRuntime(),
	}
}

func (xp *ExprMaster) SetStmtHandler(stmtHandler func() (ast.Stmt, error)) {
	xp.stmtHandler = stmtHandler
}

func (xp *ExprMaster) ParseDirectly(nodeType ExprParsingType) (ast.Expr, error) {
	switch nodeType {
	case ExprSimplest:
		return xp.parseSimplest()
	case ExprIdent:
		return xp.parseIdent()
	default:
		return nil, errors.New(" parser eror: expr, parsing langsung ga tau mau parsing apa, ini internal eror")
	}
}

func (xp *ExprMaster) getCurrentToken() *token.Token {
	return xp.parseRuntime.GetCurrentToken()
}

func (xp *ExprMaster) getPosition() ast.Position {
	return xp.parseRuntime.GetParsePosition()
}

func (xp *ExprMaster) consume(tok token.Kind) error {
	return xp.parseRuntime.ConsumeToken(tok)
}

func (xp *ExprMaster) parseIdent() (ast.Ident, error) {
	pos := xp.getPosition()

	tok := xp.getCurrentToken()
	if tok.Kind != token.IDENT {
		return ast.Ident{}, errors.New(" parser eror: harusnya ada ID di sini")
	}

	e := xp.consume(token.IDENT)
	if e != nil {
		return ast.Ident{}, e
	}

	return ast.Ident{
		Loc:  pos,
		Name: tok.Val,
	}, nil
}

func (xp *ExprMaster) parseSimplest() (ast.Expr, error) {
	pos := xp.getPosition()

	var e error

	tok := xp.getCurrentToken()

	switch tok.Kind {
	case token.TRUE, token.FALSE:
		e = xp.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.Ident{Loc: pos, Name: tok.Val}, nil
	case token.INT, token.STRING:
		e = xp.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.BasicLit{Loc: pos, Tok: tok}, nil
	case token.IDENT:
		return xp.parseIdent()
	}

	return nil, nil
}

func (xp *ExprMaster) parseParen() (ast.Expr, error) {
	if xp.getCurrentToken().Kind == token.LPAREN {
		pos := xp.getPosition()

		e := xp.consume(token.LPAREN)
		if e != nil {
			return nil, e
		}

		node, e := xp.parseLogicOr()
		if e != nil {
			return nil, e
		}

		e = xp.consume(token.RPAREN)
		if e != nil {
			return nil, e
		}

		return ast.ParenExpr{
			Loc: pos,
			X:   node,
		}, nil
	}

	return xp.parseSimplest()
}

var unaryNeeds = map[token.Kind]struct{}{
	token.SUB: {},
	token.NOT: {},
}

func (xp *ExprMaster) parseUnary() (ast.Expr, error) {
	tok := xp.getCurrentToken()

	_, isNeeded := unaryNeeds[tok.Kind]

	if isNeeded {
		pos := xp.getPosition()

		e := xp.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		node, e := xp.parseLogicOr()
		if e != nil {
			return nil, e
		}

		return ast.UnaryExpr{
			Loc: pos,
			X:   node,
			Op:  tok,
		}, nil
	}

	return xp.parseParen()
}

var factorNeeds = map[token.Kind]struct{}{
	token.MUL: {},
	token.DIV: {},
}

func (xp *ExprMaster) parseFactor() (ast.Expr, error) {
	node, e := xp.parseUnary()
	if e != nil {
		return nil, e
	}

	_, isNeeded := factorNeeds[xp.getCurrentToken().Kind]

	for isNeeded {
		pos := xp.getPosition()

		currentToken := xp.getCurrentToken()

		switch currentToken.Kind {
		case token.MUL, token.DIV:
			e = xp.consume(currentToken.Kind)
			break
		}

		if e != nil {
			return nil, e
		}

		var nodeRight ast.Node
		nodeRight, e = xp.parseUnary()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   nodeRight,
		}

		_, isNeeded = factorNeeds[xp.getCurrentToken().Kind]
	}

	return node, nil
}

var termNeeds = map[token.Kind]struct{}{
	token.ADD: {},
	token.SUB: {},
}

func (xp *ExprMaster) parseTerm() (ast.Expr, error) {
	node, e := xp.parseFactor()
	if e != nil {
		return nil, e
	}

	_, isNeeded := termNeeds[xp.getCurrentToken().Kind]

	for isNeeded {
		pos := xp.getPosition()

		currentToken := xp.getCurrentToken()
		e = xp.consume(currentToken.Kind)

		if e != nil {
			return nil, e
		}

		var term ast.Node
		term, e = xp.parseFactor()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   term,
		}

		_, isNeeded = termNeeds[xp.getCurrentToken().Kind]
	}

	return node, nil
}

var comparisonNeeds = map[token.Kind]struct{}{
	token.LSS: {},
	token.LEQ: {},
	token.GTR: {},
	token.GEQ: {},
	token.EQL: {},
}

func (xp *ExprMaster) parseComparison() (ast.Expr, error) {
	node, e := xp.parseTerm()
	if e != nil {
		return nil, e
	}

	_, isNeeded := comparisonNeeds[xp.getCurrentToken().Kind]

	for isNeeded {
		pos := xp.getPosition()

		currentToken := xp.getCurrentToken()
		e = xp.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = xp.parseTerm()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = comparisonNeeds[xp.getCurrentToken().Kind]
	}

	return node, nil
}

var logicAndNeeds = map[token.Kind]struct{}{
	token.LAND: {},
}

func (xp *ExprMaster) parseLogicAnd() (ast.Expr, error) {
	node, e := xp.parseComparison()
	if e != nil {
		return nil, e
	}

	_, isNeeded := logicAndNeeds[xp.getCurrentToken().Kind]

	for isNeeded {
		pos := xp.getPosition()
		currentToken := xp.getCurrentToken()
		e = xp.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = xp.parseComparison()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = logicAndNeeds[xp.getCurrentToken().Kind]
	}

	return node, nil
}

var logicOrNeeds = map[token.Kind]struct{}{
	token.LOR: {},
}

func (xp *ExprMaster) parseLogicOr() (ast.Expr, error) {
	pos := xp.getPosition()

	node, e := xp.parseLogicAnd()
	if e != nil {
		return nil, e
	}

	_, isNeeded := logicOrNeeds[xp.getCurrentToken().Kind]

	for isNeeded {
		currentToken := xp.getCurrentToken()
		e = xp.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = xp.parseLogicAnd()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = logicOrNeeds[xp.getCurrentToken().Kind]
	}

	return node, nil
}

func (xp *ExprMaster) ValidateBinaryExpr(node ast.Node) (ast.Expr, error) {
	var (
		ok    bool
		ident ast.Ident
		expr  ast.Expr
		bin   ast.BinaryExpr
	)

	if bin, ok = node.(ast.BinaryExpr); ok {
		expr = bin
	} else if ident, ok = node.(ast.Ident); ok {
		expr = ident
	} else {
		return nil, errors.New(" parse eror: butuh ekspresi perbandingan")
	}

	return expr, nil
}

func (xp *ExprMaster) ParseExpr() (ast.Expr, error) {
	if xp.stmtHandler == nil {
		return nil, errors.New(" parser eror: pengelola stmt-nya ga ada, ini internal eror")
	}
	return xp.parseLogicOr()
}
