package parser

import (
	"errors"
	"log"

	"ravka/ast"
	"ravka/lexer"
	"ravka/storage"
	"ravka/token"
)

type Parser struct {
	lexer        *lexer.Lexer
	currentToken *token.Token
	jumper       storage.JumperInterface
	useJumper    bool
}

func NewParser(lexer *lexer.Lexer, jumper storage.JumperInterface) (*Parser, error) {
	currentToken, e := lexer.GetNextToken()
	if e != nil {
		return nil, e
	}

	return &Parser{
		lexer,
		currentToken,
		jumper,
		false,
	}, nil
}

func (p *Parser) consume(kind token.Kind) error {
	if p.currentToken == nil {
		return errors.New(" parser eror: tokennya udah abis")
	}

	if p.currentToken.Kind == kind {
		newToken, e := p.lexer.GetNextToken()
		if e != nil {
			return e
		}

		p.currentToken = newToken
		return nil
	}

	return errors.New(" parser eror: harusnya sih ga eror, hmmm")
}

func (p *Parser) parseIdent() (ast.Ident, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	tok := p.currentToken
	if tok.Kind != token.IDENT {
		return ast.Ident{}, errors.New(" parser eror: harusnya ada ID di sini")
	}

	e := p.consume(token.IDENT)
	if e != nil {
		return ast.Ident{}, e
	}

	return ast.Ident{
		Loc:  pos,
		Name: tok.Val,
	}, nil
}

func (p *Parser) parseSimplest() (ast.Node, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	var e error

	tok := p.currentToken

	switch tok.Kind {
	case token.TRUE, token.FALSE:
		e = p.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.Ident{Loc: pos, Name: tok.Val}, nil
	case token.CONTINUE, token.BREAK:
		e = p.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.BranchStmt{Loc: pos, Tok: tok}, nil
	case token.INT, token.STRING:
		e = p.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.BasicLit{Loc: pos, Tok: tok}, nil
	case token.IDENT:
		return p.parseIdent()
	}

	return nil, nil
}

func (p *Parser) parseParen() (ast.Node, error) {
	if p.currentToken.Kind == token.LPAREN {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		e := p.consume(token.LPAREN)
		if e != nil {
			return nil, e
		}

		node, e := p.parseLogicOr()
		if e != nil {
			return nil, e
		}

		e = p.consume(token.RPAREN)
		if e != nil {
			return nil, e
		}

		return ast.ParenExpr{
			Loc: pos,
			X:   node,
		}, nil
	}

	return p.parseSimplest()
}

var unaryNeeds = map[token.Kind]struct{}{
	token.SUB: {},
	token.NOT: {},
}

func (p *Parser) parseUnary() (ast.Node, error) {
	tok := p.currentToken

	_, isNeeded := unaryNeeds[tok.Kind]

	if isNeeded {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		e := p.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		node, e := p.parseLogicOr()
		if e != nil {
			return nil, e
		}

		return ast.UnaryExpr{
			Loc: pos,
			X:   node,
			Op:  tok,
		}, nil
	}

	return p.parseParen()
}

var factorNeeds = map[token.Kind]struct{}{
	token.MUL: {},
	token.DIV: {},
}

func (p *Parser) parseFactor() (ast.Node, error) {
	node, e := p.parseUnary()
	if e != nil {
		return nil, e
	}

	_, isNeeded := factorNeeds[p.currentToken.Kind]

	for isNeeded {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		currentToken := p.currentToken

		switch currentToken.Kind {
		case token.MUL, token.DIV:
			e = p.consume(currentToken.Kind)
			break
		}

		if e != nil {
			return nil, e
		}

		var nodeRight ast.Node
		nodeRight, e = p.parseUnary()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   nodeRight,
		}

		_, isNeeded = factorNeeds[p.currentToken.Kind]
	}

	return node, nil
}

var termNeeds = map[token.Kind]struct{}{
	token.ADD: {},
	token.SUB: {},
}

func (p *Parser) parseTerm() (ast.Node, error) {
	node, e := p.parseFactor()
	if e != nil {
		return nil, e
	}

	_, isNeeded := termNeeds[p.currentToken.Kind]

	for isNeeded {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		currentToken := p.currentToken
		e = p.consume(currentToken.Kind)

		if e != nil {
			return nil, e
		}

		var term ast.Node
		term, e = p.parseFactor()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   term,
		}

		_, isNeeded = termNeeds[p.currentToken.Kind]
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

func (p *Parser) parseComparison() (ast.Node, error) {
	node, e := p.parseTerm()
	if e != nil {
		return nil, e
	}

	_, isNeeded := comparisonNeeds[p.currentToken.Kind]

	for isNeeded {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		currentToken := p.currentToken
		e = p.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = p.parseTerm()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = comparisonNeeds[p.currentToken.Kind]
	}

	return node, nil
}

var logicAndNeeds = map[token.Kind]struct{}{
	token.LAND: {},
}

func (p *Parser) parseLogicAnd() (ast.Node, error) {
	node, e := p.parseComparison()
	if e != nil {
		return nil, e
	}

	_, isNeeded := logicAndNeeds[p.currentToken.Kind]

	for isNeeded {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}
		currentToken := p.currentToken
		e = p.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = p.parseComparison()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = logicAndNeeds[p.currentToken.Kind]
	}

	return node, nil
}

var logicOrNeeds = map[token.Kind]struct{}{
	token.LOR: {},
}

func (p *Parser) parseLogicOr() (ast.Node, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	node, e := p.parseLogicAnd()
	if e != nil {
		return nil, e
	}

	_, isNeeded := logicOrNeeds[p.currentToken.Kind]

	for isNeeded {
		currentToken := p.currentToken
		e = p.consume(currentToken.Kind)
		if e != nil {
			return nil, e
		}

		var y ast.Node
		y, e = p.parseLogicAnd()
		if e != nil {
			return nil, e
		}

		node = ast.BinaryExpr{
			Loc: pos,
			X:   node,
			Op:  currentToken,
			Y:   y,
		}

		_, isNeeded = logicOrNeeds[p.currentToken.Kind]
	}

	return node, nil
}

var assignNeeds = map[token.Kind]struct{}{
	token.DEFINE: {},
	token.ASSIGN: {},
}

func (p *Parser) parseAssignStmt() (ast.Node, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	node, e := p.parseIncDec()
	if e != nil {
		return nil, e
	}

	switch v := node.(type) {
	case ast.Ident:
		if _, needed := assignNeeds[p.currentToken.Kind]; needed {
			currentToken := p.currentToken
			e = p.consume(p.currentToken.Kind)
			if e != nil {
				return nil, e
			}

			var rhs ast.Node
			rhs, e = p.parseIncDec()
			if e != nil {
				return nil, e
			}

			node = ast.AssignStmt{
				Loc: pos,
				LHS: v,
				Op:  currentToken,
				RHS: rhs,
			}
		}
	}

	return node, nil
}

var incDecNeeds = map[token.Kind]struct{}{
	token.INC: {},
	token.DEC: {},
}

func (p *Parser) parseIncDec() (ast.Node, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	node, e := p.parseLogicOr()
	if e != nil {
		return nil, e
	}

	if _, isNeeded := incDecNeeds[p.currentToken.Kind]; isNeeded {
		if typed, ok := node.(ast.Ident); ok {
			tok := p.currentToken
			e = p.consume(tok.Kind)
			if e != nil {
				return nil, e
			}

			return ast.IncDecStmt{
				Loc: pos,
				Tok: tok,
				X:   typed,
			}, nil
		}
	}

	return node, nil
}

func (p *Parser) parsePrint() (ast.PrintStmt, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}
	tok := p.currentToken

	var (
		val ast.Node
		e   error
	)

	if tok.Kind == token.PRT || tok.Kind == token.PRTLN {
		e = p.consume(tok.Kind)
		if e != nil {
			return ast.PrintStmt{}, e
		}

		val, e = p.parseLogicOr()
		if e != nil {
			return ast.PrintStmt{}, e
		}
	}

	return ast.PrintStmt{
		Loc: pos,
		Val: val,
		Tok: tok,
	}, e
}

func (p *Parser) parseBinaryBoolExpr(node ast.Node) (ast.Expr, error) {
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
		log.Println(node)
		return nil, errors.New(" parse eror: butuh ekspresi perbandingan")
	}

	return expr, nil
}

func (p *Parser) parseIf() (ast.IfStmt, error) {
	var (
		e        error
		cond     ast.Node
		condExpr ast.Expr
		stmts    ast.BlockStmt
	)

	e = p.consume(token.IF)
	if e != nil {
		return ast.IfStmt{}, e
	}

	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	cond, e = p.parseLogicOr()
	if e != nil {
		return ast.IfStmt{}, e
	}

	condExpr, e = p.parseBinaryBoolExpr(cond)
	if e != nil {
		return ast.IfStmt{}, e
	}

	stmts, e = p.parseBlockStmt()
	if e != nil {
		return ast.IfStmt{}, e
	}

	els, e := p.parseStmt()
	if e != nil {
		return ast.IfStmt{}, e
	}

	return ast.IfStmt{
		Loc:  pos,
		Cond: condExpr,
		Body: stmts,
		Else: els,
	}, nil
}

func (p *Parser) parseFor() (ast.Stmt, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	var (
		cond     ast.Node
		condExpr ast.Expr
		stmts    ast.BlockStmt
		e        error
	)

	e = p.consume(token.FOR)
	if e != nil {
		return nil, e
	}

	e = p.consume(token.IF)
	if e != nil {
		return nil, e
	}

	cond, e = p.parseLogicOr()
	if e != nil {
		return nil, e
	}

	condExpr, e = p.parseBinaryBoolExpr(cond)
	if e != nil {
		return nil, e
	}

	stmts, e = p.parseBlockStmt()
	if e != nil {
		return nil, e
	}

	return ast.ForStmt{
		Loc:  pos,
		Body: stmts,
		Cond: condExpr,
	}, nil
}

func (p *Parser) parseCheckStmt() (ast.CheckStmt, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	checkId := pos.GetCoordinate()
	e := p.consume(token.CHECK)
	if e != nil {
		return ast.CheckStmt{}, e
	}

	tag, e := p.parseLogicOr()
	if e != nil {
		return ast.CheckStmt{}, e
	}

	tagExpr, ok := tag.(ast.Expr)
	if !ok {
		return ast.CheckStmt{}, errors.New(" parser eror: \"cek\" butuh sebuah ekspresi ")
	}

	p.useJumper = true
	p.jumper.SetGroupId(checkId)
	block, e := p.parseBlockStmt()
	p.useJumper = false
	if e != nil {
		return ast.CheckStmt{}, e
	}

	return ast.CheckStmt{
		Loc:  pos,
		Body: block,
		Tag:  tagExpr,
	}, nil
}

var checkClauseBrake = map[token.Kind]struct{}{
	token.IF:     {},
	token.ELSE:   {},
	token.RBRACE: {},
}

func (p *Parser) parseCaseClause() (ast.CaseClause, error) {
	var (
		e              error
		exprList       []ast.Expr
		stmtList       []ast.Stmt
		node           ast.Stmt
		jumperId       string
		parentJumperId string
	)

	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	e = p.consume(token.IF)
	if e != nil {
		return ast.CaseClause{}, e
	}

	cond, e := p.parseLogicOr()
	if e != nil {
		return ast.CaseClause{}, e
	}

	if expr, ok := cond.(ast.Expr); ok {
		exprList = append(exprList, expr)

		switch typed := expr.(type) {
		case ast.BasicLit:
			prefix := "s-"
			switch typed.Tok.Kind {
			case token.INT:
				prefix = "i-"
			}
			jumperId = prefix + typed.Tok.Val
		case ast.Ident:
			jumperId = "ident-" + typed.Name
		case ast.ParenExpr:
			jumperId = "paren-" + typed.Loc.GetCoordinate()
		}

		parentJumperId = jumperId
	}

	if jumperId == "" {
		return ast.CaseClause{}, errors.New(" parser eror: nilai cek statment butuh sebuah ")
	}

	if p.currentToken.Kind == token.COMMA {
		for p.currentToken.Kind != token.COLON {
			cond, e = p.parseLogicOr()
			if e != nil {
				return ast.CaseClause{}, e
			}

			if expr, ok := cond.(ast.Expr); ok {
				exprList = append(exprList, expr)

				switch typed := expr.(type) {
				case ast.BasicLit:
					prefix := "s-"
					switch typed.Tok.Kind {
					case token.INT:
						prefix = "i-"
					}
					jumperId = prefix + typed.Tok.Val
				case ast.Ident:
					jumperId = "ident-" + typed.Loc.GetCoordinate()
				case ast.ParenExpr:
					jumperId = "paren-" + typed.Loc.GetCoordinate()
				}

				p.jumper.SetAlias(jumperId, parentJumperId)
			}

			if p.currentToken.Kind != token.COLON {
				e = p.consume(token.COMMA)
				if e != nil {
					return ast.CaseClause{}, e
				}
			}
		}
	}

	if p.currentToken.Kind != token.COLON {
		return ast.CaseClause{}, errors.New(" parser eror: nilai cek statment butuh \":\"")
	}

	e = p.consume(token.COLON)
	if e != nil {
		return ast.CaseClause{}, e
	}

	_, stop := checkClauseBrake[p.currentToken.Kind]

	for !stop {
		node, e = p.parseStmt()
		if e != nil {
			return ast.CaseClause{}, e
		}

		stmtList = append(stmtList, node)

		_, stop = checkClauseBrake[p.currentToken.Kind]
	}

	p.jumper.SetByKey(parentJumperId, stmtList)

	return ast.CaseClause{
		Loc:  pos,
		List: exprList,
		Body: stmtList,
	}, nil
}

func (p *Parser) parseDefaultCaseClause() (ast.CaseClause, error) {
	var (
		e        error
		node     ast.Stmt
		exprList []ast.Expr
		stmtList []ast.Stmt
	)

	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}

	e = p.consume(token.ELSE)
	if e != nil {
		return ast.CaseClause{}, e
	}

	if p.currentToken.Kind != token.COLON {
		return ast.CaseClause{}, errors.New(" parser eror: nilai default cek statment butuh \":\"")
	}

	e = p.consume(token.COLON)

	_, stop := checkClauseBrake[p.currentToken.Kind]

	for !stop {
		node, e = p.parseStmt()
		if e != nil {
			return ast.CaseClause{}, e
		}

		stmtList = append(stmtList, node)

		_, stop = checkClauseBrake[p.currentToken.Kind]
	}

	p.jumper.SetDefault(stmtList)

	return ast.CaseClause{
		Loc:  pos,
		List: exprList,
		Body: stmtList,
	}, nil
}

func (p *Parser) parseStmt() (ast.Stmt, error) {
	tok := p.currentToken
	switch tok.Kind {
	case token.PRT, token.PRTLN:
		return p.parsePrint()
	case token.IF:
		if p.useJumper {
			return p.parseCaseClause()
		}
		return p.parseIf()
	case token.CHECK:
		return p.parseCheckStmt()
	case token.ELSE:
		if p.useJumper {
			return p.parseDefaultCaseClause()
		}

		e := p.consume(token.ELSE)
		if e != nil {
			return nil, e
		}

		if p.currentToken.Kind == token.IF {
			return p.parseIf()
		}

		return p.parseBlockStmt()
	case token.FOR:
		return p.parseFor()
	default:
		node, e := p.parseAssignStmt()
		if e != nil {
			return nil, e
		}

		if typed, ok := node.(ast.Stmt); ok {
			return typed, nil
		}
	}

	return nil, nil
}

func (p *Parser) parseBlockStmt() (ast.BlockStmt, error) {
	tok := p.currentToken
	block := ast.BlockStmt{List: []ast.Stmt{}}

	if tok.Kind == token.LBRACE {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		e := p.consume(token.LBRACE)
		if e != nil {
			return ast.BlockStmt{}, e
		}
		for tok.Kind != token.RBRACE {
			var newStmt ast.Stmt
			newStmt, e = p.parseStmt()
			if e != nil {
				return block, e
			}

			block.List = append(block.List, newStmt)

			tok = p.currentToken
		}

		e = p.consume(token.RBRACE)
		if e != nil {
			return block, e
		}

		block.Loc = pos
		return block, nil
	}

	return ast.BlockStmt{}, nil
}

func (p *Parser) parseFuncType() (ast.FuncType, error) {
	tok := p.currentToken

	if tok.Kind == token.LPAREN {
		pos := ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		}

		var (
			e     error
			ident ast.Ident
		)

		e = p.consume(token.LPAREN)
		if e != nil {
			return ast.FuncType{}, e
		}

		fType := ast.FuncType{}
		fType.Loc = pos

		for p.currentToken.Kind != token.RPAREN {
			if p.currentToken.Kind == token.COMMA {
				e = p.consume(token.COMMA)
				if e != nil {
					return ast.FuncType{}, e
				}
				continue
			}

			ident, e = p.parseIdent()
			if e != nil {
				return ast.FuncType{}, e
			}

			fType.Params = append(fType.Params, ident)
		}

		e = p.consume(token.RPAREN)
		if e != nil {
			return ast.FuncType{}, e
		}

		return fType, nil
	}

	return ast.FuncType{}, errors.New(" parser eror: daftar parameternya ga bisa dipahami")
}

func (p *Parser) parseFuncDecl() (ast.FuncDecl, error) {
	pos := ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosY(),
	}
	tok := p.currentToken
	if tok.Kind != token.FUNC {
		return ast.FuncDecl{}, errors.New(" parser eror: deklarasi fungsi ga jelas")
	}

	e := p.consume(token.FUNC)
	if e != nil {
		return ast.FuncDecl{}, e
	}

	fn := ast.FuncDecl{
		Loc:  pos,
		Body: ast.BlockStmt{},
		Name: ast.Ident{},
		Tok:  nil,
		Type: ast.FuncType{},
	}

	ident, e := p.parseIdent()
	if e != nil {
		return ast.FuncDecl{}, e
	}

	typ, e := p.parseFuncType()
	if e != nil {
		return ast.FuncDecl{}, e
	}

	block, e := p.parseBlockStmt()
	if e != nil {
		return ast.FuncDecl{}, e
	}

	fn.Name = ident
	fn.Type = typ
	fn.Body = block

	return fn, nil
}

func (p *Parser) parseDecls() (ast.Decls, error) {
	decls := ast.Decls{
		Loc: ast.Position{
			X: p.lexer.GetPosX(),
			Y: p.lexer.GetPosY(),
		},
		Val: []ast.Decl{},
	}
	decl, e := p.parseFuncDecl()
	if e != nil {
		return ast.Decls{}, e
	}

	decls.Val = append(decls.Val, decl)

	return decls, nil
}

func (p *Parser) parseMain() (ast.Node, error) {
	if p.lexer.GetInputType() == lexer.STRING {
		return p.parseStmt()
	} else {
		return p.parseDecls()
	}
}

func (p *Parser) Parse() (ast.Node, error) {
	return p.parseMain()
}
