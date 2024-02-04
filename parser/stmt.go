package parser

import (
	"errors"

	"ravka/ast"
	"ravka/runtime"
	"ravka/token"
)

type StmtParsingType int

const (
	StmtBlock StmtParsingType = iota
)

type StmtMasterInterface interface {
	ParseStatement() (ast.Stmt, error)
	SetExprHandler(handler func() (ast.Expr, error))
	SetBinaryExprValidator(validator func(ast.Node) (ast.Expr, error))
	ParseDirectly(nodeType StmtParsingType) (ast.Node, error)
}

type StmtMaster struct {
	parseRuntime        runtime.ParseInterface
	runtimeContainer    runtime.ContainerInterface
	exprHandler         func() (ast.Expr, error)
	binaryExprValidator func(node ast.Node) (ast.Expr, error)
}

func NewStmtMaster(runtime runtime.ContainerInterface) *StmtMaster {
	return &StmtMaster{
		runtimeContainer: runtime,
		parseRuntime:     runtime.GetParseRuntime(),
	}
}

func (s *StmtMaster) SetExprHandler(handler func() (ast.Expr, error)) {
	s.exprHandler = handler
}

func (s *StmtMaster) SetBinaryExprValidator(validator func(ast.Node) (ast.Expr, error)) {
	s.binaryExprValidator = validator
}

func (s *StmtMaster) ParseDirectly(nodeType StmtParsingType) (ast.Node, error) {
	switch nodeType {
	case StmtBlock:
		return s.parseBlockStmt()
	default:
		return nil, errors.New(" parser eror: stmt, parsing langsung ga tau mau parsing apa, ini internal eror")
	}
}

func (s *StmtMaster) getCurrentToken() *token.Token {
	return s.parseRuntime.GetCurrentToken()
}

func (s *StmtMaster) getPosition() ast.Position {
	return s.parseRuntime.GetParsePosition()
}

func (s *StmtMaster) consume(tok token.Kind) error {
	return s.parseRuntime.ConsumeToken(tok)
}

func (s *StmtMaster) parseSimplest() (ast.Stmt, error) {
	pos := s.getPosition()

	var e error

	tok := s.getCurrentToken()

	switch tok.Kind {
	case token.CONTINUE, token.BREAK:
		e = s.consume(tok.Kind)
		if e != nil {
			return nil, e
		}

		return ast.BranchStmt{Loc: pos, Tok: tok}, nil
	}
	return nil, nil
}

func (s *StmtMaster) parsePrint() (ast.PrintStmt, error) {
	pos := s.getPosition()
	tok := s.getCurrentToken()

	var (
		val ast.Node
		e   error
	)

	if tok.Kind == token.PRT || tok.Kind == token.PRTLN {
		e = s.consume(tok.Kind)
		if e != nil {
			return ast.PrintStmt{}, e
		}

		val, e = s.exprHandler()
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

func (s *StmtMaster) parseIf() (ast.IfStmt, error) {
	var (
		e        error
		cond     ast.Node
		condExpr ast.Expr
		stmts    ast.BlockStmt
	)

	e = s.consume(token.IF)
	if e != nil {
		return ast.IfStmt{}, e
	}

	pos := s.getPosition()

	cond, e = s.exprHandler()
	if e != nil {
		return ast.IfStmt{}, e
	}

	condExpr, e = s.binaryExprValidator(cond)
	if e != nil {
		return ast.IfStmt{}, e
	}

	stmts, e = s.parseBlockStmt()
	if e != nil {
		return ast.IfStmt{}, e
	}

	els, e := s.ParseStatement()
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

func (s *StmtMaster) parseCheckStmt() (ast.CheckStmt, error) {
	pos := s.getPosition()

	checkId := pos.GetCoordinate()
	e := s.consume(token.CHECK)
	if e != nil {
		return ast.CheckStmt{}, e
	}

	tag, e := s.exprHandler()
	if e != nil {
		return ast.CheckStmt{}, e
	}

	tagExpr, ok := tag.(ast.Expr)
	if !ok {
		return ast.CheckStmt{}, errors.New(" parser eror: \"cek\" butuh sebuah ekspresi ")
	}

	s.parseRuntime.UseJumper(true)
	s.runtimeContainer.SetJumperGroupId(checkId)
	block, e := s.parseBlockStmt()
	s.parseRuntime.UseJumper(false)
	if e != nil {
		return ast.CheckStmt{}, e
	}

	return ast.CheckStmt{
		Loc:  pos,
		Body: block,
		Tag:  tagExpr,
	}, nil
}

func (s *StmtMaster) parseBlockStmt() (ast.BlockStmt, error) {
	tok := s.getCurrentToken()
	block := ast.BlockStmt{List: []ast.Stmt{}}

	if tok.Kind == token.LBRACE {
		pos := s.getPosition()

		e := s.consume(token.LBRACE)
		if e != nil {
			return ast.BlockStmt{}, e
		}
		for tok.Kind != token.RBRACE {
			var newStmt ast.Stmt
			newStmt, e = s.ParseStatement()
			if e != nil {
				return block, e
			}

			block.List = append(block.List, newStmt)

			tok = s.getCurrentToken()
		}

		e = s.consume(token.RBRACE)
		if e != nil {
			return block, e
		}

		block.Loc = pos
		return block, nil
	}

	return ast.BlockStmt{}, nil
}

var checkClauseBrake = map[token.Kind]struct{}{
	token.IF:     {},
	token.ELSE:   {},
	token.RBRACE: {},
}

func (s *StmtMaster) parseCaseClause() (ast.CaseClause, error) {
	var (
		e              error
		exprList       []ast.Expr
		stmtList       []ast.Stmt
		node           ast.Stmt
		jumperId       string
		parentJumperId string
	)

	pos := s.getPosition()

	e = s.consume(token.IF)
	if e != nil {
		return ast.CaseClause{}, e
	}

	cond, e := s.exprHandler()
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
			default:
				return ast.CaseClause{}, errors.New(" parser eror: jenis basic literal aneh di cek statement, ini eror internal")
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
		return ast.CaseClause{}, errors.New(" parser eror: jumper id ga ketemu di cek statement, ini eror internal")
	}

	if s.getCurrentToken().Kind == token.COMMA {
		for s.getCurrentToken().Kind != token.COLON {
			cond, e = s.exprHandler()
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
					default:
						return ast.CaseClause{}, errors.New(" parser eror: jenis basic literal aneh di cek statement, ini eror internal")
					}
					jumperId = prefix + typed.Tok.Val
				case ast.Ident:
					jumperId = "ident-" + typed.Loc.GetCoordinate()
				case ast.ParenExpr:
					jumperId = "paren-" + typed.Loc.GetCoordinate()
				}

				s.runtimeContainer.SetJumperAlias(jumperId, parentJumperId)
			}

			if s.getCurrentToken().Kind != token.COLON {
				e = s.consume(token.COMMA)
				if e != nil {
					return ast.CaseClause{}, e
				}
			}
		}
	}

	if s.getCurrentToken().Kind != token.COLON {
		return ast.CaseClause{}, errors.New(" parser eror: nilai cek statment butuh \":\"")
	}

	e = s.consume(token.COLON)
	if e != nil {
		return ast.CaseClause{}, e
	}

	_, stop := checkClauseBrake[s.getCurrentToken().Kind]

	for !stop {
		node, e = s.ParseStatement()
		if e != nil {
			return ast.CaseClause{}, e
		}

		stmtList = append(stmtList, node)

		_, stop = checkClauseBrake[s.getCurrentToken().Kind]
	}

	s.runtimeContainer.SetJumperByKey(parentJumperId, stmtList)

	return ast.CaseClause{
		Loc:  pos,
		List: exprList,
		Body: stmtList,
	}, nil
}

func (s *StmtMaster) parseDefaultCaseClause() (ast.CaseClause, error) {
	var (
		e        error
		node     ast.Stmt
		exprList []ast.Expr
		stmtList []ast.Stmt
	)

	pos := s.getPosition()

	e = s.consume(token.ELSE)
	if e != nil {
		return ast.CaseClause{}, e
	}

	if s.getCurrentToken().Kind != token.COLON {
		return ast.CaseClause{}, errors.New(" parser eror: nilai default cek statment butuh \":\"")
	}

	e = s.consume(token.COLON)

	_, stop := checkClauseBrake[s.getCurrentToken().Kind]

	for !stop {
		node, e = s.ParseStatement()
		if e != nil {
			return ast.CaseClause{}, e
		}

		stmtList = append(stmtList, node)

		_, stop = checkClauseBrake[s.getCurrentToken().Kind]
	}

	s.runtimeContainer.SetJumperDefaultClause(stmtList)

	return ast.CaseClause{
		Loc:  pos,
		List: exprList,
		Body: stmtList,
	}, nil
}

func (s *StmtMaster) parseFor() (ast.Stmt, error) {
	pos := s.getPosition()

	var (
		cond     ast.Node
		condExpr ast.Expr
		stmts    ast.BlockStmt
		e        error
	)

	e = s.consume(token.FOR)
	if e != nil {
		return nil, e
	}

	e = s.consume(token.IF)
	if e != nil {
		return nil, e
	}

	cond, e = s.exprHandler()
	if e != nil {
		return nil, e
	}

	condExpr, e = s.binaryExprValidator(cond)
	if e != nil {
		return nil, e
	}

	stmts, e = s.parseBlockStmt()
	if e != nil {
		return nil, e
	}

	return ast.ForStmt{
		Loc:  pos,
		Body: stmts,
		Cond: condExpr,
	}, nil
}

var incDecNeeds = map[token.Kind]struct{}{
	token.INC: {},
	token.DEC: {},
}

func (s *StmtMaster) parseIncDec() (ast.Node, error) {
	pos := s.getPosition()

	node, e := s.exprHandler()
	if e != nil {
		return nil, e
	}

	if _, isNeeded := incDecNeeds[s.getCurrentToken().Kind]; isNeeded {
		if typed, ok := node.(ast.Ident); ok {
			tok := s.getCurrentToken()
			e = s.consume(tok.Kind)
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

var assignNeeds = map[token.Kind]struct{}{
	token.DEFINE: {},
	token.ASSIGN: {},
}

func (s *StmtMaster) parseAssignStmt() (ast.Node, error) {
	pos := s.getPosition()

	node, e := s.parseIncDec()
	if e != nil {
		return nil, e
	}

	switch v := node.(type) {
	case ast.Ident:
		if _, needed := assignNeeds[s.getCurrentToken().Kind]; needed {
			currentToken := s.getCurrentToken()
			e = s.consume(s.getCurrentToken().Kind)
			if e != nil {
				return nil, e
			}

			var rhs ast.Node
			rhs, e = s.parseIncDec()
			if e != nil {
				return nil, e
			}

			node = ast.AssignStmt{
				Loc: pos,
				LHS: v,
				Op:  currentToken,
				RHS: rhs,
			}
		} else if s.getCurrentToken().Kind == token.LPAREN {
			e = s.consume(token.LPAREN)
			if e != nil {
				return nil, e
			}

			e = s.consume(token.RPAREN)
			if e != nil {
				return nil, e
			}

			node = ast.ExprStmt{
				Loc: pos,
				X: ast.CallExpr{
					Loc:  pos,
					Args: nil,
					Fun:  v,
				},
			}
		}
	}

	return node, nil
}

func (s *StmtMaster) ParseStatement() (ast.Stmt, error) {
	if s.exprHandler == nil {
		return nil, errors.New(" parser eror: pengelola expr-nya ga ada, ini internal eror")
	}

	tok := s.getCurrentToken()
	switch tok.Kind {
	case token.PRT, token.PRTLN:
		return s.parsePrint()
	case token.IF:
		if s.parseRuntime.ShouldUseJumper() {
			return s.parseCaseClause()
		}
		return s.parseIf()
	case token.CHECK:
		return s.parseCheckStmt()
	case token.ELSE:
		if s.parseRuntime.ShouldUseJumper() {
			return s.parseDefaultCaseClause()
		}

		e := s.consume(token.ELSE)
		if e != nil {
			return nil, e
		}

		if s.getCurrentToken().Kind == token.IF {
			return s.parseIf()
		}

		return s.parseBlockStmt()
	case token.FOR:
		return s.parseFor()
	case token.BREAK, token.CONTINUE:
		return s.parseSimplest()
	default:
		node, e := s.parseAssignStmt()
		if e != nil {
			return nil, e
		}

		if typed, ok := node.(ast.Stmt); ok {
			return typed, nil
		}
	}

	return nil, nil
}
