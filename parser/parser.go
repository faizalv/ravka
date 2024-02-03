package parser

import (
	"errors"

	"ravka/ast"
	"ravka/lexer"
	"ravka/runtime"
	"ravka/token"
)

type Parser struct {
	parseRuntime runtime.ParseInterface
	stmtMaster   StmtMasterInterface
	exprMaster   ExprMasterInterface
}

type EntrypointInterface interface {
	Parse() (ast.Node, error)
}

func NewParser(parseRuntime runtime.ParseInterface, stmtMaster StmtMasterInterface, exprMaster ExprMasterInterface) (*Parser, error) {
	instance := &Parser{
		parseRuntime: parseRuntime,
		stmtMaster:   stmtMaster,
		exprMaster:   exprMaster,
	}

	stmtMaster.SetBinaryExprValidator(exprMaster.ValidateBinaryExpr)
	stmtMaster.SetExprHandler(exprMaster.ParseExpr)
	exprMaster.SetStmtHandler(stmtMaster.ParseStatement)

	e := instance.parseRuntime.Init()
	if e != nil {
		return nil, e
	}

	return instance, nil
}

func (p *Parser) getCurrentToken() *token.Token {
	return p.parseRuntime.GetCurrentToken()
}

func (p *Parser) getPosition() ast.Position {
	return p.parseRuntime.GetParsePosition()
}

func (p *Parser) consume(tok token.Kind) error {
	return p.parseRuntime.ConsumeToken(tok)
}

func (p *Parser) parseFuncType() (ast.FuncType, error) {
	tok := p.getCurrentToken()

	if tok.Kind == token.LPAREN {
		pos := p.getPosition()

		e := p.consume(token.LPAREN)
		if e != nil {
			return ast.FuncType{}, e
		}

		fType := ast.FuncType{}
		fType.Loc = pos

		for p.getCurrentToken().Kind != token.RPAREN {
			if p.getCurrentToken().Kind == token.COMMA {
				e = p.consume(token.COMMA)
				if e != nil {
					return ast.FuncType{}, e
				}
				continue
			}

			identRaw, e := p.exprMaster.ParseDirectly(ExprIdent)
			if e != nil {
				return ast.FuncType{}, e
			}

			ident, ok := identRaw.(ast.Ident)
			if !ok {
				return ast.FuncType{}, errors.New(" parse eror: tipe fungsi butuh Ident, ini internal eror")
			}

			fType.Params = append(fType.Params, ident)
		}

		e = p.parseRuntime.ConsumeToken(token.RPAREN)
		if e != nil {
			return ast.FuncType{}, e
		}

		return fType, nil
	}

	return ast.FuncType{}, errors.New(" parser eror: daftar parameternya ga bisa dipahami")
}

func (p *Parser) parseFuncDecl() (ast.FuncDecl, error) {

	pos := p.getPosition()
	tok := p.getCurrentToken()
	if tok.Kind != token.FUNC {
		return ast.FuncDecl{}, errors.New(" parser eror: deklarasi fungsi ga jelas")
	}

	e := p.parseRuntime.ConsumeToken(token.FUNC)
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

	identRaw, e := p.exprMaster.ParseDirectly(ExprIdent)
	if e != nil {
		return ast.FuncDecl{}, e
	}
	ident, ok := identRaw.(ast.Ident)
	if !ok {
		return ast.FuncDecl{}, errors.New(" parse eror: fungsi butuh Ident, ini internal eror")
	}

	typ, e := p.parseFuncType()
	if e != nil {
		return ast.FuncDecl{}, e
	}

	blockRaw, e := p.stmtMaster.ParseDirectly(StmtBlock)
	if e != nil {
		return ast.FuncDecl{}, e
	}
	block, ok := blockRaw.(ast.BlockStmt)
	if !ok {
		return ast.FuncDecl{}, errors.New(" parse eror: fungsi butuh BlockStmt, ini internal eror")
	}

	fn.Name = ident
	fn.Type = typ
	fn.Body = block

	return fn, nil
}

func (p *Parser) parseDecls() (ast.Decls, error) {
	decls := ast.Decls{
		Loc: p.getPosition(),
		Val: []ast.Decl{},
	}

	for p.getCurrentToken().Kind != token.EOF {
		decl, e := p.parseFuncDecl()
		if e != nil {
			return ast.Decls{}, e
		}

		decls.Val = append(decls.Val, decl)
	}

	return decls, nil
}

func (p *Parser) parseMain() (ast.Node, error) {
	if p.parseRuntime.GetInputType() == lexer.STRING {
		return p.stmtMaster.ParseStatement()
	} else {
		return p.parseDecls()
	}
}

func (p *Parser) Parse() (ast.Node, error) {
	return p.parseMain()
}
