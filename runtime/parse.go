package runtime

import (
	"errors"

	"ravka/ast"
	"ravka/lexer"
	"ravka/token"
)

type ParseInterface interface {
	ConsumeToken(tok token.Kind) error
	GetCurrentToken() *token.Token
	GetParsePosition() ast.Position
	GetInputType() int
	ShouldUseJumper() bool
	UseJumper(state bool)
	Init() error
	Reset()
}

type ParseRuntime struct {
	lexer        lexer.Interface
	currentToken *token.Token
	useJumper    bool
}

func NewParseRuntime(lexer lexer.Interface) *ParseRuntime {
	return &ParseRuntime{
		lexer:        lexer,
		currentToken: nil,
		useJumper:    false,
	}
}

func (p *ParseRuntime) Init() error {
	if p.currentToken == nil {
		nextToken, e := p.lexer.GetNextToken()
		if e != nil {
			return e
		}

		p.currentToken = nextToken
	}

	return nil
}

func (p *ParseRuntime) Reset() {
	p.lexer.Flush()
	p.currentToken = nil
	p.useJumper = false
}

func (p *ParseRuntime) UseJumper(state bool) {
	p.useJumper = state
}

func (p *ParseRuntime) ShouldUseJumper() bool {
	return p.useJumper
}

func (p *ParseRuntime) ConsumeToken(kind token.Kind) error {
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

func (p *ParseRuntime) GetCurrentToken() *token.Token {
	return p.currentToken
}

func (p *ParseRuntime) GetParsePosition() ast.Position {
	return ast.Position{
		X: p.lexer.GetPosX(),
		Y: p.lexer.GetPosX(),
	}
}

func (p *ParseRuntime) GetInputType() int {
	return p.lexer.GetInputType()
}
