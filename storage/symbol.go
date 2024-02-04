package storage

import (
	"errors"

	"ravka/ast"
	"ravka/token"
)

type SymbolInterface interface {
	StoreVar(ident string, val string, kind token.Kind)
	GetVar(ident string) (token.Token, error)
	StoreFunc(ident string, block *ast.BlockStmt, typ *ast.FuncType)
	GetFunc(ident string) (*ast.BlockStmt, *ast.FuncType)
}

type varVal struct {
	Val  string
	Type token.Kind
}

type funVal struct {
	block *ast.BlockStmt
	typ   *ast.FuncType
}

type Symbol struct {
	vars map[string]varVal
	fns  map[string]funVal
}

func NewSymbol() *Symbol {
	return &Symbol{
		vars: make(map[string]varVal),
		fns:  make(map[string]funVal),
	}
}

func (s *Symbol) StoreVar(ident string, val string, kind token.Kind) {
	s.vars[ident] = varVal{
		Val:  val,
		Type: kind,
	}
}

func (s *Symbol) GetVar(ident string) (token.Token, error) {
	if v, exists := s.vars[ident]; exists {
		return token.NewToken(v.Type, v.Val), nil
	}

	return token.Token{}, errors.New(" variabel " + ident + " tidak terdefinisi")
}

func (s *Symbol) StoreFunc(ident string, block *ast.BlockStmt, typ *ast.FuncType) {
	s.fns[ident] = funVal{
		block: block,
		typ:   typ,
	}
}

func (s *Symbol) GetFunc(ident string) (*ast.BlockStmt, *ast.FuncType) {
	if f, exists := s.fns[ident]; exists {
		return f.block, f.typ
	}

	return nil, nil
}
