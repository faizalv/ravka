package ast

import (
	"ravka/token"
	"strconv"
)

type Position struct {
	X int
	Y int
}

func (p Position) GetCoordinate() string {
	return strconv.Itoa(p.X) + ":" + strconv.Itoa(p.Y)
}

type Node interface {
	node()
}

// Decl begin
type Decl interface {
	Node
	nodeDecl()
}

type Decls struct {
	Loc Position
	Val []Decl
}

type FuncDecl struct {
	Loc  Position
	Body BlockStmt
	Name Ident
	Tok  *token.Token
	Type FuncType
}

func (FuncDecl) nodeDecl() {}

// Decl end

// Expr begin
type Expr interface {
	Node
	nodeExpr()
}

type Ident struct {
	Loc  Position
	Name string
}

type BinaryExpr struct {
	Loc Position
	X   Node
	Op  *token.Token
	Y   Node
}

type UnaryExpr struct {
	Loc Position
	X   Node
	Op  *token.Token
}

type ParenExpr struct {
	Loc Position
	X   Node
}

type BasicLit struct {
	Loc Position
	Tok *token.Token
}

type FuncType struct {
	Loc    Position
	Params []Ident
}

type CallExpr struct {
	Loc  Position
	Args []Expr
	Fun  Ident
}

func (BinaryExpr) nodeExpr() {}
func (UnaryExpr) nodeExpr()  {}
func (ParenExpr) nodeExpr()  {}
func (Ident) nodeExpr()      {}
func (BasicLit) nodeExpr()   {}
func (FuncType) nodeExpr()   {}
func (CallExpr) name() {

}

// Expr end

// Stmt begin
type Stmt interface {
	Node
	nodeStmt()
}

type BlockStmt struct {
	Loc  Position
	List []Stmt
}

type AssignStmt struct {
	Loc Position
	LHS Ident
	Op  *token.Token
	RHS Node
}

type PrintStmt struct {
	Loc Position
	Val Node
	Tok *token.Token
}

type IfStmt struct {
	Loc  Position
	Body BlockStmt
	Cond Expr
	Else Stmt
}

type CheckStmt struct {
	Loc  Position
	Body BlockStmt
	Tag  Expr
}

type ForStmt struct {
	Loc  Position
	Body BlockStmt
	Cond Expr
}

type BranchStmt struct {
	Loc Position
	Tok *token.Token
}

type CaseClause struct {
	Loc  Position
	List []Expr
	Body []Stmt
}

type IncDecStmt struct {
	Loc Position
	Tok *token.Token
	X   Ident
}

type ExprStmt struct {
	Loc Position
	X   Expr
}

func (BlockStmt) nodeStmt()  {}
func (AssignStmt) nodeStmt() {}
func (PrintStmt) nodeStmt()  {}
func (IfStmt) nodeStmt()     {}
func (CheckStmt) nodeStmt()  {}
func (CaseClause) nodeStmt() {}
func (ForStmt) nodeStmt()    {}
func (BranchStmt) nodeStmt() {}
func (IncDecStmt) nodeStmt() {}
func (ExprStmt) nodeStmt()   {}

// Stmt end

func (BlockStmt) node()  {}
func (AssignStmt) node() {}
func (PrintStmt) node()  {}
func (IfStmt) node()     {}
func (CheckStmt) node()  {}
func (CaseClause) node() {}
func (ForStmt) node()    {}
func (BranchStmt) node() {}
func (IncDecStmt) node() {}
func (BinaryExpr) node() {}
func (UnaryExpr) node()  {}
func (ParenExpr) node()  {}
func (Ident) node()      {}
func (Decls) node()      {}
func (FuncDecl) node()   {}
func (BasicLit) node()   {}
func (FuncType) node()   {}
