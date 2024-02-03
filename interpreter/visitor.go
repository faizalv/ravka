package interpreter

import (
	"errors"
	"strconv"

	"ravka/ast"
	"ravka/storage"
	"ravka/token"
)

const (
	VisitModeSingleLine int = iota
	VisitModeMultiLines
)

type crateValType int

const (
	crateValInt crateValType = iota + 11
	crateValString
	crateValBool
)

type crate struct {
	Val  string
	Type crateValType
}

type Visitor struct {
	visitMode int
	symbol    storage.SymbolInterface
	jumper    storage.JumperInterface
}

func NewVisitor(ss storage.SymbolInterface, jm storage.JumperInterface) *Visitor {
	return &Visitor{
		VisitModeSingleLine,
		ss,
		jm,
	}
}

func (v *Visitor) ChangeVisitMode(visitMode int) {
	v.visitMode = visitMode
}

func (v *Visitor) visitParenExpr(node ast.ParenExpr) (*crate, error) {
	x, e := v.visit(node.X)
	if e != nil {
		return nil, e
	}

	return x, nil
}

func (v *Visitor) visitAssignStmt(node ast.AssignStmt) error {
	lhs := node.LHS
	rhs, e := v.visit(node.RHS)
	if e != nil {
		return e
	}

	valKind := token.STRING

	if rhs.Type == crateValBool {
		valKind = token.FALSE
		if rhs.Val == "1" {
			valKind = token.TRUE
		}
	} else if rhs.Type == crateValInt {
		valKind = token.INT
	}

	v.symbol.StoreVar(lhs.Name, rhs.Val, valKind)

	return nil
}

func (v *Visitor) visitCompareExpr(node ast.BinaryExpr) (*crate, error) {
	lhs, e := v.visit(node.X)
	if e != nil {
		return nil, e
	}

	rhs, e := v.visit(node.Y)
	if e != nil {
		return nil, e
	}

	var (
		lhsInt, rhsInt  int
		lhse, rhse      error
		compareAsString bool
		res             bool
	)

	finalRes := &crate{
		Val:  "0",
		Type: crateValBool,
	}

	lhsInt, lhse = strconv.Atoi(lhs.Val)
	rhsInt, rhse = strconv.Atoi(rhs.Val)

	compareAsString = lhse != nil || rhse != nil

	switch node.Op.Kind {
	case token.EQL:
		if compareAsString {
			res = lhs.Val == rhs.Val
		} else {
			res = lhsInt == rhsInt
		}
		break
	case token.LEQ:
		if compareAsString {
			res = lhs.Val <= rhs.Val
		} else {
			res = lhsInt <= rhsInt
		}
		break
	case token.LSS:
		if compareAsString {
			res = lhs.Val < rhs.Val
		} else {
			res = lhsInt < rhsInt
		}
		break
	case token.GEQ:
		if compareAsString {
			res = lhs.Val >= rhs.Val
		} else {
			res = lhsInt >= rhsInt
		}
		break
	case token.GTR:
		if compareAsString {
			res = lhs.Val > rhs.Val
		} else {
			res = lhsInt > rhsInt
		}
	}

	if res {
		finalRes.Val = "1"
	}

	return finalRes, nil
}

func (v *Visitor) visitArithmeticExpr(node ast.BinaryExpr) (*crate, error) {
	lhs, e := v.visit(node.X)
	if e != nil {
		return nil, e
	}

	rhs, e := v.visit(node.Y)
	if e != nil {
		return nil, e
	}

	var (
		lhsInt, rhsInt     int
		lhse, rhse         error
		result             string
		isConcatenatingStr bool
	)

	lhsInt, lhse = strconv.Atoi(lhs.Val)
	rhsInt, rhse = strconv.Atoi(rhs.Val)

	if node.Op.Kind != token.ADD {
		if lhse != nil || rhse != nil {
			return nil, errors.New(" logic eror: butuh angka")
		}
	}

	isConcatenatingStr = lhse != nil || rhse != nil

	switch node.Op.Kind {
	case token.ADD:
		if isConcatenatingStr {
			result = lhs.Val + rhs.Val
		} else {
			result = strconv.Itoa(lhsInt + rhsInt)
		}
		break
	case token.SUB:
		result = strconv.Itoa(lhsInt - rhsInt)
		break
	case token.MUL:
		result = strconv.Itoa(lhsInt * rhsInt)
		break
	case token.DIV:
		if rhsInt == 0 {
			return nil, errors.New(" logic eror: kok bisa dibagi 0?")
		}
		result = strconv.Itoa(lhsInt / rhsInt)
		break
	}

	return &crate{
		Val:  result,
		Type: crateValString,
	}, nil
}

func (v *Visitor) visitBoolOpExpr(node ast.BinaryExpr) (*crate, error) {
	var (
		lhs, rhs *crate
		e        error
	)

	crateVal := &crate{
		Val:  "0",
		Type: crateValBool,
	}

	lhs, e = v.visit(node.X)
	if e != nil {
		return nil, e
	} else if lhs.Type != crateValBool {
		return nil, errors.New(" logic eror: bukan sebuah bool")
	}

	switch node.Op.Kind {
	case token.LOR:
		if lhs.Val == "1" {
			crateVal.Val = "1"
		} else {
			rhs, e = v.visit(node.Y)
			if e != nil {
				return nil, e
			} else if lhs.Type != crateValBool {
				return nil, errors.New(" logic eror: bukan sebuah bool")
			}

			if rhs.Val == "1" {
				crateVal.Val = "1"
			}
		}
		break
	case token.LAND:
		if lhs.Val == "1" {
			rhs, e = v.visit(node.Y)
			if e != nil {
				return nil, e
			} else if lhs.Type != crateValBool {
				return nil, errors.New(" logic eror: bukan sebuah bool")
			}

			if rhs.Val == "1" {
				crateVal.Val = "1"
			}
		}
	}

	return crateVal, e
}

func (v *Visitor) visitBinaryExpr(node ast.BinaryExpr) (*crate, error) {
	tok := node.Op

	if tok.IsComparisonOperator() {
		return v.visitCompareExpr(node)
	} else if tok.IsBoolOperator() {
		return v.visitBoolOpExpr(node)
	} else {
		return v.visitArithmeticExpr(node)
	}
}

func (v *Visitor) visitUnaryExpr(node ast.UnaryExpr) (*crate, error) {
	tok := node.Op
	x, e := v.visit(node.X)
	if e != nil {
		return nil, e
	}

	switch tok.Kind {
	case token.NOT:
		switch x.Type {
		case crateValBool:
			val := "1"
			if x.Val == "1" {
				val = "0"
			}
			return &crate{
				Val:  val,
				Type: crateValBool,
			}, nil
		}
		fallthrough
	default:
		var xInt int
		xInt, e = strconv.Atoi(x.Val)

		if e != nil {
			return nil, e
		}

		switch tok.Kind {
		case token.ADD:
			xInt = +xInt
			break
		case token.SUB:
			xInt = -xInt
			break
		}

		return &crate{
			Val:  strconv.Itoa(xInt),
			Type: crateValString,
		}, nil
	}
}

func (v *Visitor) visitBasicLit(node ast.BasicLit) (*crate, error) {
	typ := crateValString
	switch node.Tok.Kind {
	case token.INT:
		typ = crateValInt
		break
	case token.TRUE, token.FALSE:
		typ = crateValBool
	}

	return &crate{
		Val:  node.Tok.Val,
		Type: typ,
	}, nil
}

func (v *Visitor) visitIdent(node ast.Ident) (*crate, error) {
	if token.IsBoolKeyword(node.Name) {
		res := &crate{
			Val:  "0",
			Type: crateValBool,
		}

		if token.CheckBoolVal(node.Name) {
			res.Val = "1"
		}

		return res, nil
	}

	typ := crateValString

	tok, e := v.symbol.GetVar(node.Name)
	if e != nil {
		return nil, e
	}

	switch tok.Kind {
	case token.INT:
		typ = crateValInt
		break
	case token.TRUE, token.FALSE:
		typ = crateValBool
	}

	return &crate{
		Val:  tok.Val,
		Type: typ,
	}, nil
}

func (v *Visitor) visitBlockStmt(node ast.BlockStmt) error {
	for _, stmt := range node.List {
		_, e := v.visit(stmt)
		if e != nil {
			return e
		}
	}
	return nil
}

func (v *Visitor) visitFuncDeclStmt(node ast.FuncDecl) error {
	return v.visitBlockStmt(node.Body)
}

func (v *Visitor) visitPrintStmt(node ast.PrintStmt) error {
	val, e := v.visit(node.Val)
	if e != nil {
		return e
	}

	switch node.Tok.Kind {
	case token.PRT:
		print(val.Val)
		break
	case token.PRTLN:
		println(val.Val)
	}

	return nil
}

func (v *Visitor) visitCondExpr(node ast.Expr) (*crate, error) {
	var e error
	crt := &crate{
		Val:  "0",
		Type: crateValBool,
	}

	switch typed := node.(type) {
	case ast.BinaryExpr:
		crt, e = v.visitBinaryExpr(typed)
		if e != nil {
			return nil, e
		} else if crt.Type != crateValBool {
			println(crt.Type)
			return nil, errors.New(" logic eror: kondisional bermasalah")
		}
		break
	case ast.Ident:
		if token.CheckBoolVal(typed.Name) {
			crt.Val = "1"
		}
	default:
		return nil, errors.New(" logic eror: ekspresi gak menghasilkan bool")
	}

	return crt, e
}

func (v *Visitor) visitIfStmt(node ast.IfStmt) error {
	var (
		e    error
		cond *crate
	)
	cond, e = v.visitCondExpr(node.Cond)
	if e != nil {
		return e
	}

	if cond.Val == "1" {
		_, e = v.visit(node.Body)
		if e != nil {
			return e
		}
	} else {
		_, e = v.visit(node.Else)
		if e != nil {
			return e
		}
	}

	return nil
}

func (v *Visitor) visitForStmt(node ast.ForStmt) error {
	var (
		e    error
		cond *crate
	)
	cond, e = v.visitCondExpr(node.Cond)
	if e != nil {
		return e
	}

	for cond.Val == "1" {
		_, e = v.visit(node.Body)
		if e != nil {
			return e
		}

		cond, e = v.visitCondExpr(node.Cond)
		if e != nil {
			return e
		}
	}

	return nil
}

func (v *Visitor) visitCheckStmt(node ast.CheckStmt) error {
	tag := node.Tag
	locId := node.Loc.GetCoordinate()
	v.jumper.SetGroupId(locId)
	hasDefault := v.jumper.HasDefault(locId)

	var (
		e          error
		jumperId   string
		stmts      []ast.Stmt
		checkedVal ast.BasicLit
	)

	prefix := "s-"

	switch typed := tag.(type) {
	case ast.BasicLit:
		checkedVal = typed
	case ast.Ident:
		tok, e := v.symbol.GetVar(typed.Name)
		if e != nil {
			return e
		}

		switch tok.Kind {
		case token.INT:
			fallthrough
		case token.STRING:
			checkedVal = ast.BasicLit{
				Loc: ast.Position{},
				Tok: &tok,
			}
		default:
			return errors.New(" visitor eror: cek tag yang diberikan selain INT dan STRING")
		}
	}

	if checkedVal.Tok.Kind == token.INT {
		prefix = "i-"
	}

	jumperId = prefix + checkedVal.Tok.Val

	stmts, e = v.jumper.GetByKey(jumperId)
	if e != nil {
		if hasDefault {
			stmts, _ = v.jumper.GetDefault()
		} else {
			return e
		}
	}

	for _, stmt := range stmts {
		_, e = v.visit(stmt)
		if e != nil {
			return e
		}
	}

	return nil
}

func (v *Visitor) visitIncDecStmt(node ast.IncDecStmt) error {
	val, e := v.visit(node.X)
	if e != nil {
		return e
	}

	var valInt int
	if val.Type == crateValInt {
		valInt, e = strconv.Atoi(val.Val)
		if e != nil {
			return e
		}

		switch node.Tok.Kind {
		case token.INC:
			valInt++
			break
		case token.DEC:
			valInt--
			break
		}

		v.symbol.StoreVar(node.X.Name, strconv.Itoa(valInt), token.INT)
	}

	return nil
}

func (v *Visitor) visitDeclsStmt(node ast.Decls) error {
	ls := node.Val

	for _, l := range ls {
		_, e := v.visit(l)
		if e != nil {
			return e
		}
	}
	return nil
}

func (v *Visitor) visit(node ast.Node) (*crate, error) {
	if v.visitMode != VisitModeSingleLine && v.visitMode != VisitModeMultiLines {
		return nil, errors.New(" visitor eror: visit mode tidak terdefinisi")
	}

	switch nTyped := node.(type) {
	case ast.Decls:
		return nil, v.visitDeclsStmt(nTyped)
	case ast.FuncDecl:
		return nil, v.visitFuncDeclStmt(nTyped)
	case ast.BlockStmt:
		return nil, v.visitBlockStmt(nTyped)
	case ast.IncDecStmt:
		return nil, v.visitIncDecStmt(nTyped)
	case ast.AssignStmt:
		return nil, v.visitAssignStmt(nTyped)
	case ast.IfStmt:
		return nil, v.visitIfStmt(nTyped)
	case ast.CheckStmt:
		return nil, v.visitCheckStmt(nTyped)
	case ast.ForStmt:
		return nil, v.visitForStmt(nTyped)
	case ast.PrintStmt:
		return nil, v.visitPrintStmt(nTyped)
	case ast.BinaryExpr:
		return v.visitBinaryExpr(nTyped)
	case ast.UnaryExpr:
		return v.visitUnaryExpr(nTyped)
	case ast.ParenExpr:
		return v.visitParenExpr(nTyped)
	case ast.BasicLit:
		return v.visitBasicLit(nTyped)
	case ast.Ident:
		return v.visitIdent(nTyped)
	}

	return nil, nil
}

func (v *Visitor) Start(node ast.Node) error {
	_, e := v.visit(node)
	return e
}
