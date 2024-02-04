package runtime

import (
	"sync"

	"ravka/ast"
	"ravka/storage"
)

var once sync.Once

const MainEntrypointFuncName = "utama"

type ContainerInterface interface {
	GetParseRuntime() ParseInterface
	SetJumperGroupId(groupId string)
	SetJumperDefaultClause(listStmt []ast.Stmt)
	SetJumperAlias(alias string, targetKey string)
	SetJumperByKey(key string, stmtList []ast.Stmt)
	StoreFuncDecl(ident string, block *ast.BlockStmt, typ *ast.FuncType)
	GetFuncDecl(ident string) (*ast.BlockStmt, *ast.FuncType)
	Flush()
}

type Runtime struct {
	symbolTable  storage.SymbolInterface
	jumperTable  storage.JumperInterface
	parseRuntime ParseInterface
}

var runtime *Runtime

func GetRuntimeContainer(parseRuntime ParseInterface, symbolTable storage.SymbolInterface, jumperTable storage.JumperInterface) *Runtime {
	if runtime == nil {
		once.Do(func() {
			runtime = &Runtime{
				symbolTable:  symbolTable,
				jumperTable:  jumperTable,
				parseRuntime: parseRuntime,
			}
		})
	}

	return runtime
}

func (r *Runtime) GetParseRuntime() ParseInterface {
	return r.parseRuntime
}

func (r *Runtime) SetJumperGroupId(id string) {
	r.jumperTable.SetGroupId(id)
}

func (r *Runtime) SetJumperDefaultClause(stmtList []ast.Stmt) {
	r.jumperTable.SetDefault(stmtList)
}

func (r *Runtime) SetJumperAlias(alias string, targetKey string) {
	r.jumperTable.SetAlias(alias, targetKey)
}

func (r *Runtime) SetJumperByKey(key string, stmtList []ast.Stmt) {
	r.jumperTable.SetByKey(key, stmtList)
}

func (r *Runtime) StoreFuncDecl(ident string, block *ast.BlockStmt, typ *ast.FuncType) {
	r.symbolTable.StoreFunc(ident, block, typ)
}

func (r *Runtime) GetFuncDecl(ident string) (*ast.BlockStmt, *ast.FuncType) {
	return r.symbolTable.GetFunc(ident)
}

func (r *Runtime) Flush() {
	go r.parseRuntime.Reset()
	go r.jumperTable.CleanUp()
}
