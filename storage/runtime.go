package storage

type RuntimeStorageInterface interface {
	SetStack(id string)
	GetStackId()
	StoreExpr()
	GetExpr()
}
