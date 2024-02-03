package storage

import (
	"errors"
	"ravka/ast"
)

type JumperInterface interface {
	SetGroupId(id string)
	SetByKey(keyVal string, stmt []ast.Stmt)
	GetByKey(keyVal string) ([]ast.Stmt, error)
	HasDefault(groupId string) bool
	SetDefault(stmt []ast.Stmt)
	GetDefault() ([]ast.Stmt, error)
	SetAlias(alias string, targetKey string)
	CleanUp()
}

type JumperColumn struct {
	stmt []ast.Stmt
}

type JumperTable map[string]JumperColumn
type JumperAliasTable map[string]string

type Jumper struct {
	jumpers map[string]JumperTable
	aliases JumperAliasTable
	groupId string
}

func NewJumper() *Jumper {
	return &Jumper{
		jumpers: make(map[string]JumperTable),
		aliases: make(JumperAliasTable),
	}
}

func (j *Jumper) SetGroupId(id string) {
	j.groupId = id

	if j.jumpers[id] == nil {
		j.jumpers[id] = make(JumperTable)
	}
}

func (j *Jumper) SetByKey(keyVal string, stmt []ast.Stmt) {
	j.jumpers[j.groupId][keyVal] = JumperColumn{stmt: stmt}
}

func (j *Jumper) GetByKey(keyVal string) ([]ast.Stmt, error) {
	if target, isAlias := j.aliases[keyVal]; isAlias {
		keyVal = target
	}

	if v, exists := j.jumpers[j.groupId][keyVal]; exists {
		return v.stmt, nil
	}

	return nil, errors.New(" jumper eror: jumper ga ketemu")
}

func (j *Jumper) HasDefault(groupId string) bool {
	_, exists := j.jumpers[groupId]["default"]
	return exists
}

func (j *Jumper) SetDefault(stmt []ast.Stmt) {
	j.jumpers[j.groupId]["default"] = JumperColumn{stmt: stmt}
}

func (j *Jumper) GetDefault() ([]ast.Stmt, error) {
	if v, exists := j.jumpers[j.groupId]["default"]; exists {
		return v.stmt, nil
	}

	return nil, errors.New(" jumper eror: jumper default ga ada")
}

func (j *Jumper) SetAlias(alias string, targetKey string) {
	j.aliases[alias] = targetKey
}

func (j *Jumper) CleanUp() {
	j.jumpers = make(map[string]JumperTable)
	j.aliases = make(JumperAliasTable)
}
