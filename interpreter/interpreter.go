package interpreter

import (
	"ravka/parser"
)

type Interpreter struct {
	parser  *parser.Parser
	visitor *Visitor
}

func NewInterpreter(parser *parser.Parser, visitor *Visitor) *Interpreter {
	return &Interpreter{
		parser,
		visitor,
	}
}

func (i *Interpreter) Interpret() error {
	parsed, e := i.parser.Parse()
	if e != nil {
		return e
	}

	return i.visitor.Start(parsed)
}
