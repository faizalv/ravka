package lexer

import (
	"bufio"
	"errors"
	"os"
	"strconv"
	"unicode"

	"ravka/token"
)

const (
	FILE int = iota
	STRING
)

type AcceptedType interface {
	*os.File | string
}

type Interface interface {
	SetInputType(inputType int)
	Flush()
	PrepareFile(file *os.File) error
	PrepareString(input string) error
	GetNextToken() (*token.Token, error)
	GetInputType() int
	CheckRows() bool
	GetPosX() int
	GetPosY() int
}

type Lexer struct {
	inputType   int
	xPointer    int
	yPointer    int
	rows        []string
	currentChar *string
	currentRow  string
}

func NewLexer() *Lexer {
	return &Lexer{}
}

func (l *Lexer) SetInputType(inputType int) {
	l.inputType = inputType
}

func (l *Lexer) Flush() {
	l.xPointer = 0
	l.yPointer = 0
	l.rows = make([]string, 0)
	l.currentChar = nil
	l.currentRow = ""
}

func (l *Lexer) PrepareFile(file *os.File) error {
	if l.inputType != FILE {
		return errors.New(" lexer eror: siap-siap baca file, tapi ternyata bukan file")
	}

	bufioScanner := bufio.NewScanner(file)
	for bufioScanner.Scan() {
		l.rows = append(l.rows, bufioScanner.Text())
	}

	if len(l.rows) > 0 {
		l.currentRow = l.rows[0]
		l.xPointer = 1
		l.yPointer = 0
		currentChar := l.currentRow[0:1]
		l.currentChar = &currentChar
	}

	return nil
}

func (l *Lexer) PrepareString(input string) error {
	if l.inputType != STRING {
		return errors.New(" lexer eror: siap-siap baca string, tapi ternyata bukan string")
	}

	l.rows = append(l.rows, input)
	l.currentRow = input
	l.xPointer = 1
	l.yPointer = 0
	currentChar := input[0:1]
	l.currentChar = &currentChar

	return nil
}

func (l *Lexer) bindInteger() string {
	v := *l.currentChar
	_, e := strconv.Atoi(v)
	str := ""

	for e == nil {
		str += v
		l.movePointer()
		if l.currentChar == nil {
			break
		}

		v = *l.currentChar
		_, e = strconv.Atoi(v)
	}

	return str
}

func (l *Lexer) bindLetter() string {
	v := *l.currentChar
	str := ""

	for unicode.IsLetter([]rune(v)[0]) {
		str += v
		l.movePointer()
		if l.currentChar == nil {
			break
		}

		v = *l.currentChar
	}

	return str
}

func (l *Lexer) bindString() string {
	l.movePointer()
	v := *l.currentChar
	str := ""

	for v != "\"" {
		str += v
		l.movePointer()
		v = *l.currentChar
	}

	l.movePointer()
	return str
}

func (l *Lexer) ignoreWhitespace() {
	v := *l.currentChar
	for l.currentChar != nil && v == " " {
		l.movePointer()
		v = *l.currentChar
	}
}

func (l *Lexer) movePointer() {
	l.xPointer++

	if l.xPointer > len(l.currentRow) {
		currentChar := ""
		for currentChar == "" {
			l.yPointer++
			if l.yPointer < len(l.rows) {
				l.currentRow = l.rows[l.yPointer]
				l.xPointer = 1

				if l.currentRow == "" {
					continue
				}

				currentChar = l.currentRow[0:1]
				l.currentChar = &currentChar
			} else {
				l.currentChar = nil
				break
			}
		}
	} else {
		selected := l.currentRow[l.xPointer-1 : l.xPointer]
		l.currentChar = &selected
	}
}

var singleToken = map[string]token.Kind{
	",": token.COMMA,
	"-": token.SUB,
	"*": token.MUL,
	"/": token.DIV,
	"(": token.LPAREN,
	")": token.RPAREN,
	"{": token.LBRACE,
	"}": token.RBRACE,
	"!": token.NOT,
}

func (l *Lexer) GetNextToken() (*token.Token, error) {
	for l.currentChar != nil {
		currentChar := *l.currentChar
		if currentChar == " " {
			l.ignoreWhitespace()
			continue
		} else if kind, exists := singleToken[currentChar]; exists {
			newToken := token.NewToken(kind, currentChar)
			l.movePointer()
			return &newToken, nil
		} else if currentChar == "+" {
			var newToken token.Token
			l.movePointer()
			if *l.currentChar == "+" {
				newToken = token.NewToken(token.INC, "++")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.ADD, "+")
			}

			return &newToken, nil
		} else if currentChar == "-" {
			var newToken token.Token
			l.movePointer()
			if *l.currentChar == "-" {
				newToken = token.NewToken(token.DEC, "--")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.DEC, "-")
			}

			return &newToken, nil
		} else if currentChar == ":" {
			var (
				newToken token.Token
				e        error
			)

			l.movePointer()
			if *l.currentChar == "=" {
				newToken = token.NewToken(token.DEFINE, ":=")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.COLON, ":")
			}

			return &newToken, e
		} else if currentChar == "=" {
			var newToken token.Token
			l.movePointer()
			if *l.currentChar == "=" {
				newToken = token.NewToken(token.EQL, "==")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.ASSIGN, "=")
			}

			return &newToken, nil
		} else if currentChar == "&" {
			var (
				newToken token.Token
				e        error
			)

			l.movePointer()
			if *l.currentChar == "&" {
				newToken = token.NewToken(token.LAND, "&&")
				l.movePointer()
			} else {
				e = errors.New(" lexer eror: berharap & tapi dapet " + *l.currentChar)
			}

			return &newToken, e
		} else if currentChar == "|" {
			var (
				newToken token.Token
				e        error
			)

			l.movePointer()
			if *l.currentChar == "|" {
				newToken = token.NewToken(token.LOR, "||")
				l.movePointer()
			} else {
				e = errors.New(" lexer eror: berharap | tapi dapet " + *l.currentChar)
			}

			return &newToken, e
		} else if currentChar == ">" {
			var newToken token.Token
			l.movePointer()
			if *l.currentChar == "=" {
				newToken = token.NewToken(token.GEQ, ">=")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.GTR, ">")
			}

			return &newToken, nil
		} else if currentChar == "<" {
			var newToken token.Token
			l.movePointer()
			if *l.currentChar == "=" {
				newToken = token.NewToken(token.LEQ, "<=")
				l.movePointer()
			} else {
				newToken = token.NewToken(token.LSS, "<")
			}

			return &newToken, nil
		} else if currentChar == "\"" {
			str := l.bindString()
			newToken := token.NewToken(token.STRING, str)
			return &newToken, nil
		} else if unicode.IsLetter([]rune(currentChar)[0]) {
			ident := l.bindLetter()
			keywords := token.GetKeywords()
			var newToken token.Token
			if kind, exists = keywords[ident]; exists {
				newToken = token.NewToken(kind, ident)
			} else {
				newToken = token.NewToken(token.IDENT, ident)
			}
			return &newToken, nil
		} else if _, e := strconv.Atoi(currentChar); e == nil {
			fullNumeric := l.bindInteger()
			newToken := token.NewToken(token.INT, fullNumeric)
			return &newToken, nil
		}

		return nil, errors.New(" lexer eror: ga tau jenis token")
	}

	eof := token.NewToken(token.EOF, "")
	return &eof, nil
}

func (l *Lexer) GetInputType() int {
	return l.inputType
}

func (l *Lexer) CheckRows() bool {
	return l.yPointer < len(l.rows)
}

func (l *Lexer) GetPosX() int {
	return l.xPointer
}

func (l *Lexer) GetPosY() int {
	return l.yPointer + 1
}
