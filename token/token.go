package token

type Kind int

const (
	ILLEGAL Kind = iota
	EOF
	BREAKL

	literal_beg
	INT    // 123
	STRING // "halo"
	literal_end

	operator_beg
	ADD
	SUB
	MUL
	DIV

	LAND
	LOR
	INC
	DEC

	DEFINE
	ASSIGN

	EQL
	LSS
	LEQ
	GTR
	GEQ
	NEQ

	NOT

	LPAREN
	LBRACE

	RPAREN
	RBRACE

	PERIOD
	COMMA
	COLON
	operator_end

	IDENT

	keyword_beg
	FUNC

	TRUE
	FALSE

	RETURN
	CONTINUE
	BREAK

	IF
	ELSE
	FOR
	CHECK

	PRT
	PRTLN

	NIL
	keyword_end
)

var tokens = [...]string{
	ILLEGAL: "ILLEGAL",
	EOF:     "EOF",
	BREAKL:  "BREAKL",

	INT:    "INT",
	STRING: "STR",

	ADD: "+",
	SUB: "-",
	MUL: "*",
	DIV: "/",

	LAND: "&&",
	LOR:  "||",
	INC:  "++",
	DEC:  "--",

	DEFINE: ":=",
	ASSIGN: "=",

	EQL: "==",
	LSS: "<",
	LEQ: "<=",
	GTR: ">",
	GEQ: ">=",
	NEQ: "!=",

	NOT: "!",

	LPAREN: "(",
	LBRACE: "{",

	RPAREN: ")",
	RBRACE: "}",

	PERIOD: ".",
	COMMA:  ",",
	COLON:  ":",

	IDENT: "id",

	FUNC: "fungsi",

	TRUE:  "bener",
	FALSE: "salah",

	RETURN:   "kasi",
	CONTINUE: "lanjut",
	BREAK:    "berhenti",

	IF:    "kalo",
	ELSE:  "lainnya",
	FOR:   "ulangi",
	CHECK: "cek",

	PRT:   "cetak",
	PRTLN: "cetakb",

	NIL: "kosong",
}

var keywords map[string]Kind

func init() {
	keywords = make(map[string]Kind, keyword_end-(keyword_beg+1))
	for i := keyword_beg + 1; i < keyword_end; i++ {
		keywords[tokens[i]] = i
	}
}

type Token struct {
	Kind
	Val string
}

func NewToken(kind Kind, V string) Token {
	tok := Token{
		kind,
		V,
	}

	return tok
}

func (t Token) isReservedKeyword() bool {
	return t.Kind < keyword_end && t.Kind > keyword_beg
}

func (t Token) IsComparisonOperator() bool {
	return t.Kind >= EQL && t.Kind <= NEQ
}

func (t Token) IsArithmeticOperator() bool {
	return t.Kind >= ADD && t.Kind <= DIV
}

func (t Token) IsBoolOperator() bool {
	return t.Kind == LAND || t.Kind == LOR
}

func (t Token) isLiteral() bool {
	return t.Kind < literal_end && t.Kind > literal_beg
}

func GetKeywords() map[string]Kind {
	return keywords
}

func IsBoolKeyword(val string) bool {
	return val == tokens[TRUE] || val == tokens[FALSE]
}
func CheckBoolVal(val string) bool {
	return val == tokens[TRUE]
}
