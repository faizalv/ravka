package main

import (
	"bufio"
	"os"
	"strings"

	"ravka/interpreter"
	"ravka/lexer"
	"ravka/parser"
	"ravka/runtime"
	"ravka/storage"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	symbol := storage.NewSymbol()
	jumper := storage.NewJumper()
	lx := lexer.NewLexer()
	parseRuntime := runtime.NewParseRuntime(lx)
	mainRuntimeContainer := runtime.GetRuntimeContainer(parseRuntime, symbol, jumper)
	exprParser := parser.NewExprMaster(mainRuntimeContainer)
	stmtParser := parser.NewStmtMaster(mainRuntimeContainer)
	languageParser := parser.NewParser(mainRuntimeContainer, stmtParser, exprParser)
	stdout()
	for scanner.Scan() {
		input := scanner.Text()
		cmd := strings.Fields(input)

		if input == "" {
			stdout()
			continue
		}

		var e error
		var file *os.File
		inputType := lexer.STRING

		if cmd[0] == "baca" {
			if len(cmd) == 2 {
				file, e = os.Open(cmd[1])
				if e != nil {
					println(e.Error())
					stdout()
					continue
				}

				inputType = lexer.FILE
				jumper.CleanUp()
			} else {
				print("Empty bro")
				stdout()
				continue

			}
		}

		if input == "keluar" {
			println("bye!")
			break
		}

		vst := interpreter.NewVisitor(symbol, jumper)
		lx.SetInputType(inputType)
		if inputType == lexer.FILE {
			e = lx.PrepareFile(file)
			vst.ChangeVisitMode(interpreter.VisitModeMultiLines)
		} else {
			e = lx.PrepareString(input)
			vst.ChangeVisitMode(interpreter.VisitModeSingleLine)
		}

		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		e = languageParser.Prepare()
		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		engine := interpreter.NewInterpreter(languageParser, vst)
		e = engine.Interpret()
		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		stdout()
		mainRuntimeContainer.Flush()
	}
}

func stdout() {
	print("perintah_$ ")
}
