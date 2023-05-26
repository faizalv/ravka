package main

import (
	"bufio"
	"os"
	"strings"

	"ravka/interpreter"
	"ravka/lexer"
	"ravka/parser"
	"ravka/storage"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	symbol := storage.NewSymbol()
	jumper := storage.NewJumper()
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
				symbol = storage.NewSymbol()
				jumper = storage.NewJumper()
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
		lx := lexer.NewLexer(inputType)
		if inputType == lexer.FILE {
			e = lx.PrepareFile(file)
			vst.ChangeVisitMode(interpreter.VisitModeMultiLines)
		} else {
			e = lx.PrepareString(input)
			vst.ChangeVisitMode(interpreter.VisitModeOneLine)
		}

		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		prs, e := parser.NewParser(lx, jumper)
		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		engine := interpreter.NewInterpreter(prs, vst)
		e = engine.Interpret()
		if e != nil {
			println(e.Error())
			stdout()
			continue
		}

		stdout()
	}
}

func stdout() {
	print("input$ ")
}
