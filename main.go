package main

import (
	"bufio"
	"fmt"
	"io"
	"os"

	"github.com/youngfr/mlox/interpreter"
	"github.com/youngfr/mlox/parser"
	"github.com/youngfr/mlox/scanner"
)

func main() {
	switch n := len(os.Args); n {
	case 1:
		runPrompt()
	case 2:
		runFile(os.Args[1])
	default:
		fmt.Println("Usage: mlox OR mlox [script]")
		os.Exit(64)
	}
}

func runFile(path string) {
	file, err := os.Open(path)
	if err != nil {
		panic("Open " + path + " Failed")
	}
	defer file.Close()
	source, err := io.ReadAll(file)
	if err != nil {
		panic("Read " + file.Name() + " Failed")
	}
	sc := scanner.NewScanner(string(source))
	tokens, _ := sc.ScanTokens()
	var stmts = parser.NewParser(tokens).Parse()
	// parser.PrintAST(stmts)
	interpreter.NewInterpreter().Interpret(stmts)
}

func runPrompt() {
	fmt.Println(`Welcome to MLOX-REPL. Type "exit" or "Ctrl+Z" to exit.`)
	inputsc := bufio.NewScanner(os.Stdin)
	ir := interpreter.NewInterpreter()
	fmt.Print(">>> ")
loop:
	for inputsc.Scan() {
		switch line := inputsc.Text(); line {
		case "exit":
			break loop
		case "":
			fmt.Print(">>> ")
		default:
			tokens, _ := scanner.NewScanner(line).ScanTokens()
			stmts := parser.NewParser(tokens).Parse()
			ir.Interpret(stmts)
			fmt.Print(">>> ")
		}
	}
}
