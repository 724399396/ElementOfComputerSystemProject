package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
	"unicode"
)

type tokenType string

const (
	keyword         tokenType = "keyword"
	symbol                    = "symbol"
	identifier                = "identifier"
	integerConstant           = "integerConstant"
	stringConstant            = "stringConstant"
)

type syntaxType string

const (
	class            syntaxType = "class"
	classVarDec                 = "classVarDec"
	subroutineDec               = "subroutineDec"
	parameterList               = "parameterList"
	subroutineBody              = "subroutineBody"
	statements                  = "statements"
	letStatement                = "letStatement"
	ifStatement                 = "ifStatement"
	whileStatement              = "whileStatement"
	doStatement                 = "doStatement"
	returnStatement             = "returnStatement"
	expression                  = "expression"
	term                        = "term"
	subroutineCall              = "subroutineCall"
	expressionList              = "expressionList"
	sKeyword                    = "keyword"
	sSymbol                     = "symbol"
	sIdentifier                 = "identifier"
	sIntegerConstant            = "integerConstant"
	sStringConstant             = "stringConstant"
)

type token struct {
	tType tokenType
	val   string
}

type keywords []string

var allSymbols = map[byte]string{'{': "{", '}': "}", '(': "(", ')': ")", '[': "[", ']': "]", '.': ".", ',': ",", ';': ";", '+': "+", '-': "-",
	'*': "*", '/': "/", '&': "&amp;", '|': "|", '<': "&lt;", '>': "&gt;", '=': "=", '~': "~"}
var opSymbols = map[string]string{"+": "add", "-": "sub", "*": "call Math.multiply 2", "/": "call Math.divide 2", "&amp;": "and", "|": "or", "&lt;": "lt", "&gt;": "gt", "=": "eq"}
var allKeywords = keywords{"class", "constructor", "function", "method", "field", "static",
	"var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do",
	"if", "else", "while", "return"}
var ifLabelIndex = 0
var whileLabelIndex = 0

func readLines(inFile string) []*token {
	dat, err := ioutil.ReadFile(inFile)
	if err != nil {
		panic(err)
	}
	lines := strings.Split(string(dat), "\n")
	i := 0
	for _, line := range lines {
		commentIndex := strings.Index(line, "//")
		trimed := line
		if commentIndex >= 0 {
			trimed = line[:commentIndex]
		}
		trimed = strings.TrimSpace(trimed)
		if len(trimed) > 0 {
			lines[i] = trimed
			i++
		}
	}
	lines = lines[:i]

	i = 0
	inComment := false
	for _, line := range lines {
		if inComment {
			endCommentIndex := strings.Index(line, "*/")
			if endCommentIndex >= 0 {
				inComment = false
			}
		} else {
			commentIndex := strings.Index(line, "/**")
			if commentIndex >= 0 {

				endCommentIndex := strings.Index(line, "*/")
				if endCommentIndex >= 0 {
					inComment = false
				} else {
					inComment = true
				}
			} else {
				trimed := strings.TrimSpace(line)
				if len(trimed) > 0 {
					lines[i] = trimed
					i++
				}
			}
		}
	}
	lines = lines[:i]

	tokens := []*token{}
	for _, line := range lines {
		tokens = append(tokens, tokenizer(line)...)
	}
	return tokens
}

func (list keywords) Has(x string) bool {
	for _, y := range list {
		if y == x {
			return true
		}
	}
	return false
}

func tokenizer(line string) []*token {
	var res []*token
	pi := -1
	for i := 0; i < len(line); {
		if pi == i {
			fmt.Printf("%v %v\n", i, line)
			break
		}
		pi = i
		// empty space
		if line[i] == ' ' {
			i++
			continue
		}
		// symbol
		if s, ok := allSymbols[line[i]]; ok {
			res = append(res, &token{symbol, s})
			i++
			continue
		}
		// identifier
		if unicode.IsLetter(rune(line[i])) || line[i] == '_' {
			j := i
			for ; j < len(line); j++ {
				if unicode.IsLetter(rune(line[j])) || unicode.IsDigit(rune(line[j])) {

				} else {
					break
				}
			}
			word := line[i:j]
			i = j
			if allKeywords.Has(word) {
				res = append(res, &token{keyword, word})
			} else {
				res = append(res, &token{identifier, word})
			}
			continue
		}
		// string constant
		if line[i] == '"' {
			j := i + 1
			for ; j < len(line) && line[j] != '"'; j++ {
			}
			word := line[i+1 : j]
			i = j + 1
			res = append(res, &token{stringConstant, word})
			continue
		}
		// interger constant
		if unicode.IsDigit(rune(line[i])) {
			j := i
			for ; j < len(line); j++ {
				if unicode.IsDigit(rune(line[j])) || line[j] == '.' {
				} else {
					break
				}
			}
			word := line[i:j]
			i = j
			res = append(res, &token{integerConstant, word})
			continue
		}
	}
	return res
}

type symbolDefine struct {
	sType string
	fType string
	index int
}

var className string
var staticIndex = 0
var fieldIndex = 0
var classSymbols map[string]*symbolDefine = make(map[string]*symbolDefine)
var argumentIndex = 0
var localIndex = 0
var methodSymbols map[string]*symbolDefine = make(map[string]*symbolDefine)

func parser(tokens []*token, f *os.File) {
	leftToks := compileClass(tokens, f)
	if len(leftToks) > 0 {
		fmt.Println(leftToks[0].val)
		panic("not consume all token")
	}
}

func eat(tokens []*token, tType tokenType, val string) []*token {
	if (tType == "" || tokens[0].tType == tType) && (val == "" || tokens[0].val == val) {
		return tokens[1:]
	} else {
		fmt.Println(tokens[0])
		panic("not match")
	}
}

func compileClass(tokens []*token, f *os.File) []*token {
	staticIndex = 0
	fieldIndex = 0
	classSymbols = make(map[string]*symbolDefine)
	tokens = eat(tokens, keyword, "class")
	className = tokens[0].val
	currentClassName = className
	tokens = eat(tokens, identifier, "")
	tokens = eat(tokens, symbol, "{")

	for tokens[0].val == "static" || tokens[0].val == "field" {
		tokens = compileClassVarDec(tokens, f)
	}

	for tokens[0].val == "constructor" || tokens[0].val == "function" || tokens[0].val == "method" {
		tokens = compileSubroutineDec(tokens, f)
	}
	tokens = eat(tokens, symbol, "}")

	return tokens
}

func compileClassVarDec(tokens []*token, f *os.File) []*token {
	symbolType := tokens[0].val
	tokens = eat(tokens, keyword, tokens[0].val)
	fieldType := tokens[0].val
	updateClassSymbol(symbolType, fieldType, tokens[1].val)
	tokens = eat(tokens, "", "")
	tokens = eat(tokens, identifier, "")
	for tokens[0].val == "," {
		tokens = eat(tokens, symbol, ",")
		updateClassSymbol(symbolType, fieldType, tokens[0].val)
		tokens = eat(tokens, identifier, "")
	}
	tokens = eat(tokens, symbol, ";")
	return tokens
}

func updateClassSymbol(symbolType string, fieldType string, fieldName string) {
	switch symbolType {
	case "static":
		classSymbols[fieldName] = &symbolDefine{symbolType, fieldType, staticIndex}
		staticIndex++
	case "field":
		classSymbols[fieldName] = &symbolDefine{"this", fieldType, fieldIndex}
		fieldIndex++
	}
}

func compileSubroutineDec(tokens []*token, f *os.File) []*token {
	argumentIndex = 0
	localIndex = 0
	methodSymbols = make(map[string]*symbolDefine)
	funcType := tokens[0].val
	if funcType == "method" {
		updateMethodSymbol("argument", className, "this")
	}
	tokens = eat(tokens, keyword, tokens[0].val)
	tokens = eat(tokens, "", "")
	funcName := tokens[0].val
	tokens = eat(tokens, identifier, "")
	tokens = eat(tokens, symbol, "(")
	tokens = compileParameterList(tokens, f)
	tokens = eat(tokens, symbol, ")")
	tokens = compileSubroutineBody(tokens, f, funcType, funcName)
	return tokens
}

func compileParameterList(tokens []*token, f *os.File) []*token {
	if tokens[0].tType == identifier || tokens[0].tType == keyword {
		updateMethodSymbol("argument", tokens[0].val, tokens[1].val)
		tokens = eat(tokens, "", "")
		tokens = eat(tokens, identifier, "")
		for tokens[0].val == "," {
			tokens = eat(tokens, symbol, ",")
			updateMethodSymbol("argument", tokens[0].val, tokens[1].val)
			tokens = eat(tokens, "", "")
			tokens = eat(tokens, identifier, "")
		}
	}
	return tokens
}

func updateMethodSymbol(symbolType string, fieldType string, fieldName string) {
	switch symbolType {
	case "argument":
		methodSymbols[fieldName] = &symbolDefine{symbolType, fieldType, argumentIndex}
		argumentIndex++
	case "local":
		methodSymbols[fieldName] = &symbolDefine{symbolType, fieldType, localIndex}
		localIndex++
	}
}

func compileSubroutineBody(tokens []*token, f *os.File, funcType string, funcName string) []*token {
	tokens = eat(tokens, symbol, "{")
	for tokens[0].val == "var" {
		tokens = compileVarDec(tokens, f)
	}
	fmt.Fprintf(f, "function %s.%s %d\n", className, funcName, localIndex)
	if funcType == "constructor" {
		fmt.Fprintf(f, "push constant %d\n", fieldIndex)
		fmt.Fprintln(f, "call Memory.alloc 1")
		fmt.Fprintln(f, "pop pointer 0")
	}
	if funcType == "method" {
		fmt.Fprintln(f, "push argument 0")
		fmt.Fprintln(f, "pop pointer 0")
	}
	tokens = compileStatements(tokens, f)
	tokens = eat(tokens, symbol, "}")
	return tokens
}

func compileVarDec(tokens []*token, f *os.File) []*token {
	tokens = eat(tokens, keyword, "var")
	fieldType := tokens[0].val
	tokens = eat(tokens, "", "")
	updateMethodSymbol("local", fieldType, tokens[0].val)
	tokens = eat(tokens, identifier, "")
	for tokens[0].val == "," {
		tokens = eat(tokens, symbol, ",")
		updateMethodSymbol("local", fieldType, tokens[0].val)
		tokens = eat(tokens, identifier, "")
	}
	tokens = eat(tokens, symbol, ";")
	return tokens
}

func compileStatements(tokens []*token, f *os.File) []*token {
	for tokens[0].val == "let" || tokens[0].val == "if" || tokens[0].val == "while" || tokens[0].val == "do" || tokens[0].val == "return" {
		if tokens[0].val == "let" {
			tokens = compileLetStatement(tokens, f)
		}
		if tokens[0].val == "if" {
			tokens = compileIfStatement(tokens, f)
		}
		if tokens[0].val == "while" {
			tokens = compileWhileStatement(tokens, f)
		}
		if tokens[0].val == "do" {
			tokens = compileDoStatement(tokens, f)
		}
		if tokens[0].val == "return" {
			tokens = compileReturnStatement(tokens, f)
		}
	}
	return tokens
}

func findSymbol(name string) *symbolDefine {
	symbolInMethod, ok := methodSymbols[name]
	if ok {
		return symbolInMethod
	}
	return classSymbols[name]
}

func compileLetStatement(tokens []*token, f *os.File) []*token {
	tokens = eat(tokens, keyword, "let")
	targetSymbol := findSymbol(tokens[0].val)
	tokens = eat(tokens, identifier, "")
	var isArray = false
	if tokens[0].val == "[" {
		fmt.Fprintf(f, "push %s %d\n", targetSymbol.sType, targetSymbol.index)
		tokens = eat(tokens, symbol, "[")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, symbol, "]")
		isArray = true
		fmt.Fprintln(f, "add")
	}
	tokens = eat(tokens, symbol, "=")
	tokens = compileExpression(tokens, f)
	tokens = eat(tokens, symbol, ";")
	if isArray {
		fmt.Fprintln(f, "pop temp 0")
		fmt.Fprintln(f, "pop pointer 1")
		fmt.Fprintln(f, "push temp 0")
		fmt.Fprintln(f, "pop that 0")
	} else {
		fmt.Fprintf(f, "pop %s %d\n", targetSymbol.sType, targetSymbol.index)
	}
	return tokens
}

func compileIfStatement(tokens []*token, f *os.File) []*token {
	label := ifLabelIndex
	ifLabelIndex++

	tokens = eat(tokens, keyword, "if")
	tokens = eat(tokens, symbol, "(")
	tokens = compileExpression(tokens, f)
	tokens = eat(tokens, symbol, ")")
	tokens = eat(tokens, symbol, "{")
	fmt.Fprintf(f, "if-goto IF_TRUE%d\n", label)
	fmt.Fprintf(f, "goto IF_FALSE%d\n", label)
	fmt.Fprintf(f, "label IF_TRUE%d\n", label)
	tokens = compileStatements(tokens, f)
	tokens = eat(tokens, symbol, "}")
	fmt.Fprintf(f, "goto IF_END%d\n", label)
	fmt.Fprintf(f, "label IF_FALSE%d\n", label)
	if tokens[0].val == "else" {
		tokens = eat(tokens, keyword, "else")
		tokens = eat(tokens, symbol, "{")
		tokens = compileStatements(tokens, f)
		tokens = eat(tokens, symbol, "}")
	}
	fmt.Fprintf(f, "label IF_END%d\n", label)
	return tokens
}

func compileWhileStatement(tokens []*token, f *os.File) []*token {
	label := whileLabelIndex
	whileLabelIndex++

	tokens = eat(tokens, keyword, "while")
	tokens = eat(tokens, symbol, "(")
	fmt.Fprintf(f, "label WHILE_EXP%d\n", label)
	tokens = compileExpression(tokens, f)
	fmt.Fprintln(f, "not")
	fmt.Fprintf(f, "if-goto WHILE_END%d\n", label)
	tokens = eat(tokens, symbol, ")")
	tokens = eat(tokens, symbol, "{")
	tokens = compileStatements(tokens, f)
	fmt.Fprintf(f, "goto WHILE_EXP%d\n", label)
	tokens = eat(tokens, symbol, "}")
	fmt.Fprintf(f, "label WHILE_END%d\n", label)
	return tokens
}

func compileDoStatement(tokens []*token, f *os.File) []*token {
	tokens = eat(tokens, keyword, "do")
	tokens = compileSubroutineCall(tokens, f)
	fmt.Fprintln(f, "pop temp 0")
	tokens = eat(tokens, symbol, ";")
	return tokens
}

func compileReturnStatement(tokens []*token, f *os.File) []*token {
	tokens = eat(tokens, keyword, "return")
	if tokens[0].val != ";" {
		tokens = compileExpression(tokens, f)
	} else {
		fmt.Fprintln(f, "push constant 0")
	}
	fmt.Fprintln(f, "return")
	tokens = eat(tokens, symbol, ";")
	return tokens
}

func compileExpression(tokens []*token, f *os.File) []*token {
	tokens = compileTerm(tokens, f)
	for true {
		op, ok := opSymbols[tokens[0].val]
		if !ok {
			break
		}
		tokens = eat(tokens, symbol, "")
		tokens = compileTerm(tokens, f)
		fmt.Fprintln(f, op)
	}

	return tokens
}

func compileTerm(tokens []*token, f *os.File) []*token {
	if tokens[0].tType == identifier && tokens[1].val == "[" {
		targetSymbol := findSymbol(tokens[0].val)
		fmt.Fprintf(f, "push %s %d\n", targetSymbol.sType, targetSymbol.index)
		tokens = eat(tokens, "", "")
		tokens = eat(tokens, symbol, "[")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, symbol, "]")
		fmt.Fprintln(f, "add")
		fmt.Fprintln(f, "pop pointer 1")
		fmt.Fprintln(f, "push that 0")
		return tokens
	}
	if tokens[0].tType == identifier && tokens[1].val == "." {
		tokens = compileSubroutineCall(tokens, f)
		return tokens
	}
	if tokens[0].tType == symbol && tokens[0].val == "(" {
		tokens = eat(tokens, symbol, "(")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, symbol, ")")
		return tokens
	}

	if tokens[0].tType == symbol && (tokens[0].val == "-" || tokens[0].val == "~") {
		op := tokens[0].val
		tokens = eat(tokens, symbol, "")
		tokens = compileTerm(tokens, f)
		if op == "-" {
			fmt.Fprintln(f, "neg")
		} else if op == "~" {
			fmt.Fprintln(f, "not")
		} else {
			panic("unary op not match ")
		}
		return tokens
	}
	if tokens[0].tType == stringConstant {
		fmt.Fprintf(f, "push constant %d\n", len(tokens[0].val))
		fmt.Fprintln(f, "call String.new 1")
		for _, val := range tokens[0].val {
			fmt.Fprintf(f, "push constant %d\n", val)
			fmt.Fprintln(f, "call String.appendChar 2")
		}
		tokens = eat(tokens, "", "")
		return tokens
	}
	if tokens[0].tType == integerConstant {
		fmt.Fprintf(f, "push constant %s\n", tokens[0].val)
		tokens = eat(tokens, "", "")
		return tokens
	}
	if tokens[0].tType == keyword {
		if tokens[0].val == "true" {
			fmt.Fprintln(f, "push constant 0")
			fmt.Fprintln(f, "not")
		} else if tokens[0].val == "false" {
			fmt.Fprintln(f, "push constant 0")
		} else if tokens[0].val == "this" {
			fmt.Fprintln(f, "push pointer 0")
		} else if tokens[0].val == "null" {
			fmt.Fprintln(f, "push constant 0")
		} else {
			panic("not match keyword literal" + tokens[0].val)
		}
		tokens = eat(tokens, "", "")
		return tokens
	}
	if tokens[0].tType == identifier {
		symbol := findSymbol(tokens[0].val)
		fmt.Fprintf(f, "push %s %d\n", symbol.sType, symbol.index)
		tokens = eat(tokens, "", "")
		return tokens
	}
	panic("not cover path")
}

var currentClassName string

func compileSubroutineCall(tokens []*token, f *os.File) []*token {
	if tokens[1].val == "." {
		objOrClass := tokens[0].val
		obj := findSymbol(objOrClass)
		if obj != nil {
			fmt.Fprintf(f, "push %s %d\n", obj.sType, obj.index)
		}
		tokens = eat(tokens, identifier, "")
		tokens = eat(tokens, symbol, ".")
		funcName := tokens[0].val
		tokens = eat(tokens, identifier, "")
		tokens = eat(tokens, symbol, "(")
		tokens = compileExpressionList(tokens, f)
		tokens = eat(tokens, symbol, ")")
		if obj != nil {
			fmt.Fprintf(f, "call %s.%s %d\n", obj.fType, funcName, callArgumentNum+1)
		} else {
			fmt.Fprintf(f, "call %s.%s %d\n", objOrClass, funcName, callArgumentNum)
		}
	}

	if tokens[1].val == "(" {
		fmt.Fprintf(f, "push pointer 0\n")
		funcName := tokens[0].val
		tokens = eat(tokens, identifier, "")
		tokens = eat(tokens, symbol, "(")
		tokens = compileExpressionList(tokens, f)
		tokens = eat(tokens, symbol, ")")
		fmt.Fprintf(f, "call %s.%s %d\n", currentClassName, funcName, callArgumentNum+1)
	}

	return tokens
}

var callArgumentNum = 0

func compileExpressionList(tokens []*token, f *os.File) []*token {
	callArgumentNum = 0
	if tokens[0].val != ")" {
		callArgumentNum++
		tokens = compileExpression(tokens, f)
		for tokens[0].val == "," {
			callArgumentNum++
			tokens = eat(tokens, symbol, ",")
			tokens = compileExpression(tokens, f)
		}
	}
	return tokens
}

func main() {
	target := os.Args[1]

	fi, err := os.Stat(target)
	if err != nil {
		panic(err)
	}
	var filePaths []string
	switch mode := fi.Mode(); {
	case mode.IsDir():
		files, err := ioutil.ReadDir(target)
		if err != nil {
			panic(err)
		}
		filePaths = []string{}
		for _, f := range files {
			if strings.HasSuffix(f.Name(), ".jack") {
				filePaths = append(filePaths, target+"/"+f.Name())
			}
		}
	case mode.IsRegular():
		filePaths = []string{target}
	}

	for _, filePath := range filePaths {
		tokens := readLines(filePath)

		parserF, _ := os.OpenFile(filePath[:strings.LastIndex(filePath, ".")]+".vm", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer parserF.Close()
		parser(tokens, parserF)
	}
}
