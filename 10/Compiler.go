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

type syntax struct {
	sType syntaxType
}

type keywords []string

var allSymbols = map[byte]string{'{': "{", '}': "}", '(': "(", ')': ")", '[': "[", ']': "]", '.': ".", ',': ",", ';': ";", '+': "+", '-': "-",
	'*': "*", '/': "/", '&': "&amp;", '|': "|", '<': "&lt;", '>': "&gt;", '=': "=", '~': "~"}
var opSymbols = map[string]string{"+": "+", "-": "-", "*": "*", "/": "/", "&amp;": "&amp;", "|": "|", "&lt;": "&lt;", "&gt;": "&gt;", "=": "="}
var allKeywords = keywords{"class", "constructor", "function", "method", "field", "static",
	"var", "int", "char", "boolean", "void", "true", "false", "null", "this", "let", "do",
	"if", "else", "while", "return"}

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

func parser(tokens []*token, f *os.File) {
	leftToks := compileClass(tokens, f)
	if len(leftToks) > 0 {
		fmt.Println(leftToks[0].val)
		panic("not consume all token")
	}
}

func eat(tokens []*token, f *os.File, tType tokenType, val string) []*token {
	if (tType == "" || tokens[0].tType == tType) && (val == "" || tokens[0].val == val) {
		fmt.Fprintf(f, "<%s>%s</%s>\n", tokens[0].tType, tokens[0].val, tokens[0].tType)
		return tokens[1:]
	} else {
		fmt.Println(tokens[0])
		panic("not match")
	}
}

func compileClass(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<class>")
	tokens = eat(tokens, f, keyword, "class")
	tokens = eat(tokens, f, identifier, "")
	tokens = eat(tokens, f, symbol, "{")

	for tokens[0].val == "static" || tokens[0].val == "field" {
		tokens = compileClassVarDec(tokens, f)
	}

	for tokens[0].val == "constructor" || tokens[0].val == "function" || tokens[0].val == "method" {
		tokens = compileSubroutineDec(tokens, f)
	}
	tokens = eat(tokens, f, symbol, "}")
	fmt.Fprintln(f, "</class>")

	return tokens
}

func compileClassVarDec(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<classVarDec>")
	tokens = eat(tokens, f, keyword, tokens[0].val)
	tokens = eat(tokens, f, "", "")
	tokens = eat(tokens, f, identifier, "")
	for tokens[0].val == "," {
		tokens = eat(tokens, f, symbol, ",")
		tokens = eat(tokens, f, identifier, "")
	}
	tokens = eat(tokens, f, symbol, ";")
	fmt.Fprintln(f, "</classVarDec>")
	return tokens
}

func compileSubroutineDec(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<subroutineDec>")
	tokens = eat(tokens, f, keyword, tokens[0].val)
	tokens = eat(tokens, f, "", "")
	tokens = eat(tokens, f, identifier, "")
	tokens = eat(tokens, f, symbol, "(")
	tokens = compileParameterList(tokens, f)
	tokens = eat(tokens, f, symbol, ")")
	tokens = compileSubroutineBody(tokens, f)
	fmt.Fprintln(f, "</subroutineDec>")
	return tokens
}

func compileParameterList(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<parameterList>")
	if tokens[0].tType == identifier || tokens[0].tType == keyword {
		tokens = eat(tokens, f, "", "")
		tokens = eat(tokens, f, identifier, "")
		for tokens[0].val == "," {
			tokens = eat(tokens, f, symbol, ",")
			tokens = eat(tokens, f, "", "")
			tokens = eat(tokens, f, identifier, "")
		}
	}
	fmt.Fprintln(f, "</parameterList>")
	return tokens
}

func compileSubroutineBody(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<subroutineBody>")
	tokens = eat(tokens, f, symbol, "{")
	for tokens[0].val == "var" {
		tokens = compileVarDec(tokens, f)
	}
	tokens = compileStatements(tokens, f)
	tokens = eat(tokens, f, symbol, "}")
	fmt.Fprintln(f, "</subroutineBody>")
	return tokens
}

func compileVarDec(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<varDec>")
	tokens = eat(tokens, f, keyword, "var")
	tokens = eat(tokens, f, "", "")
	tokens = eat(tokens, f, identifier, "")
	for tokens[0].val == "," {
		tokens = eat(tokens, f, symbol, ",")
		tokens = eat(tokens, f, identifier, "")
	}
	tokens = eat(tokens, f, symbol, ";")
	fmt.Fprintln(f, "</varDec>")
	return tokens
}

func compileStatements(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<statements>")
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
	fmt.Fprintln(f, "</statements>")
	return tokens
}

func compileLetStatement(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<letStatement>")
	tokens = eat(tokens, f, keyword, "let")
	tokens = eat(tokens, f, identifier, "")
	if tokens[0].val == "[" {
		tokens = eat(tokens, f, symbol, "[")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, f, symbol, "]")
	}
	tokens = eat(tokens, f, symbol, "=")
	tokens = compileExpression(tokens, f)
	tokens = eat(tokens, f, symbol, ";")
	fmt.Fprintln(f, "</letStatement>")
	return tokens
}

func compileIfStatement(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<ifStatement>")
	tokens = eat(tokens, f, keyword, "if")
	tokens = eat(tokens, f, symbol, "(")
	tokens = compileExpression(tokens, f)
	tokens = eat(tokens, f, symbol, ")")
	tokens = eat(tokens, f, symbol, "{")
	tokens = compileStatements(tokens, f)
	tokens = eat(tokens, f, symbol, "}")
	if tokens[0].val == "else" {
		tokens = eat(tokens, f, keyword, "else")
		tokens = eat(tokens, f, symbol, "{")
		tokens = compileStatements(tokens, f)
		tokens = eat(tokens, f, symbol, "}")
	}
	fmt.Fprintln(f, "</ifStatement>")
	return tokens
}

func compileWhileStatement(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<whileStatement>")
	tokens = eat(tokens, f, keyword, "while")
	tokens = eat(tokens, f, symbol, "(")
	tokens = compileExpression(tokens, f)
	tokens = eat(tokens, f, symbol, ")")
	tokens = eat(tokens, f, symbol, "{")
	tokens = compileStatements(tokens, f)
	tokens = eat(tokens, f, symbol, "}")
	fmt.Fprintln(f, "</whileStatement>")
	return tokens
}

func compileDoStatement(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<doStatement>")
	tokens = eat(tokens, f, keyword, "do")
	tokens = compileSubroutineCall(tokens, f)
	tokens = eat(tokens, f, symbol, ";")
	fmt.Fprintln(f, "</doStatement>")
	return tokens
}

func compileReturnStatement(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<returnStatement>")
	tokens = eat(tokens, f, keyword, "return")
	if tokens[0].val != ";" {
		tokens = compileExpression(tokens, f)
	}
	tokens = eat(tokens, f, symbol, ";")
	fmt.Fprintln(f, "</returnStatement>")
	return tokens
}

func compileExpression(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<expression>")
	tokens = compileTerm(tokens, f)
	for true {
		_, ok := opSymbols[tokens[0].val]
		if !ok {
			break
		}
		tokens = eat(tokens, f, symbol, "")
		tokens = compileTerm(tokens, f)
	}

	fmt.Fprintln(f, "</expression>")
	return tokens
}

func compileTerm(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<term>")
	if tokens[0].tType == identifier && tokens[1].val == "[" {
		tokens = eat(tokens, f, "", "")
		tokens = eat(tokens, f, symbol, "[")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, f, symbol, "]")
		fmt.Fprintln(f, "</term>")
		return tokens
	}
	if tokens[0].tType == identifier && tokens[1].val == "." {
		tokens = compileSubroutineCall(tokens, f)
		fmt.Fprintln(f, "</term>")
		return tokens
	}
	if tokens[0].tType == symbol && tokens[0].val == "(" {
		tokens = eat(tokens, f, symbol, "(")
		tokens = compileExpression(tokens, f)
		tokens = eat(tokens, f, symbol, ")")
		fmt.Fprintln(f, "</term>")
		return tokens
	}

	if tokens[0].tType == symbol && (tokens[0].val == "-" || tokens[0].val == "~") {
		tokens = eat(tokens, f, symbol, "")
		tokens = compileTerm(tokens, f)
		fmt.Fprintln(f, "</term>")
		return tokens
	}
	if tokens[0].tType == stringConstant || tokens[0].tType == integerConstant || tokens[0].tType == keyword || tokens[0].tType == identifier {
		tokens = eat(tokens, f, "", "")
		fmt.Fprintln(f, "</term>")
		return tokens
	}
	panic("not cover path")
}

func compileSubroutineCall(tokens []*token, f *os.File) []*token {
	if tokens[1].val == "." {
		tokens = eat(tokens, f, identifier, "")
		tokens = eat(tokens, f, symbol, ".")
		tokens = eat(tokens, f, identifier, "")
		tokens = eat(tokens, f, symbol, "(")
		tokens = compileExpressionList(tokens, f)
		tokens = eat(tokens, f, symbol, ")")
	}

	if tokens[1].val == "(" {
		tokens = eat(tokens, f, identifier, "")
		tokens = eat(tokens, f, symbol, "(")
		tokens = compileExpressionList(tokens, f)
		tokens = eat(tokens, f, symbol, ")")
	}

	return tokens
}

func compileExpressionList(tokens []*token, f *os.File) []*token {
	fmt.Fprintln(f, "<expressionList>")
	if tokens[0].val != ")" {
		tokens = compileExpression(tokens, f)
		for tokens[0].val == "," {
			tokens = eat(tokens, f, symbol, ",")
			tokens = compileExpression(tokens, f)
		}
	}
	fmt.Fprintln(f, "</expressionList>")
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
		tokenF, _ := os.OpenFile(filePath[:strings.LastIndex(filePath, ".")]+"T.my.xml", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer tokenF.Close()
		tokens := readLines(filePath)
		fmt.Fprintln(tokenF, "<tokens>")
		for _, token := range tokens {
			fmt.Fprintf(tokenF, "<%s>%s</%s>\n", token.tType, token.val, token.tType)
		}
		fmt.Fprintln(tokenF, "</tokens>")

		parserF, _ := os.OpenFile(filePath[:strings.LastIndex(filePath, ".")]+".my.xml", os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer parserF.Close()
		parser(tokens, parserF)
	}
}
