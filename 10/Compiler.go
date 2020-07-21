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

type token struct {
	tType tokenType
	val   string
}

type keywords []string

var allSymbols = map[byte]string{'{':"{", '}':"}", '(':"(", ')':")", '[':"[", ']':"]", '.':".", ',':",", ';':";", '+':"+", '-':"-",
	'*':"*", '/':"/", '&':"&amp;", '|':"|", '<':"&lt;", '>':"&gt;", '=':"=", '~':"~"}
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
		if s,ok := allSymbols[line[i]]; ok {
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
		outFile := filePath[:strings.LastIndex(filePath, ".")] + "T.my.xml"
		tokenF, _ := os.OpenFile(outFile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer tokenF.Close()
		tokens := readLines(filePath)
		fmt.Fprintln(tokenF, "<tokens>")
		for _, token := range tokens {
			fmt.Fprintf(tokenF, "<%s>%s</%s>\n", token.tType, token.val, token.tType)
		}
		fmt.Fprintln(tokenF, "</tokens>")
	}
}
