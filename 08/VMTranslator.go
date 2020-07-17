package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type commandType int

const (
	cArithmetic commandType = iota
	cPush
	cPop
	cLabel
	cGoTo
	cIfGoTo
	cFunc
	cCall
	cReturn
)

var compareOps = map[string]string{"eq": "D;JEQ", "gt": "D;JLT", "lt": "D;JGT"}
var binaryOps = map[string]string{"add": "D=M+D", "sub": "D=M-D", "and": "D=D&M", "or": "D=D|M"}
var unaryOps = map[string]string{"neg": "D=-M", "not": "D=!M"}
var segmentLoc = map[string][]string{"local": []string{"@LCL", "A=M"}, "argument": []string{"@ARG", "A=M"},
	"this": []string{"@THIS", "A=M"}, "that": []string{"@THAT", "A=M"},
	"static": []string{"@16"}, "temp": []string{"@5"}}

var funcCallCount = map[string]int{}

var compare_label_i = 0

type command struct {
	cType commandType
	arg1  string
	arg2  string
	src   string
}

func readLines(inFile string) []string {
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
	return lines[:i]
}

func parse(line string) *command {
	if _, ok := binaryOps[line]; ok {
		return &command{cArithmetic, line, "", line}
	}
	if _, ok := compareOps[line]; ok {
		return &command{cArithmetic, line, "", line}
	}
	if _, ok := unaryOps[line]; ok {
		return &command{cArithmetic, line, "", line}
	}
	if strings.HasPrefix(line, "push") {
		splits := strings.Split(line, " ")
		return &command{cPush, splits[1], splits[2], line}
	}
	if strings.HasPrefix(line, "pop") {
		splits := strings.Split(line, " ")
		return &command{cPop, splits[1], splits[2], line}
	}
	if strings.HasPrefix(line, "label") {
		splits := strings.Split(line, " ")
		return &command{cLabel, splits[1], "", line}
	}
	if strings.HasPrefix(line, "goto") {
		splits := strings.Split(line, " ")
		return &command{cGoTo, splits[1], "", line}
	}
	if strings.HasPrefix(line, "if-goto") {
		splits := strings.Split(line, " ")
		return &command{cIfGoTo, splits[1], "", line}
	}
	if strings.HasPrefix(line, "function") {
		splits := strings.Split(line, " ")
		return &command{cFunc, splits[1], splits[2], line}
	}
	if strings.HasPrefix(line, "call") {
		splits := strings.Split(line, " ")
		return &command{cCall, splits[1], splits[2], line}
	}
	if strings.HasPrefix(line, "return") {
		return &command{cReturn, "", "", line}
	}
	panic("not support command" + line)
}

func asmCode(cmd *command) []string {
	switch cmd.cType {
	case cArithmetic:
		if op, ok := binaryOps[cmd.arg1]; ok {
			return []string{
				"@SP",
				"M=M-1",
				"A=M",
				"D=M",
				"@SP",
				"A=M-1",
				op,
				"@SP",
				"A=M-1",
				"M=D",
			}
		}
		if op, ok := compareOps[cmd.arg1]; ok {
			compare_label_i++
			return []string{
				"@SP",
				"M=M-1",
				"A=M",
				"D=M",
				"@SP",
				"A=M-1",
				"D=D-M",
				"@compare_match_" + strconv.Itoa(compare_label_i),
				op,
				"D=0",
				"@compare_not_match_" + strconv.Itoa(compare_label_i),
				"0;JMP",
				"(compare_match_" + strconv.Itoa(compare_label_i) + ")",
				"D=-1",
				"(compare_not_match_" + strconv.Itoa(compare_label_i) + ")",
				"@SP",
				"A=M-1",
				"M=D",
			}
		}
		if op, ok := unaryOps[cmd.arg1]; ok {
			return []string{
				"@SP",
				"A=M-1",
				"D=M",
				op,
				"M=D",
			}
		}
	case cPush:
		var codes []string
		if cmd.arg1 == "constant" {
			codes = []string{
				"@" + cmd.arg2,
				"D=A",
			}
		} else if cmd.arg1 == "pointer" {
			if cmd.arg2 == "0" {
				codes = []string{
					"@THIS",
					"D=M",
				}
			}
			if cmd.arg2 == "1" {
				codes = []string{
					"@THAT",
					"D=M",
				}
			}
		} else {
			loc := segmentLoc[cmd.arg1]
			n, _ := strconv.Atoi(cmd.arg2)
			for i := 0; i < n; i++ {
				loc = append(loc, "A=A+1")
			}
			codes = append(loc, "D=M")
		}
		return append(codes, []string{
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"M=M+1",
		}...)
	case cPop:
		var codes []string
		if cmd.arg1 == "pointer" {
			if cmd.arg2 == "0" {
				codes = []string{
					"@THIS",
					"M=D",
				}
			}
			if cmd.arg2 == "1" {
				codes = []string{
					"@THAT",
					"M=D",
				}
			}
		} else {
			loc := segmentLoc[cmd.arg1]
			n, _ := strconv.Atoi(cmd.arg2)
			for i := 0; i < n; i++ {
				loc = append(loc, "A=A+1")
			}
			codes = append(loc, "M=D")
		}
		return append([]string{
			"@SP",
			"AM=M-1",
			"D=M",
		}, codes...)
	case cLabel:
		return []string{
			"(" + cmd.arg1 + ")",
		}
	case cGoTo:
		return []string{
			"@" + cmd.arg1,
			"0;JMP",
		}
	case cIfGoTo:
		return []string{
			"@SP",
			"AM=M-1",
			"D=M",
			"@" + cmd.arg1,
			"D;JNE",
		}
	case cFunc:
		res := []string{
			"(" + cmd.arg1 + ")",
			"@SP",
			"D=M",
			"@LCL",
			"M=D",
		}
		n, _ := strconv.Atoi(cmd.arg2)
		for i := 0; i < n; i++ {
			res = append(res, []string{
				"@SP",
				"A=M",
				"M=0",
				"@SP",
				"M=M+1",
			}...)
		}
		return res
	case cReturn:
		return []string{
			// save return address to @R5
			"@LCL",
			"D=M",
			"@5",
			"A=D-A",
			"D=M",
			"@R5",
			"M=D",

			// save return value to current arg point to
			"@SP",
			"A=M-1",
			"D=M",
			"@ARG",
			"A=M",
			"M=D",

			// set sp to before call value
			"@ARG",
			"D=M+1",
			"@SP",
			"M=D",

			"@LCL",
			"D=M",
			"@1",
			"A=D-A",
			"D=M",
			"@THAT",
			"M=D",

			"@LCL",
			"D=M",
			"@2",
			"A=D-A",
			"D=M",
			"@THIS",
			"M=D",

			"@LCL",
			"D=M",
			"@3",
			"A=D-A",
			"D=M",
			"@ARG",
			"M=D",

			"@LCL",
			"D=M",
			"@4",
			"A=D-A",
			"D=M",
			"@LCL",
			"M=D",

			"@5",
			"A=M",
			"0;JMP",
		}
	case cCall:
		v, ok := funcCallCount[cmd.arg1]
		if !ok {
			v = 1
		}
		funcCallCount[cmd.arg1] = v + 1
		return []string{
			// get old ARG
			"@R2",
			"D=M",
			"@5",
			"M=D",

			// set ARG
			"@" + cmd.arg2,
			"D=A",
			"@SP",
			"D=M-D",
			"@ARG",
			"M=D",

			"@" + cmd.arg1 + ".ret." + strconv.Itoa(v),
			"D=A",
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"M=M+1",

			"@LCL",
			"D=M",
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"M=M+1",
			"@5",
			"D=M",
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"M=M+1",
			"@THIS",
			"D=M",
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"M=M+1",
			"@THAT",
			"D=M",
			"@SP",
			"A=M",
			"M=D",
			"@SP",
			"MD=M+1",
			// set LCL
			"@LCL",
			"M=D",

			"@" + cmd.arg1,
			"0;JMP",
			"(" + cmd.arg1 + ".ret." + strconv.Itoa(v) + ")",
		}
	}
	panic("not support command" + fmt.Sprintf("%v", cmd))
}

func main() {
	target := os.Args[1]

	fi, err := os.Stat(target)
	if err != nil {
		panic(err)
	}
	var f *os.File
	var lines []string
	switch mode := fi.Mode(); {
	case mode.IsDir():
		outFile := target + "/" + target[strings.LastIndex(target, "/"):] + ".asm"
		f, _ = os.OpenFile(outFile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer f.Close()
		fmt.Fprintln(f, "@256")
		fmt.Fprintln(f, "D=A")
		fmt.Fprintln(f, "@R0")
		fmt.Fprintln(f, "M=D")
		fmt.Fprintln(f, "@Sys.init")
		fmt.Fprintln(f, "0;JMP")
		files, err := ioutil.ReadDir(target)
		if err != nil {
			panic(err)
		}
		lines = []string{}
		for _, f := range files {
			if strings.HasSuffix(f.Name(), ".vm") {
				lines = append(lines, readLines(target+"/"+f.Name())...)
			}
		}
	case mode.IsRegular():
		outFile := target[:strings.LastIndex(target, ".")] + ".asm"
		f, _ = os.OpenFile(outFile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
		defer f.Close()
		lines = readLines(target)
	}

	cmds := make([]*command, len(lines))
	for i, line := range lines {
		cmds[i] = parse(line)
	}

	if err != nil {
		panic(err)
	}
	for _, ins := range cmds {
		fmt.Fprintf(f, "//%s\n", ins.src)
		for _, code := range asmCode(ins) {
			fmt.Fprintf(f, "%s\n", code)
		}
	}
}
