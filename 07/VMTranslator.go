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
)

var compareOps = map[string]string{"eq": "D;JEQ", "gt": "D;JLT", "lt": "D;JGT"}
var binaryOps = map[string]string{"add": "D=M+D", "sub": "D=M-D", "and": "D=D&M", "or": "D=D|M"}
var unaryOps = map[string]string{"neg": "D=-M", "not": "D=!M"}
var segmentLoc = map[string][]string{"local": []string{"@LCL", "A=M"}, "argument": []string{"@ARG", "A=M"},
	"this": []string{"@THIS", "A=M"}, "that": []string{"@THAT", "A=M"},
	"static": []string{"@16"}, "temp": []string{"@5"}}

var compare_label_i = 0

type command struct {
	cType commandType
	arg1  string
	arg2  string
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
		return &command{cArithmetic, line, ""}
	}
	if _, ok := compareOps[line]; ok {
		return &command{cArithmetic, line, ""}
	}
	if _, ok := unaryOps[line]; ok {
		return &command{cArithmetic, line, ""}
	}
	if strings.HasPrefix(line, "push") {
		splits := strings.Split(line, " ")
		return &command{cPush, splits[1], splits[2]}
	}
	if strings.HasPrefix(line, "pop") {
		splits := strings.Split(line, " ")
		return &command{cPop, splits[1], splits[2]}
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
	}
	panic("not support command" + fmt.Sprintf("%v", cmd))
}

func main() {
	inFile := os.Args[1]
	lines := readLines(inFile)
	cmds := make([]*command, len(lines))
	for i, line := range lines {
		cmds[i] = parse(line)
	}

	outFile := inFile[:strings.LastIndex(inFile, ".")] + ".asm"
	f, err := os.OpenFile(outFile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	defer f.Close()
	if err != nil {
		panic(err)
	}
	for _, ins := range cmds {
		fmt.Fprintf(f, "//%v\n", ins)
		for _, code := range asmCode(ins) {
			fmt.Fprintf(f, "%s\n", code)
		}
	}
}
