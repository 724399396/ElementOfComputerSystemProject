package main

import (
	"bytes"
	"fmt"
	"io/ioutil"
	"os"
	"strconv"
	"strings"
)

type instructionType int

const (
	aInstruction instructionType = iota
	cInstruction
)

var destMap = map[string]string{"": "000", "M": "001", "D": "010", "MD": "011", "A": "100", "AM": "101",
	"AD": "110", "AMD": "111"}

var compMap = map[string]string{"0": "0101010", "1": "0111111", "-1": "0111010", "D": "0001100", "A": "0110000",
	"M": "1110000", "!D": "0001101", "!A": "0110001", "!M": "1110001", "-D": "0001111", "-A": "0110011",
	"-M": "1110011", "D+1": "0011111", "A+1": "0110111", "M+1": "1110111", "D-1": "0001110", "A-1": "0110010",
	"M-1": "1110010", "D+A": "0000010", "D+M": "1000010", "D-A": "0010011", "D-M": "1010011", "A-D": "0000111",
	"M-D": "1000111", "D&A": "0000000", "D&M": "1000000", "D|A": "0010101", "D|M": "1010101"}

var jumpMap = map[string]string{"": "000", "JGT": "001", "JEQ": "010", "JGE": "011", "JLT": "100", "JNE": "101",
	"JLE": "110", "JMP": "111"}

var symbols = map[string]int{"SP": 0, "LCL": 1, "ARG": 2, "THIS": 3, "THAT": 4, "SCREEN": 16384, "KBD": 24576,
	"R0": 0, "R1": 1, "R2": 2, "R3": 3, "R4": 4, "R5": 5, "R6": 6, "R7": 7, "R8": 8, "R9": 9, "R10": 10, "R11": 11, "R12": 12,
	"R13": 13, "R14": 14, "R15": 15}

var varaibleLoc = 16

type instruction struct {
	iType  instructionType
	number int
	comp   string
	dest   string
	jump   string
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

func parse(line string) instruction {
	ins := instruction{}
	if strings.HasPrefix(line, "@") {
		ins.iType = aInstruction
		variable := line[1:]
		i, err := strconv.Atoi(variable)
		if err != nil {
			loc, ok := symbols[variable]
			if ok {
				ins.number = loc
			} else {
				symbols[variable] = varaibleLoc
				ins.number = varaibleLoc
				varaibleLoc++
			}
		} else {
			ins.number = i
		}
	} else {
		ins.iType = cInstruction
		equalIndex := strings.Index(line, "=")
		semiCommaIndex := strings.Index(line, ";")
		if equalIndex > 0 {
			ins.dest = line[0:equalIndex]
			if semiCommaIndex > 0 {
				ins.jump = line[semiCommaIndex+1:]
				ins.comp = line[equalIndex+1 : semiCommaIndex]
			} else {
				ins.jump = ""
				ins.comp = line[equalIndex+1:]
			}
		} else {
			ins.dest = ""
			if semiCommaIndex > 0 {
				ins.jump = line[semiCommaIndex+1:]
				ins.comp = line[:semiCommaIndex]
			} else {
				ins.jump = ""
				ins.comp = line
			}
		}
	}
	return ins
}

func decimal2binary(i int) string {
	if i == 0 {
		return ""
	}
	return decimal2binary(i/2) + strconv.Itoa(i%2)
}

func convert(ins instruction) string {
	if ins.iType == aInstruction {
		bin := decimal2binary(ins.number)
		if len(bin) < 15 {
			var prefix bytes.Buffer
			for i := 0; i < 15-len(bin); i++ {
				prefix.WriteString("0")
			}
			bin = prefix.String() + bin
		}
		return "0" + bin
	}
	var buf bytes.Buffer
	buf.WriteString("111")
	buf.WriteString(compMap[ins.comp])
	buf.WriteString(destMap[ins.dest])
	buf.WriteString(jumpMap[ins.jump])
	return buf.String()
}

func processLabel(lines []string) []string {
	i := 0
	res := make([]string, len(lines))
	for _, line := range lines {
		if (line[0] == '(') && (line[len(line)-1] == ')') {
			symbols[line[1:len(line)-1]] = i
		} else {
			res[i] = line
			i++
		}
	}
	return res[:i]
}

func main() {
	inFile := os.Args[1]
	lines := readLines(inFile)
	linesWithoutLabel := processLabel(lines)
	instructions := make([]instruction, len(linesWithoutLabel))
	for i, line := range linesWithoutLabel {
		instructions[i] = parse(line)
	}

	outFile := strings.Split(inFile, ".")[0] + ".hack"
	f, err := os.OpenFile(outFile, os.O_WRONLY|os.O_CREATE|os.O_TRUNC, 0644)
	defer f.Close()
	if err != nil {
		panic(err)
	}
	for _, ins := range instructions {
		fmt.Fprintf(f, "%s\n", convert(ins))
	}
}
