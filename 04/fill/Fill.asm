// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input.
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel;
// the screen should remain fully black as long as the key is pressed. 
// When no key is pressed, the program clears the screen, i.e. writes
// "white" in every pixel;
// the screen should remain fully clear as long as no key is pressed.

// Put your code here.
(LOOP)
@KBD
D=A
@j
M=D
@SCREEN
D=A
@i
M=D

@KBD
D=M
@WHITE
D;JEQ

(BLACK)

@i
A=M
M=-1
@i
MD=M+1
@j
D=M-D
@BLACK
D;JGT

@LOOP
0;JMP

(WHITE)

@i
A=M
M=0
@i
MD=M+1
@j
D=M-D
@WHITE
D;JGT

@LOOP
0;JMP
