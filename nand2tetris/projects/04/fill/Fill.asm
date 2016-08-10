// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Fill.asm

// Runs an infinite loop that listens to the keyboard input. 
// When a key is pressed (any key), the program blackens the screen,
// i.e. writes "black" in every pixel. When no key is pressed, the
// program clears the screen, i.e. writes "white" in every pixel.

// Put your code here.
(INIT)
    @SCREEN
    D =  A
    @CUR_POINTER
    M = D
    @8191
    D = A
    @SCREEN
    D = D + A
    @MAX_POINTER
    M = D
    @START
    0                           ;JMP
    
(START)
    @KBD
    D = M
    @WHITE
    D                           ;JEQ
    @BLACK
    0                           ;JMP
(WHITE)
    @CUR_POINTER
    A = M
    M = 0
    @INC
    0                           ;JMP
(BLACK)
    @CUR_POINTER
    A = M
    M = -1
    @INC
    0                           ;JMP
(INC)
    @CUR_POINTER
    D = M + 1
    M = D
    @MAX_POINTER
    D = M - D
    @INIT
    D                           ;JLT
    @START
    0                           ;JMP
