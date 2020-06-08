# my8085

This is the Verilog code for 8085 microprocessor with limited (18) number of instructions

# Instructions
                                1 BYTE                         2 BYTE                        3 BYTE
MOV R1, R2                     01DDDSSS
MOV R, M                       01DDD110
MOV M, R                       01110SSS
MVI R, D08                     00110110                       DDDDDDDD
LXI Rp, D16                    00DD0001                       LLLLLLLL                       HHHHHHHH
LDA ADDR                       00111010                       LLLLLLLL                       HHHHHHHH
STA ADDR                       00110010                       LLLLLLLL                       HHHHHHHH
ADC R                          10001SSS
ACI D08                        11001110                       DDDDDDDD
SBB R                          10011SSS
ANA R                          10100SSS                       
CMP R                          10111SSS
JMP ADDR                       11000011                       LLLLLLLL                       HHHHHHHH
JC ADDR                        11011010                       LLLLLLLL                       HHHHHHHH
CALL ADDR                      11001101                       LLLLLLLL                       HHHHHHHH
CZ ADDR                        11001100                       LLLLLLLL                       HHHHHHHH
RET                            11001001
RZ                             11001000


# Description

  LLLLLLLL         LOWER ORDER ADDR
  HHHHHHHH         HIGHER ORDER ADDR
  SSS              SOURCE REGISTER
  DDD              DESTINATION REGISTER
  DD               DESTINATION REGISTER PAIR

# Valid values

  LLLLLLLL ------- 00000000 TO 11111111
  HHHHHHHH ------- 00000000 TO 11111111
  SSS ------------ 000 TO 101 AND 111 # NO 110
  DDD ------------ 000 TO 101 AND 111 # NO 110
  DD ------------- 00 TO 10 # NO 11


# Values' meaning

  # SSS/DDD
    000 - REG B
    001 - REG C
    010 - REG D
    011 - REG E
    100 - REG H
    101 - REG L
    111 - REG A
    
  # DD
    00 - REG BC
    01 - REG DE
    10 - REG HL
    
    
# Instruction dscription

OF - OPCODE FETCH
MR - MEM READ
MW - MEM WRITE

MOV R1, R2 ---> 1 OF -----------------
MOV R, M -----> 1 OF + 1 MR ----------
MOV M, R -----> 1 OF + 1 MW ----------
MVI D08 ------> 1 OF + 1 MR ----------
LXI Rp, D16 --> 1 OF + 2 MR ----------
LDA ADDR -----> 1 OF + 3 MR ----------
STA ADDR -----> 1 OF + 2 MR + 1 MW ---
ADC R --------> 1 OF -----------------
ACI D08 ------> 1 OF + 1 MR ----------
SBB R --------> 1 OF -----------------
ANA R --------> 1 OF -----------------                       
CMP R --------> 1 OF -----------------
JMP ADDR -----> 1 OF + 2 MR ----------
JC ADDR ------> 1 OF + 2 MR ----------
CALL ADDR ----> 1 OF + 2 MR + 2 MW ---
CZ ADDR ------> 1 OF + 2 MR + 2 MW ---
RET ----------> 1 OF + 2 MR
RZ -----------> 1 OF + 2 MR
