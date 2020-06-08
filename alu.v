/*MIT License

Copyright (c) 2020 Debtanu Mukherjee

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/


`timescale 10ns/1ns
module alu(alu_op, iA, iB, iF, oR, oF);


parameter DATASIZE = 8;
parameter CARRY_F = 0;
parameter PARITY_F = 2;
parameter AUXC_F = 4;
parameter ZERO_F = 6;
parameter SIGN_F = 7;

input [1:0] alu_op;
input [DATASIZE-1:0] iA, iB, iF;
output [DATASIZE-1:0] oR, oF; 
wire auxa, auxb, auxc;


assign adc = ~alu_op[1] & ~alu_op[0];
assign aci = ~alu_op[1] & ~alu_op[0];
assign sbb = ~alu_op[1] & alu_op[0];
assign ana = alu_op[1] & ~alu_op[0];
assign cmp = alu_op[1] & alu_op[0];

assign {oF[CARRY_F],oR} = (adc) ? iA+iB+iF[CARRY_F] : {DATASIZE+1{1'bz}};
assign {oF[CARRY_F],oR} = (aci) ? iA+iB+iF[CARRY_F] : {DATASIZE+1{1'bz}};
assign {oF[CARRY_F],oR} = (sbb) ? iA-iB-iF[CARRY_F] : {DATASIZE+1{1'bz}};
assign {oF[CARRY_F],oR} = (cmp) ? iA-iB : {DATASIZE+1{1'bz}};
assign {oF[CARRY_F],oR} = (ana) ? {1'b0,iA&iB} : {DATASIZE+1{1'bz}};

assign oF[1] = 1'b0;
assign oF[3] = 1'b0;
assign oF[5] = 1'b0;


assign oF[ZERO_F] = ~|oR;
assign oF[SIGN_F] = oR[SIGN_F];
assign oF[PARITY_F] = ~^oR;
assign oF[AUXC_F] = (adc | aci | sbb | cmp) ? auxc : 1'bz;
assign auxa = (~iA[4] & ~iB[4] & oR[4]) | (~iA[4] & iB[4] & ~oR[4]) | (iA[4] & ~iB[4] & ~oR[4]) | (iA[4] & iB[4] & oR[4]);//adc and aci
assign auxb = (~iA[3] & iB[3] & ~oR[3]) | (~iA[3] & iB[3] & oR[3]) | (iA[3] & iB[3] & oR[3]);//sbb and cmp
assign auxc = (adc | aci) ? auxa : auxb;
assign oF[AUXC_F] = (ana) ? iF[AUXC_F] : 1'bz;

endmodule
