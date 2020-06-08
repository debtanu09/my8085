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
