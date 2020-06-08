`timescale 10ns/1ns
module incdec(incdec_op, iA, oR);

parameter DATASIZE = 8;
input incdec_op;
input [DATASIZE-1:0] iA;
output [DATASIZE-1:0] oR;

assign oR = (incdec_op) ? (iA+1'b1) : (iA-1'b1);

endmodule
