`timescale 10ns/1ns
module incdec2(incdec_op, iA, oR);

parameter DATASIZE = 8;
input incdec_op;
input [DATASIZE-1:0] iA;
output [DATASIZE-1:0] oR;

assign oR = (incdec_op) ? (iA+2'b10) : (iA-2'b10);

endmodule
