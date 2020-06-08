`timescale 10ns/1ns
module buffer(en , idata, odata);

parameter DATASIZE = 8;

input en;
input [DATASIZE-1:0] idata;
output [DATASIZE-1:0] odata;

assign odata = (en) ? idata : {DATASIZE{1'bz}};

endmodule
