`timescale 10ns/1ns
module register(clk, rst, en, idata, odata);

parameter DATASIZE = 8;

input clk, rst, en;
input [DATASIZE-1:0] idata;
output [DATASIZE-1:0] odata;

reg [DATASIZE-1:0] odata;


always @(posedge clk or posedge rst) begin
	if(rst)
		odata <= {DATASIZE{1'b0}};
	else if(en)
		odata <= idata;
end


endmodule
