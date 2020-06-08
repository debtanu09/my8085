`timescale 10ns/1ns
`include "alu.v"
`include "incdec.v"
`include "incdec2.v"
`include "register.v"
`include "buffer.v"

module my8085(CLK, ADDRDATA, ADDR, INTA_, INTR, RST55, RST65, RST75, TRAP, SID, SOD, RST_OUT, HOLD, HLDA, CLK_OUT, RST_, READY, IOM_, S1, RD_, WR_, S0, ALE);

parameter state_of = 5'b00001;
parameter state_mr = 5'b00010;
parameter state_mw = 5'b00100;
parameter state_sr = 5'b01000;
parameter state_sw = 5'b10000;
parameter state_rs = 5'b00000;

parameter flagcy = 0;
parameter flagp = 2;
parameter flagac = 4;
parameter flagz = 6;
parameter flags = 7;

// port definitions (i/o)
input CLK, RST_, READY, HOLD, SID, INTR, TRAP, RST75, RST65, RST55;
inout [7:0] ADDRDATA;
output [15:8] ADDR;
output CLK_OUT, RST_OUT, IOM_, S1, S0, INTA_, WR_, RD_, ALE, HLDA, SOD;

// output ports as wires
wire[7:0] ADDRDATA;
wire[15:8] ADDR;
wire CLK_OUT, RST_OUT, IOM_, S1, S0, INTA_, WR_, RD_, ALE, HLDA, SOD;

// alias for input signals
wire clk, rst;
assign clk = CLK;
assign rst = ~RST_;


//------------------------------------------------------------------
//---------------buffers for data and address transfer--------------
//------------------------------------------------------------------
wire chk_id, chk_od, chk_al, chk_ah, chk_tx;
wire [15:0] addrout;
wire [7:0] dataout;
wire [7:0] datain;

buffer data_out(chk_od, dataout, ADDRDATA);
buffer data_in(chk_id, ADDRDATA, datain);
buffer addr_h(chk_ah, addrout[15:8], ADDR);
buffer addr_l(chk_al, addrout[7:0], ADDRDATA);
buffer reg_tx(chk_tx, dataout, datain);
//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------



//------------------------------------------------------------------
//---------------------Registers------------------------------------
//-------------------------GP---------------------------------------

wire [7:0] rgw, rgr, rgres;
wire [7:0] regin [7:0], regdata [7:0], regout [7:0];
wire [7:0] win, wdata, wout, zin, zdata, zout;
wire [15:0] hlout, wzout;
wire hlr, hlw, ww, wr, zw, zr, wzr, wzw;
wire wres, zres;

register b_reg (clk, rgres[0], rgw[0], regin[0], regdata[0]); //reg b
buffer b_buf (rgr[0], regdata[0], regout[0]); //buff b

register c_reg (clk, rgres[1], rgw[1], regin[1], regdata[1]); //reg c
buffer c_buf (rgr[1], regdata[1], regout[1]); //buff c

register d_reg (clk, rgres[2], rgw[2], regin[2], regdata[2]); //reg d
buffer d_buf (rgr[2], regdata[2], regout[2]); //buff d 

register e_reg (clk, rgres[3], rgw[3], regin[3], regdata[3]); //reg e
buffer e_buf (rgr[3], regdata[3], regout[3]); // buff e

register h_reg (clk, rgres[4], rgw[4], regin[4], regdata[4]); //reg h
buffer h_buf (rgr[4], regdata[4], regout[4]); //buff h

register l_reg (clk, rgres[5], rgw[5], regin[5], regdata[5]); //reg l
buffer l_buf (rgr[5], regdata[5], regout[5]); // buff l

register a_reg (clk, rgres[7], rgw[7], regin[7], regdata[7]); //reg a
buffer a_buf (rgr[7], regdata[7], regout[7]); //buff a

//-------------------temp WZ registers------------------------------

register w_reg (clk, wres, ww, win, wdata);
buffer w_buf (wr, wdata, wout);

register z_reg (clk, zres, zw, zin, zdata);
buffer z_buf (zr, zdata, zout);

//--------------buffers for HL and WZ for address-------------------

buffer #(16) hl_reg(hlr, {regdata[4],regdata[5]}, hlout);
buffer #(16) wz_reg(wzr, {wdata,zdata}, wzout);

//-----------------------------PC-----------------------------------

wire pcw, pcr, pcres;
wire [15:0] pcin, pcdata, pcout;

register #(16) pc_reg(clk, pcres, pcw, pcin, pcdata);
buffer #(16) pc_buf (pcr, pcdata, pcout);

//-----------------------------SP-----------------------------------

wire spw_l, spr_l, spres_l;
wire spw_h, spr_h, spres_h;
wire [7:0] spin_l, spdata_l, spout_l;
wire [7:0] spin_h, spdata_h, spout_h;

register sp_reg_l (clk, spres_l, spw_l, spin_l, spdata_l);
buffer sp_buf_l (spr_l, spdata_l, spout_l);
register sp_reg_h (clk, spres_h, spw_h, spin_h, spdata_h);
buffer sp_buf_h (spr_h, spdata_h, spout_h);

//-----------------------------IR-----------------------------------

wire iregen, irres;
wire [7:0] iregin, iregdata; 

register ireg(clk, irres, iregen, iregin, iregdata);

//----------------------------FLAG----------------------------------


wire flagw, flagres;
wire [7:0] flagin, flagout;

register f_reg (clk, flagres, flagw, flagin, flagout);

//--------------------TEMP REGISTER FOR ALU-------------------------

wire tempres, tempw;
wire [7:0] tempin, tempout;

register temp_reg (clk, tempres, tempw, tempin, tempout);

//-----------------------TEMP POINTER-------------------------------

wire pointres_l, pointres_h, pointw_l, pointw_h;
wire [7:0] pointin_l, pointin_h, pointout_l, pointout_h;

register pointer_l(clk, pointres_l, pointw_l, pointin_l, pointout_l);
register pointer_h(clk, pointres_h, pointw_h, pointin_h, pointout_h);

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------------ALU---------------------------------
//------------------------------------------------------------------

wire [1:0] alu_op;
wire [7:0] opr_1, opr_2, res, flagnew;

alu alu_uut(alu_op, opr_1, opr_2, flagout, res, flagnew);

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------------INCDEC------------------------------
//------------------------------------------------------------------

wire incdec_op;
wire [15:0] incdec_in, incdec_out;

incdec #(16) incdec_uut(incdec_op, incdec_in, incdec_out);

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------------INCDEC2-----------------------------
//------------------------------------------------------------------

wire incdec_op2;
wire [15:0] incdec_in2, incdec_out2;

incdec2 #(16) incdec_uut2(incdec_op2, incdec_in2, incdec_out2);

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------



//------------------------------------------------------------------
//---------------------------DECODING-------------------------------
//------------------------------------------------------------------

wire i_txa, i_mov, i_alu, i_sic;

assign i_txa = ~iregdata[7] & ~iregdata[6]; // 00 - transfer + arithmetic
assign i_mov = ~iregdata[7] & iregdata[6]; // 01 - register move + halt
assign i_alu = iregdata[7] & ~iregdata[6]; // 10 - basic alu (ad,as,&,|,^,cmp)
assign i_sic = iregdata[7] & iregdata[6]; // 11 - stack, i/o & control

// decode lower 3-bits
wire lo000, lo001, lo010, lo011, lo101, lo110;

assign lo000 = ~iregdata[2] & ~iregdata[1] & ~iregdata[0];
assign lo001 = ~iregdata[2] & ~iregdata[1] & iregdata[0];
assign lo010 = ~iregdata[2] & iregdata[1] & ~iregdata[0];
assign lo011 = ~iregdata[2] & iregdata[1] & iregdata[0];
assign lo100 = iregdata[2] & ~iregdata[1] & ~iregdata[0];
assign lo101 = iregdata[2] & ~iregdata[1] & iregdata[0];
assign lo110 = iregdata[2] & iregdata[1] & ~iregdata[0];

// decode middle 3-bits
wire hi000, hi001, hi011, hi100, hi110, hi111;

assign hi000 = ~iregdata[5] & ~iregdata[4] & ~iregdata[3];
assign hi001 = ~iregdata[5] & ~iregdata[4] & iregdata[3];
assign hi011 = ~iregdata[5] & iregdata[4] & iregdata[3];
assign hi100 = iregdata[5] & ~iregdata[4] & ~iregdata[3];
assign hi110 = iregdata[5] & iregdata[4] & ~iregdata[3];
assign hi111 = iregdata[5] & iregdata[4] & iregdata[3];


//---------------------INSTRUCTION SPECIFIC-------------------------

wire i_mvi_r, i_lxi, i_lda, i_sta; //00
wire i_mov_rr, i_mov_rm, i_mov_mr; //01
wire i_adc, i_sbb, i_ana, i_cmp; //10
wire i_aci, i_jmp, i_jc, i_call, i_cz, i_ret, i_rz; //11

assign i_mvi_r = i_txa & lo110;
assign i_lxi = i_txa & ~iregdata[3] & lo001;
assign i_lda = i_txa & hi111 & lo010;
assign i_sta = i_txa & hi110 & lo010;

assign i_mov_rr = i_mov & ~hi110 & ~lo110;
assign i_mov_rm = i_mov & ~hi110 & lo110;
assign i_mov_mr = i_mov & hi110 & ~lo110;

assign i_adc = i_alu & hi001;
assign i_sbb = i_alu & hi011;
assign i_ana = i_alu & hi100;
assign i_cmp = i_alu & hi111;

assign i_aci = i_sic & hi001 & lo110;
assign i_jmp = i_sic & hi000 & lo011;
assign i_jc = i_sic & hi011 & lo010;
assign i_call = i_sic & hi001 & lo101;
assign i_cz = i_sic & hi001 & lo100;
assign i_ret = i_sic & hi001 & lo001;
assign i_rz = i_sic & hi001 & lo000;


//---------------NO OF CYCLE DECIDING-------------------------------

wire cyc1, cyc2, cyc3, cyc4, cyc5;
wire cycgo1, cycgo2, cycgo3, cycgo4, cycgo5;
wire cycr2, cycr3, cycr4;
wire cycrd2, cycrd3, cycrd4;
wire cycw2, cycw3, cycw4;
wire cycwr2, cycwr3, cycwr4, cycwr5;
wire cycsr2, cycsr3;
wire cycsw4, cycsw5;

//---------------NO OF TOTAl CYCLE DECIDING-------------------------
assign cyc1 = (i_mvi_r | i_lxi | i_lda | i_sta) | 
      	      (i_mov_rr | i_mov_rm | i_mov_mr) |
      	      (i_adc | i_sbb | i_ana | i_cmp) |
      	      (i_aci | i_jmp | i_jc | i_call | i_cz | i_ret | i_rz);
      	    
assign cycgo1 = (cyc1 === 1'b1) ? 1'b1 : 1'b0;

assign cyc2 = ((i_mvi_r | i_lxi | i_lda | i_sta) |
	      (i_mov_rm | i_mov_mr) |
	      (i_aci | i_jmp | i_call | i_ret | i_jc | i_cz)) ? 1'b1 : 1'bz;
	      	      
assign cyc2 = (flagout[flagz] & i_rz) ? 1'bz : 1'bz;

assign cycgo2 = (cyc2 === 1'b1) ? 1'b1 : 1'b0;

assign cyc3 = ((i_lxi | i_lda | i_sta) |
	      (i_jmp | i_call | i_ret)) ? 1'b1 : 1'bz;

assign cyc3 = (flagout[flagz] & (i_cz | i_rz)) ? 1'b1 : 1'bz;

assign cyc3 = (flagout[flagcy] & i_jc) ? 1'b1 : 1'bz;

assign cycgo3 = (cyc3 === 1'b1) ? 1'b1 : 1'b0;

assign cyc4 = (i_lda | i_sta) ? 1'b1 : 1'bz;

assign cyc4 = (i_call) ? 1'b1 : 1'bz;

assign cyc4 = (flagout[flagz] & i_cz) ? 1'b1 : 1'bz;

assign cycgo4 = (cyc4 === 1'b1) ? 1'b1 : 1'b0;

assign cyc5 = (i_call) ? 1'b1 : 1'bz;

assign cyc5 = (flagout[flagz] & i_cz) ? 1'b1 : 1'bz;

assign cycgo5 = (cyc5 === 1'b1) ? 1'b1 : 1'b0;

//----------------NO OF READ CYCLE DECIDING-------------------------

	      
assign cycr2 = ((i_mvi_r | i_lxi | i_lda | i_sta) |
	       (i_mov_rm) |
	       (i_aci | i_jmp | i_call | i_jc | i_cz | i_ret)) ? 1'b1 : 1'bz;
	     
assign cycr2 = (i_rz & flagout[flagz]) ? 1'b1 : 1'bz;

assign cycrd2 = (cycr2 === 1'b1) ? 1'b1 : 1'b0;

assign cycr3 = ((i_lxi | i_lda | i_sta) |
	       (i_jmp | i_call | i_ret)) ? 1'b1 : 1'bz;

assign cycr3 = (flagout[flagz] & (i_cz | i_rz)) ? 1'b1 : 1'bz;

assign cycr3 = (flagout[flagcy] & i_jc) ? 1'b1 : 1'bz;

assign cycrd3 = (cycr3 === 1'b1) ? 1'b1 : 1'b0;

assign cycr4 = i_lda;

assign cycrd4 = (cycr4 === 1'b1) ? 1'b1 : 1'b0;

//----------------NO OF WRITE CYCLE DECIDING------------------------

assign cycw2 = i_mov_mr;

assign cycwr2 = (cycw2 === 1'b1) ? 1'b1 : 1'b0;

assign cycwr3 = 1'b0;

assign cycw4 = i_sta | i_call | (i_cz & flagout[flagz]);

assign cycwr4 = (cycw4 === 1'b1) ? 1'b1 : 1'b0;

assign cycwr5 = i_call | (i_cz & flagout[flagz]);

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//-------------DECIDING NO CYCLE FOR EACH INSTRUCTION---------------
//------------------------------------------------------------------

reg cycen;
reg [4:0] cycgo;
always @(posedge cycen or cycgo1 or cycgo2 or cycgo3 or cycgo4 or cycgo5)begin
	cycgo <= {cycgo5, cycgo4, cycgo3, cycgo2, cycgo1};
	
end

reg [3:0] cycrd;
always @(posedge cycen or cycrd2 or cycrd3 or cycrd4) begin
	cycrd <= {cycrd4, cycrd3, cycrd2};
end

reg [3:0] cycwr;
always @(posedge cycen or cycwr2 or cycwr3 or cycwr4 or cycwr5) begin
	cycwr <= {cycwr5, cycwr4, cycwr3, cycwr2};
end

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------



//------------------------------------------------------------------
//------------------------CYCLE UPDATE LOGIC------------------------
//------------------------------------------------------------------



reg [5:0] cstate;
wire [5:0] nstate;
reg [2:0] fetch;
reg [1:0] mem_wr, mem_rd;

always @(posedge clk) begin
	case(cstate)
		state_of: begin
				fetch <= fetch >> 1;
				if(fetch == 3'b000) begin
					fetch <= 3'b111;
					cycgo <= cycgo >> 1;
					cycrd <= cycrd >> 1; //this is imp
					cycwr <= cycwr >> 1; //this is imp
				end
		end
		state_mr: begin
				mem_rd <= mem_rd >> 1;
				if(mem_rd == 2'b00) begin
					mem_rd <= 2'b11;
					cycgo <= cycgo >> 1;
					cycrd <= cycrd >> 1;
					cycwr <= cycwr >> 1; //this is imp
				end
		end
		state_mw: begin
				mem_wr <= mem_wr >> 1;
				if(mem_wr == 2'b00) begin
					mem_wr <= 2'b11;
					cycgo <= cycgo >> 1;
					cycrd <= cycrd >> 1; //this is imp
					cycwr <= cycwr >> 1;
				end
		end		
	endcase
end

//------------------------------------------------------------------
//----------------------CURRENT STATE LOGIC-------------------------
//------------------------------------------------------------------

always @(posedge clk) begin
	if(rst) begin
		cstate <= state_rs;
		reset_all <= 1;
	end
	else begin
		cstate <= nstate;
		reset_all <= 0;
	end
end

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------RESETTING LOGIC---------------------------
//------------------------------------------------------------------

reg reset_all;
assign rgres = (reset_all) ? 8'hff : 8'b0;
assign pcres = reset_all;
assign irres = reset_all;
assign wres = reset_all;
assign zres = reset_all;
assign flagres = reset_all;
assign spres_l = reset_all;
assign spres_h = reset_all;
assign tempres = reset_all;
assign pointres_l = reset_all;
assign pointres_h = reset_all;

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------NEXT STATE LOGIC--------------------------
//------------------------------------------------------------------


assign nstate = (rst & cstate == state_rs) ? state_rs : 5'bz;
assign nstate = (~rst & cstate == state_rs) ? state_of : 5'bz;


assign nstate = (cstate == state_of & cycgo > 5'b00001 & fetch == 3'b000 & cycrd[0]) ? state_mr : 5'bz;
assign nstate = (cstate == state_of & cycgo > 5'b00001 & fetch == 3'b000 & cycwr[0]) ? state_mw : 5'bz;
assign nstate = (cstate == state_of & cycgo == 5'b00001 & fetch == 3'b000) ? state_of : 5'bz;

assign nstate = (cstate == state_mr & cycgo > 5'b00001 & mem_rd == 2'b00 & cycrd[0]) ? state_mr : 5'bz;
assign nstate = (cstate == state_mr & cycgo > 5'b00001 & mem_rd == 2'b00 & cycwr[0]) ? state_mw : 5'bz;
assign nstate = (cstate == state_mr & cycgo == 5'b00001 & mem_rd == 2'b00) ? state_of : 5'bz;

assign nstate = (cstate == state_mw & cycgo > 5'b00001 & mem_wr == 2'b00 & cycrd[0]) ? state_mr : 5'bz;
assign nstate = (cstate == state_mw & cycgo > 5'b00001 & mem_wr == 2'b00 & cycwr[0]) ? state_mw : 5'bz;
assign nstate = (cstate == state_mw & cycgo == 5'b00001 & mem_wr == 2'b00) ? state_of : 5'bz;

assign nstate = (cstate == state_of & fetch != 3'b000) ? state_of : 5'bz;
assign nstate = (cstate == state_mr & mem_rd != 2'b00) ? state_mr : 5'bz;
assign nstate = (cstate == state_mw & mem_wr != 2'b00) ? state_mw : 5'bz;



//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


//------------------------------------------------------------------
//------------------------CONTROL SIGNALS---------------------------
//------------------------------------------------------------------

reg readreg, writereg;
reg readpc, writepc, incpc, incpc2;
reg readhl, writehl;
reg aleout, rdout_, wrout_, iomout_;
reg writeireg;
reg en_id, en_od, en_al, en_ah, en_tx;
reg [1:0] regpcount;
reg [2:0] wzcount;
reg writewz, readwz, reada, writea;
reg alu, readtemp, writetemp;
reg [1:0] pointcount, spcount;
reg readpoint, writepoint;
reg decsp, incsp, readsp, writesp;
reg decsponce;
always @(posedge clk or posedge reset_all) begin
	case(cstate)
		state_rs:begin
			fetch <= 3'b111;
			mem_wr <= 2'b11;
			mem_rd <= 2'b11;
			iomout_ <= 0;
		end
		state_of: begin
				casex(fetch)
					3'b111: begin
						aleout <= 1;
						rdout_ <= 1;
						wrout_ <= 1;
						en_al <= 1;
						en_ah <= 1;
						readpc <= 1;
						en_id <= 0;
						en_od <= 0;
						en_tx = 0;
						writeireg <= 0;
						readreg <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writepc <= 0;
						writereg <= 0;
						cycen <= 0;
						readhl <= 0;
						writehl <= 0;
						regpcount <= 2'b11;
						wzcount <= 3'b111;
						readwz <= 0;
						writewz <= 0;
						reada <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						spcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;
						decsp <= 0;
						incsp <= 0;
						readsp <= 0;
						writesp <= 0;
						decsponce <= 0;
					end
					3'b011: begin
						aleout <= 0;
						rdout_ <= 0;
						wrout_ <= 1;
						en_al <= 0;
						en_ah <= 0;
						readpc <= 0;
						writeireg <= 1;
						en_id <= 1;
						en_od <= 0;
						readreg <= 0;
						en_tx <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writepc <= 0;
						writereg <= 0;
						cycen <= 0;
						readhl <= 0;
						writehl <= 0;
						regpcount <= 2'b11;
						wzcount <= 3'b111;
						writewz <= 0;
						readwz <= 0;
						reada <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						spcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;
						decsp <= 0;
						incsp <= 0;
						readsp <= 0;
						writesp <= 0;
					end
					3'b001: begin
						rdout_ <= 0;
						writeireg <= 0;
						aleout <= 0;
						readpc <= 1;
						en_id <= 1;
						en_od <= 0;
						readreg <= 0;
						incpc <= 1;
						incpc2 <= 0;
						writepc <= 1;
						writereg <= 0;
						cycen <= 1;
						wrout_ <= 1;
						en_tx <= 0;
						readhl <= 0;
						writehl <= 0;
						en_al <= 0;
						en_ah <= 0;
						regpcount <= 2'b11;
						wzcount <= 3'b111;
						writewz <= 0;
						readwz <= 0;
						reada <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						spcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;
						incsp <= 0;
						readsp <= 0;
						writesp <= 0;
						decsp <= 0;
					end
					3'b000: begin
						if(i_mov_rr) begin
							writereg <= 1;
							readreg <= 1;
							en_tx <= 1;
						end
						else if(i_alu) begin
							en_tx <= 0;
							writereg <= 0;
							readreg <= 1;
							reada <= 1;
							writea <= 1;
							alu <= 1;
						end
						else begin
							readreg <= 0;
							en_tx <= 0;
							writereg <= 0;
							reada <= 0;
							writea <= 0;
							alu <= 0;
						end
						rdout_ <= 1;
						wrout_ <= 1;
						readpc <= 0;
						en_id <= 0;
						en_od <= 0;
						writeireg <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writepc <= 0;
						cycen <= 0;
						aleout <= 0;
						readhl <= 0;
						writehl <= 0;
						en_al <= 0;
						en_ah <= 0;
						regpcount <= 2'b11;
						wzcount <= 3'b111;
						writewz <= 0;
						readwz <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						spcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;	
					end
				endcase
		end
		state_mr: begin
				casex(mem_rd)
					2'b11: begin
						writereg <= 0;
						readreg <= 0;
						aleout <= 1;
						en_al <= 1;
						en_ah <= 1;
						if(i_mov) begin
							readhl <= 1;
							readpc <= 0;
							readwz <= 0;
							readsp <= 0;
						end
						else if((i_lda | i_sta) & wzcount == 3'b100) begin
							readpc <= 0;
							readhl <= 0;
							readwz <= 1;
							readsp <= 0;
						end
						else if(i_ret | i_rz) begin
							readpc <= 0;
							readhl <= 0;
							readwz <= 0;
							readsp <= 1;	
						end
						else if((~i_lda & ~i_sta) | ((i_lda | i_sta) & wzcount != 3'b100)) begin
							readpc <= 1;
							readhl <= 0;
							readwz <= 0;
							readsp <= 0;
						end
						rdout_ <= 1;
						wrout_ <= 1;
						en_id <= 0;
						en_od <= 0;
						writeireg <= 0;
						en_tx <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writepc <= 0;
						cycen <= 0;
						writehl <= 0;
						writewz <= 0;	
						reada <= 0;	
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						readpoint <= 0;
						writepoint <= 0;
						spcount <= 2'b11;
						decsp <= 0;
						incsp <= 0;		
						writesp <= 0;		
					end
					2'b01: begin
						aleout <= 0;
						rdout_ <= 0;
						wrout_ <= 1;
						en_al <= 0;
						en_ah <= 0;
						if ((i_jc & ~flagout[flagcy]) | (i_cz & ~flagout[flagz])) begin
							readpc <= 1;
							writepc <= 1;
							incpc2 <= 1;
						end
						else if(i_ret | i_rz) begin
							readsp <= 1;
							writesp <= 1;
							incsp <= 1;
						end
						else if(~i_mov & wzcount != 3'b100) begin
							readpc <= 1;
							writepc <= 1;
							incpc <= 1;
						end
						en_id <= 1;
						en_od <= 0;
						writeireg <= 0;
						readreg <= 0;
						if(i_lda | i_sta) begin
							writewz <= 1;
						end
						else if(i_aci) begin
							writetemp <= 1;
						end
						else if(i_jmp |( i_jc & flagout[flagcy]) | i_call | (i_cz & flagout[flagz]) | i_ret | i_rz)
							writepoint <= 1;
						else
							writereg <= 1;
						cycen <= 0;
						readhl <= 0;
						readwz <= 0;
						en_tx <= 0;
						writehl <= 0;
						regpcount <= regpcount << 1;
						wzcount <= wzcount << 1;
						alu <= 0;
						reada <= 0;
						writea <= 0;
						readtemp <= 0;
						pointcount <= pointcount << 1;
						readpoint <= 0;
						spcount <= 2'b11;
						decsp <= 0;
					end
					2'b00: begin
						rdout_ <= 1;
						readpc <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writereg <= 0;
						writewz <= 0;
						aleout <= 0;
						wrout_ <= 1;
						en_id <= 0;
						en_od <= 0;
						writeireg <= 0;
						readreg <= 0;
						cycen <= 0;
						readhl <= 0;
						en_ah <= 0;
						en_al <= 0;
						en_tx <= 0;
						writehl <= 0;
						readwz <= 0;
						alu <= 0;
						writetemp <= 0;
						if(i_aci) begin
							reada <= 1;
							writea <= 1;
							readtemp <= 1;
						end
						if((i_jmp | i_ret | i_rz | ( i_jc & flagout[flagcy])) & pointcount == 2'b00) begin
							readpoint <= 1;
							writepc <= 1;
						end
						else begin
							writepc <= 0;
						end
						writepoint <= 0;
						spcount <= 2'b11;
						incsp <= 0;
						decsp <= 0;
						readsp <= 0;
						writesp <= 0;
					end
				endcase
		end
		state_mw: begin
				casex(mem_wr)
					2'b11: begin
						writereg <= 0;
						readreg <= 0;
						aleout <= 1;
						en_al <= 1;
						en_ah <= 1;
						if(i_mov) begin
							readhl <= 1;
							readpc <= 0;
							readwz <= 0;
							readsp <= 0;
							decsp <= 0;
							writesp <= 0;
						end
						else if((~i_sta & ~i_call & ~i_cz) | (i_sta & wzcount != 3'b100)) begin
							readpc <= 1;
							readhl <= 0;
							readwz <= 0;
							readsp <= 0;
							decsp <= 0;
							writesp <= 0;
						end
						else if(i_sta & wzcount == 3'b100) begin
							readpc <= 0;
							readhl <= 0;
							readwz <= 1;
							readsp <= 0;
							decsp <= 0;
							writesp <= 0;
						end
						else if(i_call | i_cz) begin
							readpc <= 0;
							readhl <= 0;
							readwz <= 0;
							readsp <= 1;
						end
						rdout_ <= 1;
						wrout_ <= 1;
						en_id <= 0;
						en_od <= 0;
						writeireg <= 0;
						en_tx <= 0;
						incpc <= 0;
						incpc2 <= 0;
						writepc <= 0;
						cycen <= 0;
						writehl <= 0;
						writewz <= 0;
						reada <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;		
						writetemp <= 0;
						pointcount <= 2'b11;	
						readpoint <= 0;
						writepoint <= 0;
						incsp <= 0;			
					end
					2'b01: begin
						aleout <= 0;
						rdout_ <= 1;
						wrout_ <= 0;
						en_al <= 0;
						en_ah <= 0;
						incpc2 <= 0;
						if(i_call | i_cz) begin
							readpc <= 1;
							writepc <= 0;
							incpc <= 0;
						end
						else if(~i_mov & wzcount != 3'b100) begin
							readpc <= 1;
							writepc <= 1;
							incpc <= 1;
						end
						en_id <= 0;
						en_od <= 1;
						writeireg <= 0;
						if(i_sta) begin
							reada <= 1;
						end
						else if(i_call | i_cz)
							readpc <= 1;
						else
							readreg <= 1;
						writereg <= 0;
						cycen <= 0;
						en_tx <= 0;
						readhl <= 0;
						writehl <= 0;
						readwz <= 0;
						wzcount <= wzcount << 1;
						writewz <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;
						incsp <= 0;
						spcount <= spcount << 1;
						if(i_call | (i_cz & flagout[flagz])) begin
							writesp <= 1;
							decsp <= 1;
							readsp <= 1;
						end
					end
					2'b00: begin
						rdout_ <= 1;
						readpc <= 0;
						writereg <= 0;
						wrout_ <= 1;
						en_id <= 0;
						en_od <= 0;
						writeireg <= 0;
						readreg <= 0;
						incpc <= 0;
						incpc2 <= 0;
						if((i_call | i_cz) & spcount == 2'b00) begin
							writepc <= 1;
						end
						else
							writepc <= 0;
						cycen <= 0;
						aleout <= 0;
						en_ah <= 0;
						en_al <= 0;
						en_tx <= 0;
						readhl <= 0;
						writehl <= 0;
						readwz <= 0;
						writewz <= 0;
						reada <= 0;
						writea <= 0;
						alu <= 0;
						readtemp <= 0;
						writetemp <= 0;
						pointcount <= 2'b11;
						readpoint <= 0;
						writepoint <= 0;
						decsp <= 0;
						incsp <= 0;
						readsp <= 0;
						writesp <= 0;
					end
				endcase
		end
	endcase
end

assign IOM_ = iomout_;
assign RD_ = rdout_;
assign WR_ = wrout_;
assign ALE = aleout;
assign chk_id = en_id;
assign chk_od = en_od;
assign chk_al = en_al;
assign chk_ah = en_ah;
assign chk_tx = en_tx;
assign pcr = readpc;
assign pcw = writepc;
assign incdec_in = (readpc & incpc) ? pcout : 16'bz;
assign incdec_in2 = (readpc & incpc2) ? pcout : 16'bz;
assign pcin = (writepc & incpc) ? incdec_out : 16'bz;
assign pcin = (writepc & incpc2) ? incdec_out2 : 16'bz;
assign incdec_op = (incpc & readpc & writepc) ? 1'b1 : 1'bz;
assign incdec_op2 = (incpc2 & readpc & writepc) ? 1'b1 : 1'bz;
assign addrout = (readpc & aleout) ? pcout : 16'bz;
assign addrout = (readhl & aleout) ? hlout : 16'bz;
assign addrout = (readwz & aleout) ? wzout : 16'bz;
assign addrout = (readsp & aleout & (i_call | i_cz)) ? ({spout_h, spout_l} - 1'b1) : 16'bz;
assign addrout = (readsp & aleout & (i_ret | i_rz)) ? {spout_h, spout_l} : 16'bz;
assign iregen = writeireg;
assign iregin = (writeireg) ? datain : 8'bz;
assign hlr = readhl;
assign wzr = readwz;
assign alu_op = (alu & iregdata[5:3] == 3'b001) ? 2'b00 : 2'bz;
assign alu_op = (alu & iregdata[5:3] == 3'b011) ? 2'b01 : 2'bz;
assign alu_op = (alu & iregdata[5:3] == 3'b100) ? 2'b10 : 2'bz;
assign alu_op = (alu & iregdata[5:3] == 3'b111) ? 2'b11 : 2'bz;
assign opr_1 = (alu & reada) ? regdata[7] : 8'bz;
assign opr_2 = (alu & readreg) ? dataout : 8'bz;
assign regin[7] = (alu & writea) ? res : 8'bz;
assign flagin = (alu) ? flagnew : 8'bz;
assign flagw = (alu) ? 1'b1 : 1'bz;
assign alu_op = (i_aci) ? 2'b00 : 2'bz;
assign opr_1 = (i_aci & reada) ? regdata[7] : 8'bz;
assign opr_2 = (i_aci & readtemp) ? tempout : 8'bz;
assign regin[7] = (i_aci & writea) ? res : 8'bz;
assign flagin = (i_aci & reada) ? flagnew : 8'bz;
assign flagw = (i_aci & reada) ? 1'b1 : 1'bz;
assign pointw_l = ((i_jmp | i_call | (i_cz & flagout[flagz]) | (i_jc & flagout[flagcy])) & writepoint & pointcount == 2'b10) ? 1'b1 : 1'bz;
assign pointw_h = ((i_jmp | i_call | (i_cz & flagout[flagz]) | (i_jc & flagout[flagcy])) & writepoint & pointcount == 2'b00) ? 1'b1 : 1'bz;
assign pointw_l = ((i_ret | i_rz) & writepoint & pointcount == 2'b10) ? 1'b1 : 1'bz;
assign pointw_h = ((i_ret | i_rz) & writepoint & pointcount == 2'b00) ? 1'b1 : 1'bz;
assign pcin = (writepc & (i_jmp | i_ret | i_rz | (i_jc & flagout[flagcy])) & pointcount == 2'b00 & readpoint) ? {pointout_h, pointout_l} : 16'bz;
assign incdec_op = (decsp & readsp) ? 1'b0 : 1'bz;
assign incdec_in = (decsp & readsp) ? {spout_h, spout_l} : 16'bz;
assign {spin_h, spin_l} = (decsp  & writesp) ? incdec_out : 16'bz;
assign incdec_op = (incsp & readsp) ? 1'b1 : 1'bz;
assign incdec_in = (incsp & readsp) ? {spout_h, spout_l} : 16'bz;
assign {spin_h, spin_l} = (incsp  & writesp) ? incdec_out : 16'bz;
assign pcin = (writepc & (i_call | i_cz ) & spcount == 2'b00) ? {pointout_h, pointout_l} : 16'bz;
//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------

//------------------------------------------------------------------
//--------------------------SELECTING REGISTERS---------------------
//------------------------------------------------------------------

wire [2:0] regsel_d, regsel_s, regphigh, regplow, regpcurrent;
wire [1:0] regpsel;
reg [7:0] regwrite, regread;
assign regsel_d = (i_lxi) ? regpcurrent : iregdata[5:3];
assign regsel_s = iregdata[2:0];
assign regpsel = iregdata[5:4];
assign rgr = (readreg) ? regread : 8'bz;
assign rgw = (writereg) ? regwrite : 8'bz;
assign regphigh = {regpsel, 1'b0};
assign regplow = {regpsel, 1'b1};
assign regpcurrent = (regpcount == 2'b10) ? regplow : 3'bz;
assign regpcurrent = (regpcount == 2'b00) ? regphigh : 3'bz;
assign ww = (writewz & wzcount == 3'b100) ? 1'b1 : 1'bz;
assign zw = (writewz & wzcount == 3'b110) ? 1'b1 : 1'bz;
assign rgw[7] = (writewz & wzcount == 3'b000) ? 1'b1 : 1'bz;
assign rgr[7] = (reada & wzcount == 3'b000) ? 1'b1 : 1'bz;
assign tempw = (writetemp & i_aci) ? 1'b1 : 1'bz;
assign rgw[7] = (writea & alu & iregdata[5:3] != 3'b111) ? 1'b1 : 1'bz;
assign rgw[7] = (writea & i_aci) ? 1'b1 : 1'bz;
assign spr_l = (readsp & (i_call | i_ret | i_rz | (i_cz & flagout[flagz]))) ? 1'b1 : 1'bz;
assign spr_h = (readsp & (i_call | i_ret | i_rz | (i_cz & flagout[flagz]))) ? 1'b1 : 1'bz;
assign spw_l = (writesp & (i_call | i_ret | i_rz | (i_cz & flagout[flagz]))) ? 1'b1 : 1'bz;
assign spw_h = (writesp & (i_call | i_ret | i_rz | (i_cz & flagout[flagz]))) ? 1'b1 : 1'bz;

always @(regsel_d) begin
	case(regsel_d)
		3'b000: regwrite <= 8'b00000001;
		3'b001: regwrite <= 8'b00000010;
		3'b010: regwrite <= 8'b00000100;
		3'b011: regwrite <= 8'b00001000;
		3'b100: regwrite <= 8'b00010000;
		3'b101: regwrite <= 8'b00100000;
		3'b110: regwrite <= 8'b01000000;
		3'b111: regwrite <= 8'b10000000;
	endcase
end

always @(regsel_s) begin
	case(regsel_s)
		3'b000: regread <= 8'b00000001;
		3'b001: regread <= 8'b00000010;
		3'b010: regread <= 8'b00000100;
		3'b011: regread <= 8'b00001000;
		3'b100: regread <= 8'b00010000;
		3'b101: regread <= 8'b00100000;
		3'b110: regread <= 8'b01000000;
		3'b111: regread <= 8'b10000000;
	endcase
end

assign regin[0] = (writereg & regsel_d == 3'b000) ? datain : 8'bz; //b
assign regin[1] = (writereg & regsel_d == 3'b001) ? datain : 8'bz; //c
assign regin[2] = (writereg & regsel_d == 3'b010) ? datain : 8'bz; //d
assign regin[3] = (writereg & regsel_d == 3'b011) ? datain : 8'bz; //e
assign regin[4] = (writereg & regsel_d == 3'b100) ? datain : 8'bz; //h
assign regin[5] = (writereg & regsel_d == 3'b101) ? datain : 8'bz; //l
assign regin[6] = (writereg & regsel_d == 3'b110) ? datain : 8'bz;
assign regin[7] = (writereg & regsel_d == 3'b111) ? datain : 8'bz; //a

assign dataout = (readreg & regsel_s == 3'b000) ? regout[0] : 8'bz; //b
assign dataout = (readreg & regsel_s == 3'b001) ? regout[1] : 8'bz; //c
assign dataout = (readreg & regsel_s == 3'b010) ? regout[2] : 8'bz; //d
assign dataout = (readreg & regsel_s == 3'b011) ? regout[3] : 8'bz; //e
assign dataout = (readreg & regsel_s == 3'b100) ? regout[4] : 8'bz; //h
assign dataout = (readreg & regsel_s == 3'b101) ? regout[5] : 8'bz; //l
assign dataout = (readreg & regsel_s == 3'b110) ? regout[6] : 8'bz;
assign dataout = (readreg & regsel_s == 3'b111) ? regout[7] : 8'bz; //a

assign win = (ww) ? datain : 8'bz;
assign zin = (zw) ? datain : 8'bz;
assign regin[7] = (rgw[7] & i_lda) ? datain : 8'bz;
assign dataout = (rgr[7] & i_sta) ? regout[7] : 8'bz;
assign tempin = (tempw) ? datain : 8'bz;
assign pointin_l = (pointw_l) ? datain : 8'bz;
assign pointin_h = (pointw_h) ? datain : 8'bz;
assign dataout = (readpc & spcount == 2'b10 & (i_call | (i_cz & flagout[flagz]))) ? pcout[15:8] : 8'bz;
assign dataout = (readpc & spcount == 2'b00 & (i_call | (i_cz & flagout[flagz]))) ? pcout[7:0] : 8'bz;

//------------------------------------------------------------------
//------------------------------------------------------------------
//------------------------------------------------------------------


endmodule
