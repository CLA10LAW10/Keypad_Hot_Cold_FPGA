`timescale 1ns / 1ps

module keypad_decoder(
    input clk,
    input rst,
    input [3:0] row,
    output logic [3:0] col,
    output [3:0] decode_out,
    output is_a_key_pressed
  );

  logic [19:0] sclk;
  logic [3:0] decode_reg;
  logic is_a_key_pressed_reg;
  logic is_a_key_pressed_pulse;

  int count;

  always @ (posedge clk, posedge rst, posedge is_a_key_pressed_reg)
  begin
    if (rst == 1'b1)
    begin
      is_a_key_pressed_pulse = 1'b0;
      count = 1'b0;
    end
    else if (is_a_key_pressed_reg == 1'b1) begin
      is_a_key_pressed_pulse = 1'b1;
    end
    else
    begin

      if (is_a_key_pressed_pulse)
      begin
        if (count == 50_000_000)
        begin
          is_a_key_pressed_pulse = 1'b0;
          count <= 0;
        end
        else
        begin
          count <= count + 1;
        end
      end


    end
  end

  always_ff @ (posedge clk, posedge rst)
  begin
    if (rst == 1'b1)
    begin
      decode_reg = 4'b0;
      is_a_key_pressed_reg = 1'b0;
      sclk = 'b0;
    end
    else
    begin
      //////////////////////////////////////
      //          Scan Column 1           //
      //////////////////////////////////////
      // 2ms
      if (sclk == 20'd100000)
      begin
        // Column 1
        col = 4'b0111;
        sclk = sclk + 1;
      end
      // Check row pins
      else if (sclk == 20'd100008)
      begin
        // Row 1
        if (row == 4'b0111)
        begin
          decode_reg = 4'b0001; //1
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 2
        else if (row == 4'b1011)
        begin
          decode_reg = 4'b0100; //4
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 3
        else if (row == 4'b1101)
        begin
          decode_reg = 4'b0111; //7
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 4
        else if (row == 4'b1110)
        begin
          decode_reg = 4'b0000; //0
          is_a_key_pressed_reg = 1'b1;
        end
        else
        begin
          decode_reg = decode_reg;
          is_a_key_pressed_reg = 0;
        end

        sclk = sclk + 1;
      end
      //////////////////////////////////////
      //          Scan Column 2           //
      //////////////////////////////////////

      else if (sclk == 20'd200000)
      begin
        // Column 2
        col = 4'b1011;
        sclk = sclk + 1;
      end
      // Check row pins
      else if (sclk == 20'd200008)
      begin
        // Row 1
        if (row == 4'b0111)
        begin
          decode_reg = 4'b0010; // 2
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 2
        else if (row == 4'b1011)
        begin
          decode_reg = 4'b0101; // 5
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 3
        else if (row == 4'b1101)
        begin
          decode_reg = 4'b1000; // 8
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 4
        else if (row == 4'b1110)
        begin
          decode_reg = 4'b1111; // F
          is_a_key_pressed_reg = 1'b1;
        end
        else
        begin
          decode_reg = decode_reg;
          is_a_key_pressed_reg = 0;
        end

        sclk = sclk + 1;
      end
      //////////////////////////////////////
      //          Scan Column 3           //
      //////////////////////////////////////

      else if (sclk == 20'd300000)
      begin
        // Column 3
        col = 4'b1101;
        sclk = sclk + 1;
      end
      // Check row pins
      else if (sclk == 20'd300008)
      begin
        // Row 1
        if (row == 4'b0111)
        begin
          decode_reg = 4'b0011; // 3
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 2
        else if (row == 4'b1011)
        begin
          decode_reg = 4'b0110; // 6
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 3
        else if (row == 4'b1101)
        begin
          decode_reg = 4'b1001; // 9
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 4
        else if (row == 4'b1110)
        begin
          decode_reg = 4'b1110; // E
          is_a_key_pressed_reg = 1'b1;
        end
        else
        begin
          decode_reg = decode_reg;
          is_a_key_pressed_reg = 0;
        end

        sclk = sclk + 1;
      end
      //////////////////////////////////////
      //          Scan Column 4           //
      //////////////////////////////////////

      else if (sclk == 20'd400000)
      begin
        // Column 4
        col = 4'b1110;
        sclk = sclk + 1;
      end
      // Check row pins
      else if (sclk == 20'd400008)
      begin
        // Row 1
        if (row == 4'b0111)
        begin
          decode_reg = 4'b1010; // A
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 2
        else if (row == 4'b1011)
        begin
          decode_reg = 4'b1011; // b
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 3
        else if (row == 4'b1101)
        begin
          decode_reg = 4'b1100; // C
          is_a_key_pressed_reg = 1'b1;
        end
        // Row 4
        else if (row == 4'b1110)
        begin
          decode_reg = 4'b1101; // d
          is_a_key_pressed_reg = 1'b1;
        end
        else
        begin
          decode_reg = decode_reg;
          is_a_key_pressed_reg = 0;
        end

        sclk = 'b0;
      end
      else
      begin
        sclk = sclk + 1;
      end
    end // end clk
  end // end always_ff

  assign is_a_key_pressed = is_a_key_pressed_pulse;
  assign decode_out = decode_reg;

endmodule
