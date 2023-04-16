`timescale 1ns / 1ps

module ssd_top(
    input clk,
    input [3:0] btn,
    input [3:0] sw,
    output [3:0] led,
    output led_g,
    output [6:0] seg,
    output chip_sel,
    // input [3:0] row, // Used for simulation
    // output logic [3:0] col // Used for sim
    inout [7:0] keypad
  );

  parameter clk_freq = 50_000_000;
  parameter stable_time = 10; // ms

  // logic [7:0] keypad; // Used for simulation
  // assign keypad = {row,col}; // Used for simulation

  logic rst;
  logic c_sel_pulse;
  logic btn1_debounce;
  logic btn1_pulse;
  logic [6:0] output_ssd1;
  logic [6:0] output_ssd2;
  logic [13:0] two_ssd;
  logic [6:0]output_ssd;
  logic [6:0] seg_reg;
  logic [7:0] keypad_w;
  logic c_sel;
  logic is_a_key_pressed;
  logic is_a_key_pressed_pulse;
  logic key_press;
  logic [3:0] decode_out;
  logic [3:0] decode1;
  logic [3:0] decode2;

  keypad_decoder de_inst1(
                   .clk(clk),
                   .rst(rst),
                   .row(keypad[7:4]),
                   .col(keypad[3:0]),
                   .decode_out(decode_out), // Pass to SSD Controller
                   .is_a_key_pressed(is_a_key_pressed));

  disp_ctrl ssd_1(
              //.disp_val(sw),
              .disp_val(decode_out),
              .seg_out(output_ssd));

  disp_ctrl ssd_2a(
              //.disp_val(sw),
              .disp_val(decode1),
              .seg_out(output_ssd1));

  disp_ctrl ssd_2b(
              //.disp_val(sw),
              .disp_val(decode2),
              .seg_out(output_ssd2));

  debounce  #(
              .clk_freq(clk_freq),
              .stable_time(stable_time)
            )
            db_inst_1
            (
              .clk(clk),
              .rst(rst),
              .button(btn[1]),
              .result(btn1_debounce));



  single_pulse_detector #(
                          .detect_type(2'b0)
                        )
                        pls_inst_1 (
                          .clk(clk),
                          .rst(rst),
                          .input_signal(btn1_debounce),
                          .output_pulse(btn1_pulse));

  debounce  #(
              .clk_freq(clk_freq),
              .stable_time(stable_time)
            )
            db_inst_2
            (
              .clk(clk),
              .rst(rst),
              .button(is_a_key_pressed),
              .result(is_a_key_pressed_db));

  single_pulse_detector #(
                          .detect_type(2'b0)
                        )
                        pls_inst_2 (
                          .clk(clk),
                          .rst(rst),
                          .input_signal(is_a_key_pressed_db),
                          .output_pulse(is_a_key_pressed_pulse));

  pulse_gen pg_inst1(
              .clk(clk),
              .rst(rst),
              .pulse(c_sel_pulse)
            );

  always_ff @(posedge clk, posedge rst)
    //always_comb
  begin
    if (rst == 1)
    begin
      c_sel = 1'b0;
    end
    else
    begin
      if (~sw[0]) // Part 1, One toggle display.
      begin
        seg_reg = output_ssd;
        if (btn1_pulse)
        begin
          c_sel = ~c_sel;
        end
      end
      else // Part 2, Two Displays starting from the left
      begin
        if (c_sel_pulse)
        begin
          c_sel = ~c_sel;
        end
        seg_reg = c_sel ? output_ssd1 : output_ssd2; // Should assign tweo different numbers, left to right
      end
    end
  end

  always_ff @ (posedge clk, posedge rst)
  begin
    if (rst == 1)
    begin
      key_press = 0;
      decode1 = 4'b0;
      decode2 = 4'b0;
    end
    else
    begin

      if (is_a_key_pressed_pulse)
      begin
        if (~key_press)
        begin
          decode1 = decode_out;
          key_press = 1'b1;
        end
        else
        begin
          decode2 = decode_out;
          key_press = 1'b0;
        end
      end
    end
  end

  assign seg = seg_reg;
  assign led_g = is_a_key_pressed;
  assign rst = btn[0];
  assign led = decode_out;
  assign chip_sel = c_sel;

endmodule
