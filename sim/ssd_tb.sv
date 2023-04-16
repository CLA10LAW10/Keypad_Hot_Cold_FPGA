`timescale 1ns / 1ps

module ssd_tb();

  logic clk; // Input
  logic [3:0] btn; // Input
  logic [3:0] sw; // Input
  logic [3:0] led; // Output
  logic led_g; // Output
  logic [6:0] seg; // Output
  logic chip_sel; // Output
  //wire  [7:0] keypad; // Inout - bidirectional signal from DUT
  logic [3:0] row;
  logic [3:0] col;

  parameter CP = 20;

  ssd_top ssd_uut (.*);

  // Process made to toggle the clock every 5ns.
  always
  begin
    clk <= 1'b1;
    #(CP/2);
    clk <= 1'b0;
    #(CP/2);
  end

  // Simulation inputs.
  initial
  begin
    // Set initial values
    sw = 4'd0;
    row = 4'd0;
    btn = 4'd1;
    #CP;
    btn = 4'd0;

    // Hit a key
    row = 4'b1011;
    col = 4'b1011;
    #(CP * 125_000);
    row = 4'b0000;

    // Toggle the display
    btn = 4'd2;
    #(CP * 900_000);
    btn = 4'd0;
    #(CP * 500_000);

    // Tooggle the display again
    btn = 4'd2;
    #(CP * 900_000);
    btn = 4'd0;
    #(CP * 500_000);

    // Reset the device
    btn = 4'd1;
    #(CP * 900_000);
    btn = 4'd0;
    #(CP * 500_000);

    // Switch to two digits
    sw = 4'd1;
    #(CP * 500_000);

    // Hit a key
    row = 4'b1011;
    #(CP * 125_000);
    row = 4'b0000;
    #(CP * 4_000_000);

    // Hit a second key after time has passed while displaying two different digits
    row = 4'b1101;
    #(CP * 125_000);
    row = 4'b0000;
    // Let it cycle with the new two digits.
    #(CP * 4_000_000);

    $finish;

  end

endmodule
