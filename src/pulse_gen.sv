`timescale 1ns / 1ps

module pulse_gen(
    input clk,
    input rst,
    output pulse
);

logic [18:0] counter;
logic pulse_reg;
always_ff @(posedge clk, posedge rst) begin
    if (rst == 1) begin
        counter <= 0;
        pulse_reg <= 0;
    end
    else begin
        counter <= counter + 1;
        if (counter == 0) begin
            pulse_reg <= 1;
        end
        else begin
            pulse_reg <= 0;
        end
    end
end

assign pulse = pulse_reg;

endmodule