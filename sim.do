setenv LMC_TIMEUNIT -9
vlib work
vmap work work
vcom -work work "fifo.vhd"
vcom -work work "arp_top.vhd"
vcom -work work "arp_tb.vhd"
vsim +notimingchecks -L work work.arp_tb -wlf sim_arp.wlf

add wave -noupdate -group arp_tb
add wave -noupdate -group arp_tb -radix hexadecimal /arp_tb/*
add wave -noupdate -group arp_tb/arp_top_inst
add wave -noupdate -group arp_tb/arp_top_inst -radix hexadecimal /arp_tb/arp_top_inst/*
add wave -noupdate -group arp_tb/arp_top_inst/data_rx_fifo -radix hexadecimal /arp_tb/arp_top_inst/data_rx_fifo/*
add wave -noupdate -group arp_tb/arp_top_inst/data_rx_fifo
add wave -noupdate -group arp_tb/arp_top_inst/data_valid_rx_fifo -radix hexadecimal /arp_tb/arp_top_inst/data_valid_rx_fifo/*
add wave -noupdate -group arp_tb/arp_top_inst/data_valid_rx_fifo
add wave -noupdate -group arp_tb/arp_top_inst/data_tx_fifo -radix hexadecimal /arp_tb/arp_top_inst/data_tx_fifo/*
add wave -noupdate -group arp_tb/arp_top_inst/data_tx_fifo
run -all
wave zoom full