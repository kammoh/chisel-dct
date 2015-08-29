# Toy DCT Project
Discrete Cosine Transform Using Multiple Constant Multiplication in Chisel HCL

(https://dl.acm.org/citation.cfm?id=1240234&coll=ACM&dl=ACM)

## Content 
- Forward and Inverse 8x8 Integer DCT
- Verification harness

## Features
- Combinational, Pipelined, and Iterative (2-cycle) implementations choosable in the Config file
- Generic verification
- Generic configuration using ChiselConfig
- Use of JNA in a custom Chisel transform


## Synthesis results
- ~3000 LUTs and 100 MHz on Xilinx Series 7 FPGAs
