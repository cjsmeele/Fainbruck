A very naive adder for 2 ascii digits (because I honestly can't be bothered to write good Brainfuck)
It doesn't handle results larger than 9

Look up "brainfuck mandelbrot" for a more interesting demonstration :)

,------------------------------------------------>  Read first digit
,------------------------------------------------   Read second digit
[<+>-]                                              Increment fst until snd zero
<++++++++++++++++++++++++++++++++++++++++++++++++.  Print result
