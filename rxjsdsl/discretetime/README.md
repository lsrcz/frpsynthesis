# Synthesis utility

## How to use

Write the JSON query to a file (see example query `samplequery.txt`).

Call the script by passing the name of the input file and the output file as arguments.

`./main.rkt samplequery.txt output.txt`

## Configuration

The utility currently uses the discrete time reduction and a very limited DSL (map, scan, merge, with higher order functions add1, sub1, (x => 1), (x => -1)). 

