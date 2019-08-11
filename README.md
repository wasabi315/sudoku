# 🔢 The Sudoku Solver

9x9 sudoku solver using **Knuth's Algorithm X** written in Haskell.

## 🚀 Usage

This program reads the flattened sudoku problem from STDIN and writes the flattened solution to STDOUT.

e.g.)
```sh
$ cat problem
.9..2..6...36....58....5.....5..1..4.6.....9.1..7..2.....5....37....46...2..9..8.
$ stack exec sudoku < problem
597423168213687945846915372975261834462358791138749256689572413751834629324196587
```
