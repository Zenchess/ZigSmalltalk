"Test file for new Integer methods"

"Test abs"
(-5) abs printString.
Transcript show: ' = 5'. Transcript cr.
5 abs printString.
Transcript show: ' = 5'. Transcript cr.
0 abs printString.
Transcript show: ' = 0'. Transcript cr.

"Test even"
4 even printString.
Transcript show: ' = true'. Transcript cr.
5 even printString.
Transcript show: ' = false'. Transcript cr.
0 even printString.
Transcript show: ' = true'. Transcript cr.

"Test odd"
4 odd printString.
Transcript show: ' = false'. Transcript cr.
5 odd printString.
Transcript show: ' = true'. Transcript cr.

"Test positive"
5 positive printString.
Transcript show: ' = true'. Transcript cr.
0 positive printString.
Transcript show: ' = false'. Transcript cr.
(-5) positive printString.
Transcript show: ' = false'. Transcript cr.

"Test negative"
(-5) negative printString.
Transcript show: ' = true'. Transcript cr.
0 negative printString.
Transcript show: ' = false'. Transcript cr.
5 negative printString.
Transcript show: ' = false'. Transcript cr.

"Test sign"
5 sign printString.
Transcript show: ' = 1'. Transcript cr.
0 sign printString.
Transcript show: ' = 0'. Transcript cr.
(-5) sign printString.
Transcript show: ' = -1'. Transcript cr.

"Test max:"
3 max: 5 printString.
Transcript show: ' = 5'. Transcript cr.
5 max: 3 printString.
Transcript show: ' = 5'. Transcript cr.
(-3) max: (-5) printString.
Transcript show: ' = -3'. Transcript cr.

"Test min:"
3 min: 5 printString.
Transcript show: ' = 3'. Transcript cr.
5 min: 3 printString.
Transcript show: ' = 3'. Transcript cr.
(-3) min: (-5) printString.
Transcript show: ' = -5'. Transcript cr.

"Test between:and:"
5 between: 1 and: 10 printString.
Transcript show: ' = true'. Transcript cr.
0 between: 1 and: 10 printString.
Transcript show: ' = false'. Transcript cr.
11 between: 1 and: 10 printString.
Transcript show: ' = false'. Transcript cr.
1 between: 1 and: 10 printString.
Transcript show: ' = true'. Transcript cr.
10 between: 1 and: 10 printString.
Transcript show: ' = true'. Transcript cr.

"Test timesRepeat:"
5 timesRepeat: [Transcript show: 'hi '].
Transcript cr.

Transcript show: 'All tests completed!'. Transcript cr.
