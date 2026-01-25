"Test exception handling with | operator"
Transcript show: 'A'; cr.

[
    Transcript show: 'B'; cr.
    true | false.
    Transcript show: 'C'; cr.
] on: Error do: [:ex |
    Transcript show: 'Caught error: '; show: ex printString; cr
].

Transcript show: 'D'; cr.
quit.
