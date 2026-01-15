"Test script for Smalltalk TUI components
 Run this after loading the TUI to verify functionality"

Transcript show: 'Testing Smalltalk TUI...'; cr!

"Test 1: TuiColor"
Transcript show: '  Test 1: TuiColor...'; cr!
| c |
c := TuiColor r: 128 g: 64 b: 255.
(c red = 128 and: [c green = 64 and: [c blue = 255]]) ifFalse: [
    self error: 'TuiColor test failed'
].
Transcript show: '    PASS: TuiColor basic creation'; cr!

c := TuiColor fromHex: '#1e1e2e'.
(c red = 30 and: [c green = 30 and: [c blue = 46]]) ifFalse: [
    self error: 'TuiColor fromHex test failed'
].
Transcript show: '    PASS: TuiColor fromHex'; cr!

"Test 2: Theme colors"
Transcript show: '  Test 2: Theme colors...'; cr!
| bg fg |
bg := Theme background.
fg := Theme foreground.
(bg isKindOf: TuiColor) ifFalse: [self error: 'Theme background not a TuiColor'].
(fg isKindOf: TuiColor) ifFalse: [self error: 'Theme foreground not a TuiColor'].
Transcript show: '    PASS: Theme colors'; cr!

"Test 3: TuiStyle"
Transcript show: '  Test 3: TuiStyle...'; cr!
| style |
style := TuiStyle default.
style foreground isNil ifFalse: [
    (style foreground isKindOf: TuiColor) ifFalse: [self error: 'Style foreground invalid']
].
style := TuiStyle normal.
Transcript show: '    PASS: TuiStyle'; cr!

"Test 4: TuiRect"
Transcript show: '  Test 4: TuiRect...'; cr!
| rect |
rect := TuiRect x: 5 y: 10 width: 20 height: 15.
(rect x = 5 and: [rect y = 10 and: [rect width = 20 and: [rect height = 15]]]) ifFalse: [
    self error: 'TuiRect basic test failed'
].
(rect containsX: 10 y: 15) ifFalse: [self error: 'TuiRect containsPoint test failed'].
(rect containsX: 4 y: 15) ifTrue: [self error: 'TuiRect containsPoint should be false'].
Transcript show: '    PASS: TuiRect'; cr!

"Test 5: Cell"
Transcript show: '  Test 5: Cell...'; cr!
| cell1 cell2 |
cell1 := Cell char: $A style: TuiStyle normal.
cell2 := Cell char: $A style: TuiStyle normal.
(cell1 char = $A) ifFalse: [self error: 'Cell char test failed'].
Transcript show: '    PASS: Cell'; cr!

"Test 6: TuiTextArea"
Transcript show: '  Test 6: TuiTextArea...'; cr!
| ta |
ta := TuiTextArea new.
ta text: 'Hello
World'.
(ta lineCount = 2) ifFalse: [self error: 'TextArea line count wrong'].
(ta text = 'Hello
World') ifFalse: [self error: 'TextArea text round-trip failed'].
ta moveDown.
(ta currentLine = 'World') ifFalse: [self error: 'TextArea moveDown failed'].
Transcript show: '    PASS: TuiTextArea'; cr!

"Test 7: TuiListView"
Transcript show: '  Test 7: TuiListView...'; cr!
| lv |
lv := TuiListView new.
lv items: #('One' 'Two' 'Three').
(lv selectedIndex = 1) ifFalse: [self error: 'ListView initial selection wrong'].
lv selectNext.
(lv selectedIndex = 2) ifFalse: [self error: 'ListView selectNext failed'].
(lv selectedItem = 'Two') ifFalse: [self error: 'ListView selectedItem wrong'].
Transcript show: '    PASS: TuiListView'; cr!

"Test 8: TuiInputLine"
Transcript show: '  Test 8: TuiInputLine...'; cr!
| il |
il := TuiInputLine new.
il insertChar: $H.
il insertChar: $i.
(il text = 'Hi') ifFalse: [self error: 'InputLine insert failed'].
il deleteBackward.
(il text = 'H') ifFalse: [self error: 'InputLine delete failed'].
Transcript show: '    PASS: TuiInputLine'; cr!

"Test 9: TuiTranscriptPanel"
Transcript show: '  Test 9: TuiTranscriptPanel...'; cr!
| tp |
tp := TuiTranscriptPanel new.
tp addLine: 'Test line'.
Transcript show: '    PASS: TuiTranscriptPanel'; cr!

"Test 10: AnsiTerminal key constants"
Transcript show: '  Test 10: AnsiTerminal key constants...'; cr!
(AnsiTerminal keyUp = 16r1001) ifFalse: [self error: 'keyUp constant wrong'].
(AnsiTerminal keyEnter = 13) ifFalse: [self error: 'keyEnter constant wrong'].
(AnsiTerminal keyBackspace = 127) ifFalse: [self error: 'keyBackspace constant wrong'].
Transcript show: '    PASS: Key constants'; cr!

Transcript show: ''; cr!
Transcript show: 'All TUI tests passed!'; cr!
Transcript show: ''; cr!
Transcript show: 'The TUI is ready. Start with: TuiApp start'; cr!
