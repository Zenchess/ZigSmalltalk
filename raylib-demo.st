| screenWidth screenHeight bgColor textColor |

screenWidth := 800.
screenHeight := 450.

bgColor := Color new.
bgColor r: 245; g: 245; b: 245; a: 255.

textColor := Color new.
textColor r: 200; g: 200; b: 200; a: 255.

'Raylib' ffiCall: #InitWindow with: { screenWidth. screenHeight. 'Zig Smalltalk - Basic Window' }.
'Raylib' ffiCall: #SetTargetFPS with: { 60 }.

[ ('Raylib' ffiCall: #WindowShouldClose with: #()) not ] whileTrue: [
    'Raylib' ffiCall: #BeginDrawing with: #().
    'Raylib' ffiCall: #ClearBackground with: { bgColor bytes }.
    'Raylib' ffiCall: #DrawText with: {
        'Congrats! You created your first window from Smalltalk!'.
        190.
        200.
        20.
        textColor bytes
    }.
    'Raylib' ffiCall: #EndDrawing with: #().
].

'Raylib' ffiCall: #CloseWindow with: #().
