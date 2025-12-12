!ByteArray methodsFor!

uint8At: offset
    "Read unsigned 8-bit value at 0-based byte offset"
    <primitive: 770>
    ^self primitiveFailed!

uint16At: offset
    "Read unsigned 16-bit value at 0-based byte offset (little-endian)"
    <primitive: 771>
    ^self primitiveFailed!

uint32At: offset
    "Read unsigned 32-bit value at 0-based byte offset (little-endian)"
    <primitive: 772>
    ^self primitiveFailed!

int8At: offset
    "Read signed 8-bit value at 0-based byte offset"
    <primitive: 773>
    ^self primitiveFailed!

int16At: offset
    "Read signed 16-bit value at 0-based byte offset (little-endian)"
    <primitive: 774>
    ^self primitiveFailed!

int32At: offset
    "Read signed 32-bit value at 0-based byte offset (little-endian)"
    <primitive: 775>
    ^self primitiveFailed!

float32At: offset
    "Read 32-bit float value at 0-based byte offset"
    <primitive: 776>
    ^self primitiveFailed!

float64At: offset
    "Read 64-bit float value at 0-based byte offset"
    <primitive: 777>
    ^self primitiveFailed!

uint8At: offset put: value
    "Write unsigned 8-bit value at 0-based byte offset"
    <primitive: 780>
    ^self primitiveFailed!

uint16At: offset put: value
    "Write unsigned 16-bit value at 0-based byte offset (little-endian)"
    <primitive: 781>
    ^self primitiveFailed!

uint32At: offset put: value
    "Write unsigned 32-bit value at 0-based byte offset (little-endian)"
    <primitive: 782>
    ^self primitiveFailed!

int8At: offset put: value
    "Write signed 8-bit value at 0-based byte offset"
    <primitive: 783>
    ^self primitiveFailed!

int16At: offset put: value
    "Write signed 16-bit value at 0-based byte offset (little-endian)"
    <primitive: 784>
    ^self primitiveFailed!

int32At: offset put: value
    "Write signed 32-bit value at 0-based byte offset (little-endian)"
    <primitive: 785>
    ^self primitiveFailed!

float32At: offset put: value
    "Write 32-bit float value at 0-based byte offset"
    <primitive: 786>
    ^self primitiveFailed!

float64At: offset put: value
    "Write 64-bit float value at 0-based byte offset"
    <primitive: 787>
    ^self primitiveFailed!

address
    "Return pointer to the byte data as an integer (for FFI)"
    <primitive: 788>
    ^self primitiveFailed!

!

Object subclass: #ExternalStructure
    instanceVariableNames: 'bytes'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ExternalStructure class methodsFor!

new
    "Create a new instance with a properly sized byte buffer"
    ^self basicNew initialize!

byteSize
    "Subclasses must override to return their struct size in bytes"
    ^0!

fromBytes: aByteArray
    "Create an instance wrapping existing byte data"
    ^self basicNew bytes: aByteArray!

!

!ExternalStructure methodsFor!

initialize
    "Initialize with a zero-filled byte buffer of the correct size"
    bytes := ByteArray new: self class byteSize.
    ^self!

bytes
    "Return the raw byte data"
    ^bytes!

bytes: aByteArray
    "Set the raw byte data"
    bytes := aByteArray.
    ^self!

byteSize
    "Return the size in bytes of this structure"
    ^self class byteSize!

address
    "Return the memory address of the byte data (for FFI calls)"
    ^bytes address!

copy
    "Return a copy of this structure with its own byte buffer"
    ^self class fromBytes: bytes copy!

uint8At: offset
    ^bytes uint8At: offset!

uint16At: offset
    ^bytes uint16At: offset!

uint32At: offset
    ^bytes uint32At: offset!

int8At: offset
    ^bytes int8At: offset!

int16At: offset
    ^bytes int16At: offset!

int32At: offset
    ^bytes int32At: offset!

float32At: offset
    ^bytes float32At: offset!

float64At: offset
    ^bytes float64At: offset!

uint8At: offset put: value
    ^bytes uint8At: offset put: value!

uint16At: offset put: value
    ^bytes uint16At: offset put: value!

uint32At: offset put: value
    ^bytes uint32At: offset put: value!

int8At: offset put: value
    ^bytes int8At: offset put: value!

int16At: offset put: value
    ^bytes int16At: offset put: value!

int32At: offset put: value
    ^bytes int32At: offset put: value!

float32At: offset put: value
    ^bytes float32At: offset put: value!

float64At: offset put: value
    ^bytes float64At: offset put: value!

printOn: aStream
    aStream nextPutAll: self class name.
    aStream nextPutAll: '('.
    aStream nextPutAll: bytes printString.
    aStream nextPut: $)!

!

ExternalStructure subclass: #RaylibColor
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RaylibColor class methodsFor!

byteSize
    "Color is 4 bytes: r, g, b, a (each uint8)"
    ^4!

r: r g: g b: b a: a
    "Create a Color with the given RGBA values (0-255)"
    | color |
    color := self new.
    color r: r; g: g; b: b; a: a.
    ^color!

r: r g: g b: b
    "Create a Color with full opacity"
    ^self r: r g: g b: b a: 255!

white
    ^self r: 255 g: 255 b: 255 a: 255!

black
    ^self r: 0 g: 0 b: 0 a: 255!

red
    ^self r: 255 g: 0 b: 0 a: 255!

green
    ^self r: 0 g: 255 b: 0 a: 255!

blue
    ^self r: 0 g: 0 b: 255 a: 255!

yellow
    ^self r: 255 g: 255 b: 0 a: 255!

rayWhite
    "Raylib's default background color"
    ^self r: 245 g: 245 b: 245 a: 255!

!

!RaylibColor methodsFor!

r
    "Red component (0-255)"
    ^self uint8At: 0!

r: value
    self uint8At: 0 put: value!

g
    "Green component (0-255)"
    ^self uint8At: 1!

g: value
    self uint8At: 1 put: value!

b
    "Blue component (0-255)"
    ^self uint8At: 2!

b: value
    self uint8At: 2 put: value!

a
    "Alpha component (0-255)"
    ^self uint8At: 3!

a: value
    self uint8At: 3 put: value!

printOn: aStream
    aStream nextPutAll: 'Color(r: '.
    aStream nextPutAll: self r printString.
    aStream nextPutAll: ' g: '.
    aStream nextPutAll: self g printString.
    aStream nextPutAll: ' b: '.
    aStream nextPutAll: self b printString.
    aStream nextPutAll: ' a: '.
    aStream nextPutAll: self a printString.
    aStream nextPut: $)!

!

ExternalStructure subclass: #RaylibVector2
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RaylibVector2 class methodsFor!

byteSize
    "Vector2 is 8 bytes: x, y (each float32)"
    ^8!

x: x y: y
    "Create a Vector2 with the given coordinates"
    | vec |
    vec := self new.
    vec x: x; y: y.
    ^vec!

zero
    ^self x: 0.0 y: 0.0!

one
    ^self x: 1.0 y: 1.0!

!

!RaylibVector2 methodsFor!

x
    "X coordinate (float32)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "Y coordinate (float32)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

printOn: aStream
    aStream nextPutAll: 'Vector2('.
    aStream nextPutAll: self x printString.
    aStream nextPutAll: ', '.
    aStream nextPutAll: self y printString.
    aStream nextPut: $)!

!

ExternalStructure subclass: #RaylibRectangle
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RaylibRectangle class methodsFor!

byteSize
    "Rectangle is 16 bytes: x, y, width, height (each float32)"
    ^16!

x: x y: y width: w height: h
    "Create a Rectangle"
    | rect |
    rect := self new.
    rect x: x; y: y; width: w; height: h.
    ^rect!

!

!RaylibRectangle methodsFor!

x
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

width
    ^self float32At: 8!

width: value
    self float32At: 8 put: value!

height
    ^self float32At: 12!

height: value
    self float32At: 12 put: value!

printOn: aStream
    aStream nextPutAll: 'Rectangle(x: '.
    aStream nextPutAll: self x printString.
    aStream nextPutAll: ' y: '.
    aStream nextPutAll: self y printString.
    aStream nextPutAll: ' w: '.
    aStream nextPutAll: self width printString.
    aStream nextPutAll: ' h: '.
    aStream nextPutAll: self height printString.
    aStream nextPut: $)!

!
