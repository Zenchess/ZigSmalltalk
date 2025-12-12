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

uint64At: offset
    "Read unsigned 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 778>
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

int64At: offset
    "Read signed 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 779>
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

uint64At: offset put: value
    "Write unsigned 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 789>
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

int64At: offset put: value
    "Write signed 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 793>
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

uint8At: offset ^bytes uint8At: offset!
uint16At: offset ^bytes uint16At: offset!
uint32At: offset ^bytes uint32At: offset!
uint64At: offset ^bytes uint64At: offset!
int8At: offset ^bytes int8At: offset!
int16At: offset ^bytes int16At: offset!
int32At: offset ^bytes int32At: offset!
int64At: offset ^bytes int64At: offset!
float32At: offset ^bytes float32At: offset!
float64At: offset ^bytes float64At: offset!
uint8At: offset put: value ^bytes uint8At: offset put: value!
uint16At: offset put: value ^bytes uint16At: offset put: value!
uint32At: offset put: value ^bytes uint32At: offset put: value!
uint64At: offset put: value ^bytes uint64At: offset put: value!
int8At: offset put: value ^bytes int8At: offset put: value!
int16At: offset put: value ^bytes int16At: offset put: value!
int32At: offset put: value ^bytes int32At: offset put: value!
int64At: offset put: value ^bytes int64At: offset put: value!
float32At: offset put: value ^bytes float32At: offset put: value!
float64At: offset put: value ^bytes float64At: offset put: value!

!

ExternalStructure subclass: #struct_Vector2
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Vector2 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^8!

!

!struct_Vector2 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

!

ExternalStructure subclass: #Vector2
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Vector2 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^8!

!

!Vector2 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

!

ExternalStructure subclass: #struct_Vector3
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Vector3 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^12!

!

!struct_Vector3 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

z
    "z field (f32, offset 8)"
    ^self float32At: 8!

z: value
    self float32At: 8 put: value!

!

ExternalStructure subclass: #Vector3
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Vector3 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^12!

!

!Vector3 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

z
    "z field (f32, offset 8)"
    ^self float32At: 8!

z: value
    self float32At: 8 put: value!

!

ExternalStructure subclass: #struct_Vector4
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Vector4 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!struct_Vector4 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

z
    "z field (f32, offset 8)"
    ^self float32At: 8!

z: value
    self float32At: 8 put: value!

w
    "w field (f32, offset 12)"
    ^self float32At: 12!

w: value
    self float32At: 12 put: value!

!

ExternalStructure subclass: #Vector4
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Vector4 class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!Vector4 methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

z
    "z field (f32, offset 8)"
    ^self float32At: 8!

z: value
    self float32At: 8 put: value!

w
    "w field (f32, offset 12)"
    ^self float32At: 12!

w: value
    self float32At: 12 put: value!

!

ExternalStructure subclass: #Quaternion
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Quaternion class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!Quaternion methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

z
    "z field (f32, offset 8)"
    ^self float32At: 8!

z: value
    self float32At: 8 put: value!

w
    "w field (f32, offset 12)"
    ^self float32At: 12!

w: value
    self float32At: 12 put: value!

!

ExternalStructure subclass: #struct_Matrix
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Matrix class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^64!

!

!struct_Matrix methodsFor!

m0
    "m0 field (f32, offset 0)"
    ^self float32At: 0!

m0: value
    self float32At: 0 put: value!

m4
    "m4 field (f32, offset 4)"
    ^self float32At: 4!

m4: value
    self float32At: 4 put: value!

m8
    "m8 field (f32, offset 8)"
    ^self float32At: 8!

m8: value
    self float32At: 8 put: value!

m12
    "m12 field (f32, offset 12)"
    ^self float32At: 12!

m12: value
    self float32At: 12 put: value!

m1
    "m1 field (f32, offset 16)"
    ^self float32At: 16!

m1: value
    self float32At: 16 put: value!

m5
    "m5 field (f32, offset 20)"
    ^self float32At: 20!

m5: value
    self float32At: 20 put: value!

m9
    "m9 field (f32, offset 24)"
    ^self float32At: 24!

m9: value
    self float32At: 24 put: value!

m13
    "m13 field (f32, offset 28)"
    ^self float32At: 28!

m13: value
    self float32At: 28 put: value!

m2
    "m2 field (f32, offset 32)"
    ^self float32At: 32!

m2: value
    self float32At: 32 put: value!

m6
    "m6 field (f32, offset 36)"
    ^self float32At: 36!

m6: value
    self float32At: 36 put: value!

m10
    "m10 field (f32, offset 40)"
    ^self float32At: 40!

m10: value
    self float32At: 40 put: value!

m14
    "m14 field (f32, offset 44)"
    ^self float32At: 44!

m14: value
    self float32At: 44 put: value!

m3
    "m3 field (f32, offset 48)"
    ^self float32At: 48!

m3: value
    self float32At: 48 put: value!

m7
    "m7 field (f32, offset 52)"
    ^self float32At: 52!

m7: value
    self float32At: 52 put: value!

m11
    "m11 field (f32, offset 56)"
    ^self float32At: 56!

m11: value
    self float32At: 56 put: value!

m15
    "m15 field (f32, offset 60)"
    ^self float32At: 60!

m15: value
    self float32At: 60 put: value!

!

ExternalStructure subclass: #Matrix
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Matrix class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^64!

!

!Matrix methodsFor!

m0
    "m0 field (f32, offset 0)"
    ^self float32At: 0!

m0: value
    self float32At: 0 put: value!

m4
    "m4 field (f32, offset 4)"
    ^self float32At: 4!

m4: value
    self float32At: 4 put: value!

m8
    "m8 field (f32, offset 8)"
    ^self float32At: 8!

m8: value
    self float32At: 8 put: value!

m12
    "m12 field (f32, offset 12)"
    ^self float32At: 12!

m12: value
    self float32At: 12 put: value!

m1
    "m1 field (f32, offset 16)"
    ^self float32At: 16!

m1: value
    self float32At: 16 put: value!

m5
    "m5 field (f32, offset 20)"
    ^self float32At: 20!

m5: value
    self float32At: 20 put: value!

m9
    "m9 field (f32, offset 24)"
    ^self float32At: 24!

m9: value
    self float32At: 24 put: value!

m13
    "m13 field (f32, offset 28)"
    ^self float32At: 28!

m13: value
    self float32At: 28 put: value!

m2
    "m2 field (f32, offset 32)"
    ^self float32At: 32!

m2: value
    self float32At: 32 put: value!

m6
    "m6 field (f32, offset 36)"
    ^self float32At: 36!

m6: value
    self float32At: 36 put: value!

m10
    "m10 field (f32, offset 40)"
    ^self float32At: 40!

m10: value
    self float32At: 40 put: value!

m14
    "m14 field (f32, offset 44)"
    ^self float32At: 44!

m14: value
    self float32At: 44 put: value!

m3
    "m3 field (f32, offset 48)"
    ^self float32At: 48!

m3: value
    self float32At: 48 put: value!

m7
    "m7 field (f32, offset 52)"
    ^self float32At: 52!

m7: value
    self float32At: 52 put: value!

m11
    "m11 field (f32, offset 56)"
    ^self float32At: 56!

m11: value
    self float32At: 56 put: value!

m15
    "m15 field (f32, offset 60)"
    ^self float32At: 60!

m15: value
    self float32At: 60 put: value!

!

ExternalStructure subclass: #struct_Color
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Color class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^4!

!

!struct_Color methodsFor!

r
    "r field (u8, offset 0)"
    ^self uint8At: 0!

r: value
    self uint8At: 0 put: value!

g
    "g field (u8, offset 1)"
    ^self uint8At: 1!

g: value
    self uint8At: 1 put: value!

b
    "b field (u8, offset 2)"
    ^self uint8At: 2!

b: value
    self uint8At: 2 put: value!

a
    "a field (u8, offset 3)"
    ^self uint8At: 3!

a: value
    self uint8At: 3 put: value!

!

ExternalStructure subclass: #Color
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Color class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^4!

!

!Color methodsFor!

r
    "r field (u8, offset 0)"
    ^self uint8At: 0!

r: value
    self uint8At: 0 put: value!

g
    "g field (u8, offset 1)"
    ^self uint8At: 1!

g: value
    self uint8At: 1 put: value!

b
    "b field (u8, offset 2)"
    ^self uint8At: 2!

b: value
    self uint8At: 2 put: value!

a
    "a field (u8, offset 3)"
    ^self uint8At: 3!

a: value
    self uint8At: 3 put: value!

!

ExternalStructure subclass: #struct_Rectangle
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Rectangle class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!struct_Rectangle methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

width
    "width field (f32, offset 8)"
    ^self float32At: 8!

width: value
    self float32At: 8 put: value!

height
    "height field (f32, offset 12)"
    ^self float32At: 12!

height: value
    self float32At: 12 put: value!

!

ExternalStructure subclass: #Rectangle
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Rectangle class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!Rectangle methodsFor!

x
    "x field (f32, offset 0)"
    ^self float32At: 0!

x: value
    self float32At: 0 put: value!

y
    "y field (f32, offset 4)"
    ^self float32At: 4!

y: value
    self float32At: 4 put: value!

width
    "width field (f32, offset 8)"
    ^self float32At: 8!

width: value
    self float32At: 8 put: value!

height
    "height field (f32, offset 12)"
    ^self float32At: 12!

height: value
    self float32At: 12 put: value!

!

ExternalStructure subclass: #struct_Image
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Image class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_Image methodsFor!

data
    "data field (?*anyopaque, offset 0)"
    ^self uint8At: 0!

data: value
    self uint8At: 0 put: value!

width
    "width field (c_int, offset 8)"
    ^self int32At: 8!

width: value
    self int32At: 8 put: value!

height
    "height field (c_int, offset 12)"
    ^self int32At: 12!

height: value
    self int32At: 12 put: value!

mipmaps
    "mipmaps field (c_int, offset 16)"
    ^self int32At: 16!

mipmaps: value
    self int32At: 16 put: value!

format
    "format field (c_int, offset 20)"
    ^self int32At: 20!

format: value
    self int32At: 20 put: value!

!

ExternalStructure subclass: #Image
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Image class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!Image methodsFor!

data
    "data field (?*anyopaque, offset 0)"
    ^self uint8At: 0!

data: value
    self uint8At: 0 put: value!

width
    "width field (c_int, offset 8)"
    ^self int32At: 8!

width: value
    self int32At: 8 put: value!

height
    "height field (c_int, offset 12)"
    ^self int32At: 12!

height: value
    self int32At: 12 put: value!

mipmaps
    "mipmaps field (c_int, offset 16)"
    ^self int32At: 16!

mipmaps: value
    self int32At: 16 put: value!

format
    "format field (c_int, offset 20)"
    ^self int32At: 20!

format: value
    self int32At: 20 put: value!

!

ExternalStructure subclass: #struct_Texture
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Texture class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^20!

!

!struct_Texture methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

width
    "width field (c_int, offset 4)"
    ^self int32At: 4!

width: value
    self int32At: 4 put: value!

height
    "height field (c_int, offset 8)"
    ^self int32At: 8!

height: value
    self int32At: 8 put: value!

mipmaps
    "mipmaps field (c_int, offset 12)"
    ^self int32At: 12!

mipmaps: value
    self int32At: 12 put: value!

format
    "format field (c_int, offset 16)"
    ^self int32At: 16!

format: value
    self int32At: 16 put: value!

!

ExternalStructure subclass: #Texture
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Texture class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^20!

!

!Texture methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

width
    "width field (c_int, offset 4)"
    ^self int32At: 4!

width: value
    self int32At: 4 put: value!

height
    "height field (c_int, offset 8)"
    ^self int32At: 8!

height: value
    self int32At: 8 put: value!

mipmaps
    "mipmaps field (c_int, offset 12)"
    ^self int32At: 12!

mipmaps: value
    self int32At: 12 put: value!

format
    "format field (c_int, offset 16)"
    ^self int32At: 16!

format: value
    self int32At: 16 put: value!

!

ExternalStructure subclass: #Texture2D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Texture2D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^20!

!

!Texture2D methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

width
    "width field (c_int, offset 4)"
    ^self int32At: 4!

width: value
    self int32At: 4 put: value!

height
    "height field (c_int, offset 8)"
    ^self int32At: 8!

height: value
    self int32At: 8 put: value!

mipmaps
    "mipmaps field (c_int, offset 12)"
    ^self int32At: 12!

mipmaps: value
    self int32At: 12 put: value!

format
    "format field (c_int, offset 16)"
    ^self int32At: 16!

format: value
    self int32At: 16 put: value!

!

ExternalStructure subclass: #TextureCubemap
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!TextureCubemap class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^20!

!

!TextureCubemap methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

width
    "width field (c_int, offset 4)"
    ^self int32At: 4!

width: value
    self int32At: 4 put: value!

height
    "height field (c_int, offset 8)"
    ^self int32At: 8!

height: value
    self int32At: 8 put: value!

mipmaps
    "mipmaps field (c_int, offset 12)"
    ^self int32At: 12!

mipmaps: value
    self int32At: 12 put: value!

format
    "format field (c_int, offset 16)"
    ^self int32At: 16!

format: value
    self int32At: 16 put: value!

!

ExternalStructure subclass: #struct_RenderTexture
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_RenderTexture class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!struct_RenderTexture methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

texture
    "texture field (cimport.struct_Texture, offset 4)"
    ^self uint8At: 4!

texture: value
    self uint8At: 4 put: value!

depth
    "depth field (cimport.struct_Texture, offset 24)"
    ^self uint8At: 24!

depth: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #RenderTexture
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RenderTexture class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!RenderTexture methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

texture
    "texture field (cimport.struct_Texture, offset 4)"
    ^self uint8At: 4!

texture: value
    self uint8At: 4 put: value!

depth
    "depth field (cimport.struct_Texture, offset 24)"
    ^self uint8At: 24!

depth: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #RenderTexture2D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RenderTexture2D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!RenderTexture2D methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

texture
    "texture field (cimport.struct_Texture, offset 4)"
    ^self uint8At: 4!

texture: value
    self uint8At: 4 put: value!

depth
    "depth field (cimport.struct_Texture, offset 24)"
    ^self uint8At: 24!

depth: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #struct_NPatchInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_NPatchInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^36!

!

!struct_NPatchInfo methodsFor!

source
    "source field (cimport.struct_Rectangle, offset 0)"
    ^self uint8At: 0!

source: value
    self uint8At: 0 put: value!

left
    "left field (c_int, offset 16)"
    ^self int32At: 16!

left: value
    self int32At: 16 put: value!

top
    "top field (c_int, offset 20)"
    ^self int32At: 20!

top: value
    self int32At: 20 put: value!

right
    "right field (c_int, offset 24)"
    ^self int32At: 24!

right: value
    self int32At: 24 put: value!

bottom
    "bottom field (c_int, offset 28)"
    ^self int32At: 28!

bottom: value
    self int32At: 28 put: value!

layout
    "layout field (c_int, offset 32)"
    ^self int32At: 32!

layout: value
    self int32At: 32 put: value!

!

ExternalStructure subclass: #NPatchInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!NPatchInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^36!

!

!NPatchInfo methodsFor!

source
    "source field (cimport.struct_Rectangle, offset 0)"
    ^self uint8At: 0!

source: value
    self uint8At: 0 put: value!

left
    "left field (c_int, offset 16)"
    ^self int32At: 16!

left: value
    self int32At: 16 put: value!

top
    "top field (c_int, offset 20)"
    ^self int32At: 20!

top: value
    self int32At: 20 put: value!

right
    "right field (c_int, offset 24)"
    ^self int32At: 24!

right: value
    self int32At: 24 put: value!

bottom
    "bottom field (c_int, offset 28)"
    ^self int32At: 28!

bottom: value
    self int32At: 28 put: value!

layout
    "layout field (c_int, offset 32)"
    ^self int32At: 32!

layout: value
    self int32At: 32 put: value!

!

ExternalStructure subclass: #struct_GlyphInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_GlyphInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!struct_GlyphInfo methodsFor!

value
    "value field (c_int, offset 0)"
    ^self int32At: 0!

value: value
    self int32At: 0 put: value!

offsetX
    "offsetX field (c_int, offset 4)"
    ^self int32At: 4!

offsetX: value
    self int32At: 4 put: value!

offsetY
    "offsetY field (c_int, offset 8)"
    ^self int32At: 8!

offsetY: value
    self int32At: 8 put: value!

advanceX
    "advanceX field (c_int, offset 12)"
    ^self int32At: 12!

advanceX: value
    self int32At: 12 put: value!

image
    "image field (cimport.struct_Image, offset 16)"
    ^self uint8At: 16!

image: value
    self uint8At: 16 put: value!

!

ExternalStructure subclass: #GlyphInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!GlyphInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!GlyphInfo methodsFor!

value
    "value field (c_int, offset 0)"
    ^self int32At: 0!

value: value
    self int32At: 0 put: value!

offsetX
    "offsetX field (c_int, offset 4)"
    ^self int32At: 4!

offsetX: value
    self int32At: 4 put: value!

offsetY
    "offsetY field (c_int, offset 8)"
    ^self int32At: 8!

offsetY: value
    self int32At: 8 put: value!

advanceX
    "advanceX field (c_int, offset 12)"
    ^self int32At: 12!

advanceX: value
    self int32At: 12 put: value!

image
    "image field (cimport.struct_Image, offset 16)"
    ^self uint8At: 16!

image: value
    self uint8At: 16 put: value!

!

ExternalStructure subclass: #struct_Font
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Font class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^48!

!

!struct_Font methodsFor!

baseSize
    "baseSize field (c_int, offset 0)"
    ^self int32At: 0!

baseSize: value
    self int32At: 0 put: value!

glyphCount
    "glyphCount field (c_int, offset 4)"
    ^self int32At: 4!

glyphCount: value
    self int32At: 4 put: value!

glyphPadding
    "glyphPadding field (c_int, offset 8)"
    ^self int32At: 8!

glyphPadding: value
    self int32At: 8 put: value!

texture
    "texture field (cimport.struct_Texture, offset 12)"
    ^self uint8At: 12!

texture: value
    self uint8At: 12 put: value!

recs
    "recs field ([*c]cimport.struct_Rectangle, offset 32)"
    ^self uint64At: 32!

recs: value
    self uint64At: 32 put: value!

glyphs
    "glyphs field ([*c]cimport.struct_GlyphInfo, offset 40)"
    ^self uint64At: 40!

glyphs: value
    self uint64At: 40 put: value!

!

ExternalStructure subclass: #Font
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Font class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^48!

!

!Font methodsFor!

baseSize
    "baseSize field (c_int, offset 0)"
    ^self int32At: 0!

baseSize: value
    self int32At: 0 put: value!

glyphCount
    "glyphCount field (c_int, offset 4)"
    ^self int32At: 4!

glyphCount: value
    self int32At: 4 put: value!

glyphPadding
    "glyphPadding field (c_int, offset 8)"
    ^self int32At: 8!

glyphPadding: value
    self int32At: 8 put: value!

texture
    "texture field (cimport.struct_Texture, offset 12)"
    ^self uint8At: 12!

texture: value
    self uint8At: 12 put: value!

recs
    "recs field ([*c]cimport.struct_Rectangle, offset 32)"
    ^self uint64At: 32!

recs: value
    self uint64At: 32 put: value!

glyphs
    "glyphs field ([*c]cimport.struct_GlyphInfo, offset 40)"
    ^self uint64At: 40!

glyphs: value
    self uint64At: 40 put: value!

!

ExternalStructure subclass: #struct_Camera3D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Camera3D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!struct_Camera3D methodsFor!

position
    "position field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

position: value
    self uint8At: 0 put: value!

target
    "target field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

target: value
    self uint8At: 12 put: value!

up
    "up field (cimport.struct_Vector3, offset 24)"
    ^self uint8At: 24!

up: value
    self uint8At: 24 put: value!

fovy
    "fovy field (f32, offset 36)"
    ^self float32At: 36!

fovy: value
    self float32At: 36 put: value!

projection
    "projection field (c_int, offset 40)"
    ^self int32At: 40!

projection: value
    self int32At: 40 put: value!

!

ExternalStructure subclass: #Camera3D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Camera3D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!Camera3D methodsFor!

position
    "position field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

position: value
    self uint8At: 0 put: value!

target
    "target field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

target: value
    self uint8At: 12 put: value!

up
    "up field (cimport.struct_Vector3, offset 24)"
    ^self uint8At: 24!

up: value
    self uint8At: 24 put: value!

fovy
    "fovy field (f32, offset 36)"
    ^self float32At: 36!

fovy: value
    self float32At: 36 put: value!

projection
    "projection field (c_int, offset 40)"
    ^self int32At: 40!

projection: value
    self int32At: 40 put: value!

!

ExternalStructure subclass: #Camera
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Camera class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^44!

!

!Camera methodsFor!

position
    "position field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

position: value
    self uint8At: 0 put: value!

target
    "target field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

target: value
    self uint8At: 12 put: value!

up
    "up field (cimport.struct_Vector3, offset 24)"
    ^self uint8At: 24!

up: value
    self uint8At: 24 put: value!

fovy
    "fovy field (f32, offset 36)"
    ^self float32At: 36!

fovy: value
    self float32At: 36 put: value!

projection
    "projection field (c_int, offset 40)"
    ^self int32At: 40!

projection: value
    self int32At: 40 put: value!

!

ExternalStructure subclass: #struct_Camera2D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Camera2D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_Camera2D methodsFor!

offset
    "offset field (cimport.struct_Vector2, offset 0)"
    ^self uint8At: 0!

offset: value
    self uint8At: 0 put: value!

target
    "target field (cimport.struct_Vector2, offset 8)"
    ^self uint8At: 8!

target: value
    self uint8At: 8 put: value!

rotation
    "rotation field (f32, offset 16)"
    ^self float32At: 16!

rotation: value
    self float32At: 16 put: value!

zoom
    "zoom field (f32, offset 20)"
    ^self float32At: 20!

zoom: value
    self float32At: 20 put: value!

!

ExternalStructure subclass: #Camera2D
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Camera2D class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!Camera2D methodsFor!

offset
    "offset field (cimport.struct_Vector2, offset 0)"
    ^self uint8At: 0!

offset: value
    self uint8At: 0 put: value!

target
    "target field (cimport.struct_Vector2, offset 8)"
    ^self uint8At: 8!

target: value
    self uint8At: 8 put: value!

rotation
    "rotation field (f32, offset 16)"
    ^self float32At: 16!

rotation: value
    self float32At: 16 put: value!

zoom
    "zoom field (f32, offset 20)"
    ^self float32At: 20!

zoom: value
    self float32At: 20 put: value!

!

ExternalStructure subclass: #struct_Mesh
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Mesh class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^120!

!

!struct_Mesh methodsFor!

vertexCount
    "vertexCount field (c_int, offset 0)"
    ^self int32At: 0!

vertexCount: value
    self int32At: 0 put: value!

triangleCount
    "triangleCount field (c_int, offset 4)"
    ^self int32At: 4!

triangleCount: value
    self int32At: 4 put: value!

vertices
    "vertices field ([*c]f32, offset 8)"
    ^self uint64At: 8!

vertices: value
    self uint64At: 8 put: value!

texcoords
    "texcoords field ([*c]f32, offset 16)"
    ^self uint64At: 16!

texcoords: value
    self uint64At: 16 put: value!

texcoords2
    "texcoords2 field ([*c]f32, offset 24)"
    ^self uint64At: 24!

texcoords2: value
    self uint64At: 24 put: value!

normals
    "normals field ([*c]f32, offset 32)"
    ^self uint64At: 32!

normals: value
    self uint64At: 32 put: value!

tangents
    "tangents field ([*c]f32, offset 40)"
    ^self uint64At: 40!

tangents: value
    self uint64At: 40 put: value!

colors
    "colors field ([*c]u8, offset 48)"
    ^self uint64At: 48!

colors: value
    self uint64At: 48 put: value!

indices
    "indices field ([*c]c_ushort, offset 56)"
    ^self uint64At: 56!

indices: value
    self uint64At: 56 put: value!

animVertices
    "animVertices field ([*c]f32, offset 64)"
    ^self uint64At: 64!

animVertices: value
    self uint64At: 64 put: value!

animNormals
    "animNormals field ([*c]f32, offset 72)"
    ^self uint64At: 72!

animNormals: value
    self uint64At: 72 put: value!

boneIds
    "boneIds field ([*c]u8, offset 80)"
    ^self uint64At: 80!

boneIds: value
    self uint64At: 80 put: value!

boneWeights
    "boneWeights field ([*c]f32, offset 88)"
    ^self uint64At: 88!

boneWeights: value
    self uint64At: 88 put: value!

boneMatrices
    "boneMatrices field ([*c]cimport.struct_Matrix, offset 96)"
    ^self uint64At: 96!

boneMatrices: value
    self uint64At: 96 put: value!

boneCount
    "boneCount field (c_int, offset 104)"
    ^self int32At: 104!

boneCount: value
    self int32At: 104 put: value!

vaoId
    "vaoId field (c_uint, offset 108)"
    ^self uint32At: 108!

vaoId: value
    self uint32At: 108 put: value!

vboId
    "vboId field ([*c]c_uint, offset 112)"
    ^self uint64At: 112!

vboId: value
    self uint64At: 112 put: value!

!

ExternalStructure subclass: #Mesh
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Mesh class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^120!

!

!Mesh methodsFor!

vertexCount
    "vertexCount field (c_int, offset 0)"
    ^self int32At: 0!

vertexCount: value
    self int32At: 0 put: value!

triangleCount
    "triangleCount field (c_int, offset 4)"
    ^self int32At: 4!

triangleCount: value
    self int32At: 4 put: value!

vertices
    "vertices field ([*c]f32, offset 8)"
    ^self uint64At: 8!

vertices: value
    self uint64At: 8 put: value!

texcoords
    "texcoords field ([*c]f32, offset 16)"
    ^self uint64At: 16!

texcoords: value
    self uint64At: 16 put: value!

texcoords2
    "texcoords2 field ([*c]f32, offset 24)"
    ^self uint64At: 24!

texcoords2: value
    self uint64At: 24 put: value!

normals
    "normals field ([*c]f32, offset 32)"
    ^self uint64At: 32!

normals: value
    self uint64At: 32 put: value!

tangents
    "tangents field ([*c]f32, offset 40)"
    ^self uint64At: 40!

tangents: value
    self uint64At: 40 put: value!

colors
    "colors field ([*c]u8, offset 48)"
    ^self uint64At: 48!

colors: value
    self uint64At: 48 put: value!

indices
    "indices field ([*c]c_ushort, offset 56)"
    ^self uint64At: 56!

indices: value
    self uint64At: 56 put: value!

animVertices
    "animVertices field ([*c]f32, offset 64)"
    ^self uint64At: 64!

animVertices: value
    self uint64At: 64 put: value!

animNormals
    "animNormals field ([*c]f32, offset 72)"
    ^self uint64At: 72!

animNormals: value
    self uint64At: 72 put: value!

boneIds
    "boneIds field ([*c]u8, offset 80)"
    ^self uint64At: 80!

boneIds: value
    self uint64At: 80 put: value!

boneWeights
    "boneWeights field ([*c]f32, offset 88)"
    ^self uint64At: 88!

boneWeights: value
    self uint64At: 88 put: value!

boneMatrices
    "boneMatrices field ([*c]cimport.struct_Matrix, offset 96)"
    ^self uint64At: 96!

boneMatrices: value
    self uint64At: 96 put: value!

boneCount
    "boneCount field (c_int, offset 104)"
    ^self int32At: 104!

boneCount: value
    self int32At: 104 put: value!

vaoId
    "vaoId field (c_uint, offset 108)"
    ^self uint32At: 108!

vaoId: value
    self uint32At: 108 put: value!

vboId
    "vboId field ([*c]c_uint, offset 112)"
    ^self uint64At: 112!

vboId: value
    self uint64At: 112 put: value!

!

ExternalStructure subclass: #struct_Shader
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Shader class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!struct_Shader methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

locs
    "locs field ([*c]c_int, offset 8)"
    ^self uint64At: 8!

locs: value
    self uint64At: 8 put: value!

!

ExternalStructure subclass: #Shader
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Shader class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!Shader methodsFor!

id
    "id field (c_uint, offset 0)"
    ^self uint32At: 0!

id: value
    self uint32At: 0 put: value!

locs
    "locs field ([*c]c_int, offset 8)"
    ^self uint64At: 8!

locs: value
    self uint64At: 8 put: value!

!

ExternalStructure subclass: #struct_MaterialMap
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_MaterialMap class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^28!

!

!struct_MaterialMap methodsFor!

texture
    "texture field (cimport.struct_Texture, offset 0)"
    ^self uint8At: 0!

texture: value
    self uint8At: 0 put: value!

color
    "color field (cimport.struct_Color, offset 20)"
    ^self uint8At: 20!

color: value
    self uint8At: 20 put: value!

value
    "value field (f32, offset 24)"
    ^self float32At: 24!

value: value
    self float32At: 24 put: value!

!

ExternalStructure subclass: #MaterialMap
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!MaterialMap class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^28!

!

!MaterialMap methodsFor!

texture
    "texture field (cimport.struct_Texture, offset 0)"
    ^self uint8At: 0!

texture: value
    self uint8At: 0 put: value!

color
    "color field (cimport.struct_Color, offset 20)"
    ^self uint8At: 20!

color: value
    self uint8At: 20 put: value!

value
    "value field (f32, offset 24)"
    ^self float32At: 24!

value: value
    self float32At: 24 put: value!

!

ExternalStructure subclass: #struct_Material
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Material class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!struct_Material methodsFor!

shader
    "shader field (cimport.struct_Shader, offset 0)"
    ^self uint8At: 0!

shader: value
    self uint8At: 0 put: value!

maps
    "maps field ([*c]cimport.struct_MaterialMap, offset 16)"
    ^self uint64At: 16!

maps: value
    self uint64At: 16 put: value!

params
    "params field ([4]f32, offset 24)"
    ^self uint8At: 24!

params: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #Material
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Material class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!Material methodsFor!

shader
    "shader field (cimport.struct_Shader, offset 0)"
    ^self uint8At: 0!

shader: value
    self uint8At: 0 put: value!

maps
    "maps field ([*c]cimport.struct_MaterialMap, offset 16)"
    ^self uint64At: 16!

maps: value
    self uint64At: 16 put: value!

params
    "params field ([4]f32, offset 24)"
    ^self uint8At: 24!

params: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #struct_Transform
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Transform class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!struct_Transform methodsFor!

translation
    "translation field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

translation: value
    self uint8At: 0 put: value!

rotation
    "rotation field (cimport.struct_Vector4, offset 12)"
    ^self uint8At: 12!

rotation: value
    self uint8At: 12 put: value!

scale
    "scale field (cimport.struct_Vector3, offset 28)"
    ^self uint8At: 28!

scale: value
    self uint8At: 28 put: value!

!

ExternalStructure subclass: #Transform
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Transform class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!Transform methodsFor!

translation
    "translation field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

translation: value
    self uint8At: 0 put: value!

rotation
    "rotation field (cimport.struct_Vector4, offset 12)"
    ^self uint8At: 12!

rotation: value
    self uint8At: 12 put: value!

scale
    "scale field (cimport.struct_Vector3, offset 28)"
    ^self uint8At: 28!

scale: value
    self uint8At: 28 put: value!

!

ExternalStructure subclass: #struct_BoneInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_BoneInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^36!

!

!struct_BoneInfo methodsFor!

name
    "name field ([32]u8, offset 0)"
    ^self uint8At: 0!

name: value
    self uint8At: 0 put: value!

parent
    "parent field (c_int, offset 32)"
    ^self int32At: 32!

parent: value
    self int32At: 32 put: value!

!

ExternalStructure subclass: #BoneInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!BoneInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^36!

!

!BoneInfo methodsFor!

name
    "name field ([32]u8, offset 0)"
    ^self uint8At: 0!

name: value
    self uint8At: 0 put: value!

parent
    "parent field (c_int, offset 32)"
    ^self int32At: 32!

parent: value
    self int32At: 32 put: value!

!

ExternalStructure subclass: #struct_Model
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Model class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^120!

!

!struct_Model methodsFor!

transform
    "transform field (cimport.struct_Matrix, offset 0)"
    ^self uint8At: 0!

transform: value
    self uint8At: 0 put: value!

meshCount
    "meshCount field (c_int, offset 64)"
    ^self int32At: 64!

meshCount: value
    self int32At: 64 put: value!

materialCount
    "materialCount field (c_int, offset 68)"
    ^self int32At: 68!

materialCount: value
    self int32At: 68 put: value!

meshes
    "meshes field ([*c]cimport.struct_Mesh, offset 72)"
    ^self uint64At: 72!

meshes: value
    self uint64At: 72 put: value!

materials
    "materials field ([*c]cimport.struct_Material, offset 80)"
    ^self uint64At: 80!

materials: value
    self uint64At: 80 put: value!

meshMaterial
    "meshMaterial field ([*c]c_int, offset 88)"
    ^self uint64At: 88!

meshMaterial: value
    self uint64At: 88 put: value!

boneCount
    "boneCount field (c_int, offset 96)"
    ^self int32At: 96!

boneCount: value
    self int32At: 96 put: value!

bones
    "bones field ([*c]cimport.struct_BoneInfo, offset 104)"
    ^self uint64At: 104!

bones: value
    self uint64At: 104 put: value!

bindPose
    "bindPose field ([*c]cimport.struct_Transform, offset 112)"
    ^self uint64At: 112!

bindPose: value
    self uint64At: 112 put: value!

!

ExternalStructure subclass: #Model
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Model class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^120!

!

!Model methodsFor!

transform
    "transform field (cimport.struct_Matrix, offset 0)"
    ^self uint8At: 0!

transform: value
    self uint8At: 0 put: value!

meshCount
    "meshCount field (c_int, offset 64)"
    ^self int32At: 64!

meshCount: value
    self int32At: 64 put: value!

materialCount
    "materialCount field (c_int, offset 68)"
    ^self int32At: 68!

materialCount: value
    self int32At: 68 put: value!

meshes
    "meshes field ([*c]cimport.struct_Mesh, offset 72)"
    ^self uint64At: 72!

meshes: value
    self uint64At: 72 put: value!

materials
    "materials field ([*c]cimport.struct_Material, offset 80)"
    ^self uint64At: 80!

materials: value
    self uint64At: 80 put: value!

meshMaterial
    "meshMaterial field ([*c]c_int, offset 88)"
    ^self uint64At: 88!

meshMaterial: value
    self uint64At: 88 put: value!

boneCount
    "boneCount field (c_int, offset 96)"
    ^self int32At: 96!

boneCount: value
    self int32At: 96 put: value!

bones
    "bones field ([*c]cimport.struct_BoneInfo, offset 104)"
    ^self uint64At: 104!

bones: value
    self uint64At: 104 put: value!

bindPose
    "bindPose field ([*c]cimport.struct_Transform, offset 112)"
    ^self uint64At: 112!

bindPose: value
    self uint64At: 112 put: value!

!

ExternalStructure subclass: #struct_ModelAnimation
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_ModelAnimation class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^56!

!

!struct_ModelAnimation methodsFor!

boneCount
    "boneCount field (c_int, offset 0)"
    ^self int32At: 0!

boneCount: value
    self int32At: 0 put: value!

frameCount
    "frameCount field (c_int, offset 4)"
    ^self int32At: 4!

frameCount: value
    self int32At: 4 put: value!

bones
    "bones field ([*c]cimport.struct_BoneInfo, offset 8)"
    ^self uint64At: 8!

bones: value
    self uint64At: 8 put: value!

framePoses
    "framePoses field ([*c][*c]cimport.struct_Transform, offset 16)"
    ^self uint64At: 16!

framePoses: value
    self uint64At: 16 put: value!

name
    "name field ([32]u8, offset 24)"
    ^self uint8At: 24!

name: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #ModelAnimation
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ModelAnimation class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^56!

!

!ModelAnimation methodsFor!

boneCount
    "boneCount field (c_int, offset 0)"
    ^self int32At: 0!

boneCount: value
    self int32At: 0 put: value!

frameCount
    "frameCount field (c_int, offset 4)"
    ^self int32At: 4!

frameCount: value
    self int32At: 4 put: value!

bones
    "bones field ([*c]cimport.struct_BoneInfo, offset 8)"
    ^self uint64At: 8!

bones: value
    self uint64At: 8 put: value!

framePoses
    "framePoses field ([*c][*c]cimport.struct_Transform, offset 16)"
    ^self uint64At: 16!

framePoses: value
    self uint64At: 16 put: value!

name
    "name field ([32]u8, offset 24)"
    ^self uint8At: 24!

name: value
    self uint8At: 24 put: value!

!

ExternalStructure subclass: #struct_Ray
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Ray class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_Ray methodsFor!

position
    "position field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

position: value
    self uint8At: 0 put: value!

direction
    "direction field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

direction: value
    self uint8At: 12 put: value!

!

ExternalStructure subclass: #Ray
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Ray class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!Ray methodsFor!

position
    "position field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

position: value
    self uint8At: 0 put: value!

direction
    "direction field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

direction: value
    self uint8At: 12 put: value!

!

ExternalStructure subclass: #struct_RayCollision
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_RayCollision class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^32!

!

!struct_RayCollision methodsFor!

hit
    "hit field (bool, offset 0)"
    ^self uint8At: 0!

hit: value
    self uint8At: 0 put: value!

distance
    "distance field (f32, offset 4)"
    ^self float32At: 4!

distance: value
    self float32At: 4 put: value!

point
    "point field (cimport.struct_Vector3, offset 8)"
    ^self uint8At: 8!

point: value
    self uint8At: 8 put: value!

normal
    "normal field (cimport.struct_Vector3, offset 20)"
    ^self uint8At: 20!

normal: value
    self uint8At: 20 put: value!

!

ExternalStructure subclass: #RayCollision
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!RayCollision class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^32!

!

!RayCollision methodsFor!

hit
    "hit field (bool, offset 0)"
    ^self uint8At: 0!

hit: value
    self uint8At: 0 put: value!

distance
    "distance field (f32, offset 4)"
    ^self float32At: 4!

distance: value
    self float32At: 4 put: value!

point
    "point field (cimport.struct_Vector3, offset 8)"
    ^self uint8At: 8!

point: value
    self uint8At: 8 put: value!

normal
    "normal field (cimport.struct_Vector3, offset 20)"
    ^self uint8At: 20!

normal: value
    self uint8At: 20 put: value!

!

ExternalStructure subclass: #struct_BoundingBox
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_BoundingBox class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_BoundingBox methodsFor!

min
    "min field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

min: value
    self uint8At: 0 put: value!

max
    "max field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

max: value
    self uint8At: 12 put: value!

!

ExternalStructure subclass: #BoundingBox
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!BoundingBox class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!BoundingBox methodsFor!

min
    "min field (cimport.struct_Vector3, offset 0)"
    ^self uint8At: 0!

min: value
    self uint8At: 0 put: value!

max
    "max field (cimport.struct_Vector3, offset 12)"
    ^self uint8At: 12!

max: value
    self uint8At: 12 put: value!

!

ExternalStructure subclass: #struct_Wave
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Wave class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_Wave methodsFor!

frameCount
    "frameCount field (c_uint, offset 0)"
    ^self uint32At: 0!

frameCount: value
    self uint32At: 0 put: value!

sampleRate
    "sampleRate field (c_uint, offset 4)"
    ^self uint32At: 4!

sampleRate: value
    self uint32At: 4 put: value!

sampleSize
    "sampleSize field (c_uint, offset 8)"
    ^self uint32At: 8!

sampleSize: value
    self uint32At: 8 put: value!

channels
    "channels field (c_uint, offset 12)"
    ^self uint32At: 12!

channels: value
    self uint32At: 12 put: value!

data
    "data field (?*anyopaque, offset 16)"
    ^self uint8At: 16!

data: value
    self uint8At: 16 put: value!

!

ExternalStructure subclass: #Wave
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Wave class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!Wave methodsFor!

frameCount
    "frameCount field (c_uint, offset 0)"
    ^self uint32At: 0!

frameCount: value
    self uint32At: 0 put: value!

sampleRate
    "sampleRate field (c_uint, offset 4)"
    ^self uint32At: 4!

sampleRate: value
    self uint32At: 4 put: value!

sampleSize
    "sampleSize field (c_uint, offset 8)"
    ^self uint32At: 8!

sampleSize: value
    self uint32At: 8 put: value!

channels
    "channels field (c_uint, offset 12)"
    ^self uint32At: 12!

channels: value
    self uint32At: 12 put: value!

data
    "data field (?*anyopaque, offset 16)"
    ^self uint8At: 16!

data: value
    self uint8At: 16 put: value!

!

ExternalStructure subclass: #struct_AudioStream
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_AudioStream class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^32!

!

!struct_AudioStream methodsFor!

buffer
    "buffer field (?*cimport.struct_rAudioBuffer, offset 0)"
    ^self uint8At: 0!

buffer: value
    self uint8At: 0 put: value!

processor
    "processor field (?*cimport.struct_rAudioProcessor, offset 8)"
    ^self uint8At: 8!

processor: value
    self uint8At: 8 put: value!

sampleRate
    "sampleRate field (c_uint, offset 16)"
    ^self uint32At: 16!

sampleRate: value
    self uint32At: 16 put: value!

sampleSize
    "sampleSize field (c_uint, offset 20)"
    ^self uint32At: 20!

sampleSize: value
    self uint32At: 20 put: value!

channels
    "channels field (c_uint, offset 24)"
    ^self uint32At: 24!

channels: value
    self uint32At: 24 put: value!

!

ExternalStructure subclass: #AudioStream
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!AudioStream class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^32!

!

!AudioStream methodsFor!

buffer
    "buffer field (?*cimport.struct_rAudioBuffer, offset 0)"
    ^self uint8At: 0!

buffer: value
    self uint8At: 0 put: value!

processor
    "processor field (?*cimport.struct_rAudioProcessor, offset 8)"
    ^self uint8At: 8!

processor: value
    self uint8At: 8 put: value!

sampleRate
    "sampleRate field (c_uint, offset 16)"
    ^self uint32At: 16!

sampleRate: value
    self uint32At: 16 put: value!

sampleSize
    "sampleSize field (c_uint, offset 20)"
    ^self uint32At: 20!

sampleSize: value
    self uint32At: 20 put: value!

channels
    "channels field (c_uint, offset 24)"
    ^self uint32At: 24!

channels: value
    self uint32At: 24 put: value!

!

ExternalStructure subclass: #struct_Sound
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Sound class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!struct_Sound methodsFor!

stream
    "stream field (cimport.struct_AudioStream, offset 0)"
    ^self uint8At: 0!

stream: value
    self uint8At: 0 put: value!

frameCount
    "frameCount field (c_uint, offset 32)"
    ^self uint32At: 32!

frameCount: value
    self uint32At: 32 put: value!

!

ExternalStructure subclass: #Sound
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Sound class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^40!

!

!Sound methodsFor!

stream
    "stream field (cimport.struct_AudioStream, offset 0)"
    ^self uint8At: 0!

stream: value
    self uint8At: 0 put: value!

frameCount
    "frameCount field (c_uint, offset 32)"
    ^self uint32At: 32!

frameCount: value
    self uint32At: 32 put: value!

!

ExternalStructure subclass: #struct_Music
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_Music class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^56!

!

!struct_Music methodsFor!

stream
    "stream field (cimport.struct_AudioStream, offset 0)"
    ^self uint8At: 0!

stream: value
    self uint8At: 0 put: value!

frameCount
    "frameCount field (c_uint, offset 32)"
    ^self uint32At: 32!

frameCount: value
    self uint32At: 32 put: value!

looping
    "looping field (bool, offset 36)"
    ^self uint8At: 36!

looping: value
    self uint8At: 36 put: value!

ctxType
    "ctxType field (c_int, offset 40)"
    ^self int32At: 40!

ctxType: value
    self int32At: 40 put: value!

ctxData
    "ctxData field (?*anyopaque, offset 48)"
    ^self uint8At: 48!

ctxData: value
    self uint8At: 48 put: value!

!

ExternalStructure subclass: #Music
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!Music class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^56!

!

!Music methodsFor!

stream
    "stream field (cimport.struct_AudioStream, offset 0)"
    ^self uint8At: 0!

stream: value
    self uint8At: 0 put: value!

frameCount
    "frameCount field (c_uint, offset 32)"
    ^self uint32At: 32!

frameCount: value
    self uint32At: 32 put: value!

looping
    "looping field (bool, offset 36)"
    ^self uint8At: 36!

looping: value
    self uint8At: 36 put: value!

ctxType
    "ctxType field (c_int, offset 40)"
    ^self int32At: 40!

ctxType: value
    self int32At: 40 put: value!

ctxData
    "ctxData field (?*anyopaque, offset 48)"
    ^self uint8At: 48!

ctxData: value
    self uint8At: 48 put: value!

!

ExternalStructure subclass: #struct_VrDeviceInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_VrDeviceInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^60!

!

!struct_VrDeviceInfo methodsFor!

hResolution
    "hResolution field (c_int, offset 0)"
    ^self int32At: 0!

hResolution: value
    self int32At: 0 put: value!

vResolution
    "vResolution field (c_int, offset 4)"
    ^self int32At: 4!

vResolution: value
    self int32At: 4 put: value!

hScreenSize
    "hScreenSize field (f32, offset 8)"
    ^self float32At: 8!

hScreenSize: value
    self float32At: 8 put: value!

vScreenSize
    "vScreenSize field (f32, offset 12)"
    ^self float32At: 12!

vScreenSize: value
    self float32At: 12 put: value!

eyeToScreenDistance
    "eyeToScreenDistance field (f32, offset 16)"
    ^self float32At: 16!

eyeToScreenDistance: value
    self float32At: 16 put: value!

lensSeparationDistance
    "lensSeparationDistance field (f32, offset 20)"
    ^self float32At: 20!

lensSeparationDistance: value
    self float32At: 20 put: value!

interpupillaryDistance
    "interpupillaryDistance field (f32, offset 24)"
    ^self float32At: 24!

interpupillaryDistance: value
    self float32At: 24 put: value!

lensDistortionValues
    "lensDistortionValues field ([4]f32, offset 28)"
    ^self uint8At: 28!

lensDistortionValues: value
    self uint8At: 28 put: value!

chromaAbCorrection
    "chromaAbCorrection field ([4]f32, offset 44)"
    ^self uint8At: 44!

chromaAbCorrection: value
    self uint8At: 44 put: value!

!

ExternalStructure subclass: #VrDeviceInfo
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!VrDeviceInfo class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^60!

!

!VrDeviceInfo methodsFor!

hResolution
    "hResolution field (c_int, offset 0)"
    ^self int32At: 0!

hResolution: value
    self int32At: 0 put: value!

vResolution
    "vResolution field (c_int, offset 4)"
    ^self int32At: 4!

vResolution: value
    self int32At: 4 put: value!

hScreenSize
    "hScreenSize field (f32, offset 8)"
    ^self float32At: 8!

hScreenSize: value
    self float32At: 8 put: value!

vScreenSize
    "vScreenSize field (f32, offset 12)"
    ^self float32At: 12!

vScreenSize: value
    self float32At: 12 put: value!

eyeToScreenDistance
    "eyeToScreenDistance field (f32, offset 16)"
    ^self float32At: 16!

eyeToScreenDistance: value
    self float32At: 16 put: value!

lensSeparationDistance
    "lensSeparationDistance field (f32, offset 20)"
    ^self float32At: 20!

lensSeparationDistance: value
    self float32At: 20 put: value!

interpupillaryDistance
    "interpupillaryDistance field (f32, offset 24)"
    ^self float32At: 24!

interpupillaryDistance: value
    self float32At: 24 put: value!

lensDistortionValues
    "lensDistortionValues field ([4]f32, offset 28)"
    ^self uint8At: 28!

lensDistortionValues: value
    self uint8At: 28 put: value!

chromaAbCorrection
    "chromaAbCorrection field ([4]f32, offset 44)"
    ^self uint8At: 44!

chromaAbCorrection: value
    self uint8At: 44 put: value!

!

ExternalStructure subclass: #struct_FilePathList
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_FilePathList class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!struct_FilePathList methodsFor!

capacity
    "capacity field (c_uint, offset 0)"
    ^self uint32At: 0!

capacity: value
    self uint32At: 0 put: value!

count
    "count field (c_uint, offset 4)"
    ^self uint32At: 4!

count: value
    self uint32At: 4 put: value!

paths
    "paths field ([*c][*c]u8, offset 8)"
    ^self uint64At: 8!

paths: value
    self uint64At: 8 put: value!

!

ExternalStructure subclass: #FilePathList
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!FilePathList class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!FilePathList methodsFor!

capacity
    "capacity field (c_uint, offset 0)"
    ^self uint32At: 0!

capacity: value
    self uint32At: 0 put: value!

count
    "count field (c_uint, offset 4)"
    ^self uint32At: 4!

count: value
    self uint32At: 4 put: value!

paths
    "paths field ([*c][*c]u8, offset 8)"
    ^self uint64At: 8!

paths: value
    self uint64At: 8 put: value!

!

ExternalStructure subclass: #struct_AutomationEvent
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_AutomationEvent class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!struct_AutomationEvent methodsFor!

frame
    "frame field (c_uint, offset 0)"
    ^self uint32At: 0!

frame: value
    self uint32At: 0 put: value!

type
    "type field (c_uint, offset 4)"
    ^self uint32At: 4!

type: value
    self uint32At: 4 put: value!

params
    "params field ([4]c_int, offset 8)"
    ^self uint8At: 8!

params: value
    self uint8At: 8 put: value!

!

ExternalStructure subclass: #AutomationEvent
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!AutomationEvent class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^24!

!

!AutomationEvent methodsFor!

frame
    "frame field (c_uint, offset 0)"
    ^self uint32At: 0!

frame: value
    self uint32At: 0 put: value!

type
    "type field (c_uint, offset 4)"
    ^self uint32At: 4!

type: value
    self uint32At: 4 put: value!

params
    "params field ([4]c_int, offset 8)"
    ^self uint8At: 8!

params: value
    self uint8At: 8 put: value!

!

ExternalStructure subclass: #struct_AutomationEventList
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!struct_AutomationEventList class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!struct_AutomationEventList methodsFor!

capacity
    "capacity field (c_uint, offset 0)"
    ^self uint32At: 0!

capacity: value
    self uint32At: 0 put: value!

count
    "count field (c_uint, offset 4)"
    ^self uint32At: 4!

count: value
    self uint32At: 4 put: value!

events
    "events field ([*c]cimport.struct_AutomationEvent, offset 8)"
    ^self uint64At: 8!

events: value
    self uint64At: 8 put: value!

!

ExternalStructure subclass: #AutomationEventList
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!AutomationEventList class methodsFor!

byteSize
    "Size of this struct in bytes"
    ^16!

!

!AutomationEventList methodsFor!

capacity
    "capacity field (c_uint, offset 0)"
    ^self uint32At: 0!

capacity: value
    self uint32At: 0 put: value!

count
    "count field (c_uint, offset 4)"
    ^self uint32At: 4!

count: value
    self uint32At: 4 put: value!

events
    "events field ([*c]cimport.struct_AutomationEvent, offset 8)"
    ^self uint64At: 8!

events: value
    self uint64At: 8 put: value!

!

