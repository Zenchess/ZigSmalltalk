"Dynamic class creation primitives"
!Class methodsFor: 'subclass creation'!

subclass: aSymbol
    "Create a new subclass of the receiver with the given name"
    <primitive: 796>
    ^self primitiveFailed!

compile: sourceString
    "Compile and install a method from source code"
    <primitive: 797>
    ^self primitiveFailed!
!

"ByteArray accessor methods for FFI structs"
!ByteArray methodsFor: 'ffi accessing'!

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

"ExternalStructure - Base class for FFI struct wrappers"
Object subclass: #ExternalStructure
    instanceVariableNames: 'bytes'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ExternalStructure class methodsFor: 'instance creation'!

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

!ExternalStructure methodsFor: 'initialization'!

initialize
    "Initialize with a zero-filled byte buffer of the correct size"
    bytes := ByteArray new: self class byteSize.
    ^self!
!

!ExternalStructure methodsFor: 'accessing'!

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

"Delegate accessor methods to bytes"
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

"Float math methods"
!Float methodsFor!

negated
    "Return the receiver negated"
    ^0.0 - self!

sqrt
    "Compute square root using Newton-Raphson method (fixed iterations)"
    | guess |
    self <= 0 ifTrue: [^0.0].
    guess := self / 2.0.
    "20 iterations is enough for double precision"
    1 to: 20 do: [:i | guess := (guess + (self / guess)) / 2.0].
    ^guess!

sin
    "Compute sine using Taylor series (works best for small angles)"
    | x2 result term sign n |
    x2 := self * self.
    result := self.
    term := self.
    sign := -1.
    "10 terms is enough for good precision"
    1 to: 10 do: [:i |
        n := (i * 2) + 1.
        term := term * x2 / (n * (n - 1)).
        result := result + (term * sign).
        sign := sign * -1
    ].
    ^result!

cos
    "Compute cosine using Taylor series (works best for small angles)"
    | x2 result term sign n |
    x2 := self * self.
    result := 1.0.
    term := 1.0.
    sign := -1.
    "10 terms is enough for good precision"
    1 to: 10 do: [:i |
        n := i * 2.
        term := term * x2 / (n * (n - 1)).
        result := result + (term * sign).
        sign := sign * -1
    ].
    ^result!

!

"Number math methods"
!Number methodsFor!

sqrt
    "Convert to float and compute sqrt"
    ^self asFloat sqrt!

!

Object subclass: #LibC
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!LibC class methodsFor!

malloc: size
    <primitive: 702>
    ^self primitiveFailed!

free: pointer
    <primitive: 703>
    ^self primitiveFailed!

memset: pointer value: val size: size
    <primitive: 750>
    ^self primitiveFailed!

memcpy: dest from: src size: size
    <primitive: 751>
    ^self primitiveFailed!

strlen: aString
    <primitive: 710>
    ^self primitiveFailed!

puts: aString
    <primitive: 711>
    ^self primitiveFailed!

readInt8: pointer
    <primitive: 752>
    ^self primitiveFailed!

readInt16: pointer
    <primitive: 753>
    ^self primitiveFailed!

readInt32: pointer
    <primitive: 754>
    ^self primitiveFailed!

readInt64: pointer
    <primitive: 755>
    ^self primitiveFailed!

readFloat64: pointer
    <primitive: 756>
    ^self primitiveFailed!

writeInt8: pointer value: val
    <primitive: 757>
    ^self primitiveFailed!

writeInt32: pointer value: val
    <primitive: 758>
    ^self primitiveFailed!

writeFloat64: pointer value: val
    <primitive: 759>
    ^self primitiveFailed!
!

Object subclass: #LibMath
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!LibMath class methodsFor!

sin: x
    <primitive: 720>
    ^self primitiveFailed!

cos: x
    <primitive: 721>
    ^self primitiveFailed!

tan: x
    <primitive: 730>
    ^self primitiveFailed!

asin: x
    <primitive: 731>
    ^self primitiveFailed!

acos: x
    <primitive: 732>
    ^self primitiveFailed!

atan: x
    <primitive: 733>
    ^self primitiveFailed!

atan2: y x: x
    <primitive: 729>
    ^self primitiveFailed!

sqrt: x
    <primitive: 722>
    ^self primitiveFailed!

pow: x y: y
    <primitive: 723>
    ^self primitiveFailed!

exp: x
    <primitive: 724>
    ^self primitiveFailed!

log: x
    <primitive: 725>
    ^self primitiveFailed!

floor: x
    <primitive: 726>
    ^self primitiveFailed!

ceil: x
    <primitive: 727>
    ^self primitiveFailed!

abs: x
    <primitive: 728>
    ^self primitiveFailed!

pi
    ^3.14159265358979323846!

e
    ^2.71828182845904523536!
!

Object subclass: #ExternalPointer
    instanceVariableNames: 'address'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ExternalPointer class methodsFor!

address: anInteger
    ^self new address: anInteger!

null
    ^self address: 0!

malloc: size
    ^self address: (LibC malloc: size)!
!

!ExternalPointer methodsFor!

address
    ^address!

address: anInteger
    address := anInteger!

isNull
    ^address = 0 or: [address isNil]!

free
    self isNull ifFalse: [
        LibC free: address.
        address := 0
    ]!

readInt8
    ^LibC readInt8: address!

readInt16
    ^LibC readInt16: address!

readInt32
    ^LibC readInt32: address!

readInt64
    ^LibC readInt64: address!

readFloat64
    ^LibC readFloat64: address!

writeInt8: val
    ^LibC writeInt8: address value: val!

writeInt32: val
    ^LibC writeInt32: address value: val!

writeFloat64: val
    ^LibC writeFloat64: address value: val!

+ offset
    ^ExternalPointer address: address + offset!

- offset
    ^ExternalPointer address: address - offset!

at: index
    ^ExternalPointer address: address + (index * 8)!
!

"FFI Library Introspection"

Object subclass: #FFILibrary
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!FFILibrary class methodsFor!

structNamesFor: libraryName
    "Return array of struct names for the given library"
    <primitive: 790>
    ^#()!

structInfo: structName for: libraryName
    "Return struct info: #(size #(fieldName offset size accessorType) ...)"
    <primitive: 791>
    ^nil!

createStructClass: structName for: libraryName
    "Create an ExternalStructure subclass with accessor methods.
     The primitive handles class creation and method compilation.
     Returns the new class object, or nil on failure."
    <primitive: 799>
    ^nil!

generateMethodFor: funcName in: libraryName
    "Generate Smalltalk method source for an FFI function.
     Returns a String with the method source, or nil if not found.
     Example: FFILibrary generateMethodFor: #sin in: 'LibMath'"
    <primitive: 794>
    ^nil!

functionInfo: funcName in: libraryName
    "Return function info: #(argCount returnType arg1Type arg2Type ...)
     Example: FFILibrary functionInfo: #sin in: 'LibMath'
       => #(1 'double' 'double')"
    <primitive: 795>
    ^nil!

installMethodsFor: libraryName on: aClass
    "Generate and install all FFI methods for a library on the given class.
     Example: FFILibrary installMethodsFor: 'LibMath' on: LibMath class"
    | functions |
    functions := libraryName ffiFunctions.
    functions isNil ifTrue: [^self].
    functions do: [:funcName |
        | source |
        source := self generateMethodFor: funcName asSymbol in: libraryName.
        source notNil ifTrue: [
            aClass compile: source
        ]
    ]!

ensureClassFor: libraryName
    "Ensure a class exists for the given library name. Creates one if needed.
     Returns the class (not metaclass).
     Example: FFILibrary ensureClassFor: 'Raylib'"
    | sym cls |
    sym := libraryName asSymbol.
    cls := Smalltalk at: sym ifAbsent: [nil].
    cls notNil ifTrue: [^cls].
    "Create the class"
    Object subclass: sym instanceVariableNames: '' classVariableNames: '' poolDictionaries: ''.
    ^Smalltalk at: sym!

setupLibrary: libraryName
    "Create class for library and install all FFI methods.
     Example: FFILibrary setupLibrary: 'Raylib'"
    | cls |
    cls := self ensureClassFor: libraryName.
    self installMethodsFor: libraryName on: cls class.
    Transcript show: 'Installed FFI methods for ', libraryName; cr.
    ^cls!

setupAllLibraries
    "Setup all available FFI libraries.
     Creates classes and installs methods for each."
    | names |
    names := Object ffiLibraries.
    names do: [:name |
        self setupLibrary: name
    ].
    Transcript show: 'All FFI libraries configured.'; cr!

libraryNames
    "Return array of available FFI library names"
    ^#('LibC' 'LibMath' 'Raylib' 'GLFW' 'GL')!

generateStructsFor: libraryName
    "Generate ExternalStructure subclasses for all structs in a library.
     Example: FFILibrary generateStructsFor: 'Raylib'"
    | structNames count sz structName result |
    structNames := FFILibrary structNamesFor: libraryName.
    (structNames isNil or: [structNames size = 0]) ifTrue: [^0].
    count := 0.
    sz := structNames size.
    "Note: block-local temps don't work in to:do:, use method temps instead"
    1 to: sz do: [:i |
        structName := structNames at: i.
        result := FFILibrary generateStructClass: structName for: libraryName.
        result ifTrue: [count := count + 1]
    ].
    ^count!

generateStructClass: structName for: libraryName
    "Generate a single ExternalStructure subclass.
     Returns true if successful."
    | info structSize cls sz fieldInfo fieldName offset accessorType |
    info := FFILibrary structInfo: structName for: libraryName.
    info isNil ifTrue: [^false].

    "First element is struct size"
    structSize := info at: 1.

    "Create the subclass (structName is already a symbol)"
    cls := ExternalStructure subclass: structName.
    cls isNil ifTrue: [^false].

    "Add byteSize class method"
    cls class compile: 'byteSize ^', structSize printString.

    "Add field accessors (skip first element which is size)"
    sz := info size.
    "Note: block-local temps don't work in to:do:, use method temps instead"
    2 to: sz do: [:i |
        fieldInfo := info at: i.
        fieldName := fieldInfo at: 1.
        offset := fieldInfo at: 2.
        accessorType := fieldInfo at: 4.

        "Generate getter"
        cls compile: fieldName, '
    ^self ', accessorType, 'At: ', offset printString.

        "Generate setter"
        cls compile: fieldName, ': value
    self ', accessorType, 'At: ', offset printString, ' put: value'
    ].

    ^true!

generateAllStructs
    "Generate struct classes for all FFI libraries"
    | total libNames sz libName count |
    total := 0.
    libNames := self libraryNames.
    sz := libNames size.
    "Note: block-local temps don't work in to:do:, use method temps instead"
    1 to: sz do: [:i |
        libName := libNames at: i.
        count := self generateStructsFor: libName.
        total := total + count
    ].
    ^total!
!

"Function Pointer Calling Support"

!Integer methodsFor: 'ffi'!

ffiCallWithSignature: signature args: args
    "Call this integer as a function pointer with the given signature and arguments.
     Signature format: 'returnType(argType1,argType2,...)'
     Types: void, int, uint, int32, uint32, int64, uint64, float, double, pointer, string"
    <primitive: 793>
    ^self primitiveFailed!
!

"Dynamic Library Loading Support"

Object subclass: #ExternalLibrary
    instanceVariableNames: 'handle name'
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

!ExternalLibrary class methodsFor!

load: libraryName
    "Load a dynamic library by name. Returns an ExternalLibrary instance."
    | lib handle |
    lib := self new.
    lib name: libraryName.
    handle := lib primLoad: libraryName.
    (handle isNil or: [handle = 0]) ifTrue: [^nil].
    lib handle: handle.
    ^lib!

named: libraryName
    "Alias for load:"
    ^self load: libraryName!
!

!ExternalLibrary methodsFor!

name
    ^name!

name: aString
    name := aString!

handle
    ^handle!

handle: anInteger
    handle := anInteger!

primLoad: libraryName
    "Load a DLL and return a handle"
    <primitive: 870>
    ^nil!

getProcAddress: funcName
    "Get a function pointer from this library.
     Calls primitive with handle explicitly."
    ^self primGetProcAddress: funcName handle: handle!

primGetProcAddress: funcName handle: aHandle
    "Primitive to get proc address from library handle"
    <primitive: 871>
    ^nil!

free
    "Free the loaded library"
    ^self primFree: handle!

primFree: aHandle
    "Primitive to free a library"
    <primitive: 872>
    ^self!

close
    "Alias for free"
    ^self free!
!

"OpenGL Dynamic Loading Support for Windows"
"Using Smalltalk globals instead of class variables"

Object subclass: #GL
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''
    classInstanceVariableNames: ''!

"Initialize GL globals"
Smalltalk at: #GL_OpenGL32 put: nil.
Smalltalk at: #GL_WglGetProcAddress put: nil.
Smalltalk at: #GL_FunctionCache put: nil.
Smalltalk at: #GL_Initialized put: false.

!GL class methodsFor!

initialize
    "Initialize the GL dynamic loader. Call this after creating an OpenGL context."
    Smalltalk at: #GL_Initialized put: false.
    Smalltalk at: #GL_FunctionCache put: Dictionary new.

    "Load opengl32.dll"
    Smalltalk at: #GL_OpenGL32 put: (ExternalLibrary load: 'opengl32.dll').
    GL_OpenGL32 isNil ifTrue: [
        Transcript show: 'Failed to load opengl32.dll'; cr.
        ^false
    ].

    "Get wglGetProcAddress"
    Smalltalk at: #GL_WglGetProcAddress put: (GL_OpenGL32 getProcAddress: 'wglGetProcAddress').
    GL_WglGetProcAddress isNil ifTrue: [
        Transcript show: 'Failed to get wglGetProcAddress'; cr.
        ^false
    ].
    GL_WglGetProcAddress = 0 ifTrue: [
        Transcript show: 'wglGetProcAddress returned null'; cr.
        ^false
    ].

    Smalltalk at: #GL_Initialized put: true.
    Transcript show: 'GL dynamic loader initialized'; cr.
    ^true!

isInitialized
    ^GL_Initialized == true!

ensureInitialized
    self isInitialized ifFalse: [self initialize]!

getProcAddress: funcName
    "Get an OpenGL function pointer by name using wglGetProcAddress"
    | ptr cachedPtr |
    self ensureInitialized.

    "Check cache first"
    GL_FunctionCache notNil ifTrue: [
        cachedPtr := GL_FunctionCache at: funcName ifAbsent: [nil].
        cachedPtr notNil ifTrue: [^cachedPtr]
    ].

    "Try wglGetProcAddress first (for GL 1.2+ functions)"
    ptr := self primCallWglGetProcAddress: funcName.
    (ptr notNil and: [ptr > 3]) ifTrue: [
        GL_FunctionCache at: funcName put: ptr.
        ^ptr
    ].

    "Fallback to direct lookup in opengl32.dll (for GL 1.1 functions)"
    ptr := GL_OpenGL32 getProcAddress: funcName.
    ptr notNil ifTrue: [
        GL_FunctionCache at: funcName put: ptr
    ].
    ^ptr!

primCallWglGetProcAddress: funcName
    "Call wglGetProcAddress with the given function name.
     wglGetProcAddress signature: PROC wglGetProcAddress(LPCSTR)"
    ^GL_WglGetProcAddress ffiCallWithSignature: 'pointer(string)' args: { funcName }!

"GL 1.1 Functions (exported from opengl32.dll) - using dynamic loading"

glViewport: x y: y width: w height: h
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glViewport'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,int32,int32,int32)' args: { x. y. w. h }!

glEnable: cap
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glEnable'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { cap }!

glDisable: cap
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glDisable'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { cap }!

glClear: mask
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glClear'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { mask }!

glClearColor: r g: g b: b a: a
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glClearColor'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(float,float,float,float)' args: { r. g. b. a }!

glDepthMask: flag
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glDepthMask'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32)' args: { flag }!

glBindTexture: target texture: texture
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glBindTexture'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32)' args: { target. texture }!

glGenTextures: n textures: textures
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glGenTextures'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { n. textures }!

glDeleteTextures: n textures: textures
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glDeleteTextures'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { n. textures }!

glTexImage2D: target level: level internalformat: internalformat width: width height: height border: border format: format type: type pixels: pixels
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glTexImage2D'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,int32,int32,int32,int32,uint32,uint32,pointer)' args: { target. level. internalformat. width. height. border. format. type. pixels }!

glTexParameteri: target pname: pname param: param
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glTexParameteri'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32,int32)' args: { target. pname. param }!

glDrawElements: mode count: count type: type indices: indices
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glDrawElements'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,uint32,pointer)' args: { mode. count. type. indices }!

glDrawArrays: mode first: first count: count
    | funcPtr |
    self ensureInitialized.
    funcPtr := GL_OpenGL32 getProcAddress: 'glDrawArrays'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,int32)' args: { mode. first. count }!

"GL 2.0+ Functions (loaded via wglGetProcAddress) - Shader functions"

createShader: shaderType
    | funcPtr |
    funcPtr := self getProcAddress: 'glCreateShader'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'uint32(uint32)' args: { shaderType }!

shaderSource: shader source: source
    "glShaderSource(shader, 1, &source, NULL)"
    | funcPtr sourceArray |
    funcPtr := self getProcAddress: 'glShaderSource'.
    funcPtr isNil ifTrue: [^nil].
    sourceArray := { source }.
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,string_array,pointer)' args: { shader. 1. sourceArray. nil }!

compileShader: shader
    | funcPtr |
    funcPtr := self getProcAddress: 'glCompileShader'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { shader }!

deleteShader: shader
    | funcPtr |
    funcPtr := self getProcAddress: 'glDeleteShader'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { shader }!

createProgram
    | funcPtr |
    funcPtr := self getProcAddress: 'glCreateProgram'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'uint32()' args: #()!

attachShader: program shader: shader
    | funcPtr |
    funcPtr := self getProcAddress: 'glAttachShader'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32)' args: { program. shader }!

linkProgram: program
    | funcPtr |
    funcPtr := self getProcAddress: 'glLinkProgram'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { program }!

detachShader: program shader: shader
    | funcPtr |
    funcPtr := self getProcAddress: 'glDetachShader'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32)' args: { program. shader }!

useProgram: program
    | funcPtr |
    funcPtr := self getProcAddress: 'glUseProgram'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { program }!

deleteProgram: program
    | funcPtr |
    funcPtr := self getProcAddress: 'glDeleteProgram'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { program }!

getUniformLocation: program name: name
    | funcPtr |
    funcPtr := self getProcAddress: 'glGetUniformLocation'.
    funcPtr isNil ifTrue: [^-1].
    ^funcPtr ffiCallWithSignature: 'int32(uint32,string)' args: { program. name }!

"Uniform functions"

uniform1i: location value: v0
    | funcPtr |
    funcPtr := self getProcAddress: 'glUniform1i'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,int32)' args: { location. v0 }!

uniform3f: location x: x y: y z: z
    | funcPtr |
    funcPtr := self getProcAddress: 'glUniform3f'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,float,float,float)' args: { location. x. y. z }!

uniformMatrix4fv: location count: count transpose: transpose value: value
    | funcPtr |
    funcPtr := self getProcAddress: 'glUniformMatrix4fv'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,int32,int32,pointer)' args: { location. count. transpose. value }!

"Buffer and VAO functions"

genBuffer
    | funcPtr arr |
    funcPtr := self getProcAddress: 'glGenBuffers'.
    funcPtr isNil ifTrue: [^nil].
    arr := ByteArray new: 4.
    funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { 1. arr }.
    ^arr uint32At: 0!

deleteBuffer: buffer
    | funcPtr arr |
    funcPtr := self getProcAddress: 'glDeleteBuffers'.
    funcPtr isNil ifTrue: [^nil].
    arr := ByteArray new: 4.
    arr uint32At: 0 put: buffer.
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { 1. arr }!

bindBuffer: target buffer: buffer
    | funcPtr |
    funcPtr := self getProcAddress: 'glBindBuffer'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32)' args: { target. buffer }!

bufferData: target size: size data: data usage: usage
    | funcPtr |
    funcPtr := self getProcAddress: 'glBufferData'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int64,pointer,uint32)' args: { target. size. data. usage }!

genVertexArray
    | funcPtr arr |
    funcPtr := self getProcAddress: 'glGenVertexArrays'.
    funcPtr isNil ifTrue: [^nil].
    arr := ByteArray new: 4.
    funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { 1. arr }.
    ^arr uint32At: 0!

deleteVertexArray: vao
    | funcPtr arr |
    funcPtr := self getProcAddress: 'glDeleteVertexArrays'.
    funcPtr isNil ifTrue: [^nil].
    arr := ByteArray new: 4.
    arr uint32At: 0 put: vao.
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { 1. arr }!

bindVertexArray: vao
    | funcPtr |
    funcPtr := self getProcAddress: 'glBindVertexArray'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { vao }!

vertexAttribPointer: index size: size type: type normalized: normalized stride: stride pointer: pointer
    | funcPtr |
    funcPtr := self getProcAddress: 'glVertexAttribPointer'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,uint32,int32,int32,pointer)' args: { index. size. type. normalized. stride. pointer }!

enableVertexAttribArray: index
    | funcPtr |
    funcPtr := self getProcAddress: 'glEnableVertexAttribArray'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { index }!

"Texture functions"

activeTexture: texture
    | funcPtr |
    funcPtr := self getProcAddress: 'glActiveTexture'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { texture }!

generateMipmap: target
    | funcPtr |
    funcPtr := self getProcAddress: 'glGenerateMipmap'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { target }!

"Framebuffer functions"

genFramebuffers: n framebuffers: framebuffers
    | funcPtr |
    funcPtr := self getProcAddress: 'glGenFramebuffers'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { n. framebuffers }!

deleteFramebuffer: fbo
    | funcPtr arr |
    funcPtr := self getProcAddress: 'glDeleteFramebuffers'.
    funcPtr isNil ifTrue: [^nil].
    arr := ByteArray new: 4.
    arr uint32At: 0 put: fbo.
    ^funcPtr ffiCallWithSignature: 'void(int32,pointer)' args: { 1. arr }!

bindFramebuffer: target framebuffer: framebuffer
    | funcPtr |
    funcPtr := self getProcAddress: 'glBindFramebuffer'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32)' args: { target. framebuffer }!

framebufferTexture2D: target attachment: attachment textarget: textarget texture: texture level: level
    | funcPtr |
    funcPtr := self getProcAddress: 'glFramebufferTexture2D'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,uint32,uint32,uint32,int32)' args: { target. attachment. textarget. texture. level }!

drawBuffer: mode
    | funcPtr |
    funcPtr := self getProcAddress: 'glDrawBuffer'.
    (funcPtr isNil or: [funcPtr = 0]) ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { mode }!

readBuffer: mode
    | funcPtr |
    funcPtr := self getProcAddress: 'glReadBuffer'.
    (funcPtr isNil or: [funcPtr = 0]) ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32)' args: { mode }!

"Draw functions"

drawElements: mode count: count type: type indices: indices
    | funcPtr |
    funcPtr := self getProcAddress: 'glDrawElements'.
    funcPtr isNil ifTrue: [^nil].
    ^funcPtr ffiCallWithSignature: 'void(uint32,int32,uint32,pointer)' args: { mode. count. type. indices }!
!
