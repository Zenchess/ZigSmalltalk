"Dynamic class and method creation primitives"

"Add primitiveFailed to Object for ByteArray primitive methods"
!Object methodsFor!

primitiveFailed
    "Handle primitive failure - default implementation"
    ^nil!

!

"SmallInteger printString - needed for FFI struct generation"
!SmallInteger methodsFor!

printString
    "Answer a string representation of the receiver using VM primitive."
    <primitive: 44>
    ^self primitiveFailed!

!

!Class methodsFor!

subclass: aSymbol
    "Create a new subclass of the receiver with the given name"
    <primitive: 796>
    ^self primitiveFailed!

compile: sourceString
    "Compile and install a method from source code"
    <primitive: 797>
    ^self primitiveFailed!

!

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

uint64At: offset
    "Read unsigned 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 778>
    ^self primitiveFailed!

int64At: offset
    "Read signed 64-bit value at 0-based byte offset (little-endian)"
    <primitive: 779>
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

"FFI Library Introspection and Dynamic Struct Generation"

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

generateStructsFor: libraryName
    "Dynamically generate ExternalStructure subclasses for all structs in the library"
    | structNames |
    structNames := self structNamesFor: libraryName.
    structNames do: [:name |
        self createStructClass: name inLibrary: libraryName
    ].
    ^structNames size!

createStructClass: structName inLibrary: libraryName
    | info size newClass base |
    info := self structInfo: structName for: libraryName.
    info isNil ifTrue: [^nil].
    size := info at: 1.
    base := ExternalStructure.
    newClass := base subclass: structName.
    newClass class compile: ('byteSize ^' , size printString).
    self addAccessorsFor: info to: newClass.
    ^newClass!

addAccessorsFor: info to: newClass
    "Add getter and setter methods for each field"
    | idx fieldInfo fieldName offset accessorType |
    idx := 2.
    [idx <= info size] whileTrue: [
        fieldInfo := info at: idx.
        fieldName := fieldInfo at: 1.
        offset := fieldInfo at: 2.
        accessorType := fieldInfo at: 4.
        newClass compile: (fieldName , ' ^self ' , accessorType , 'At: ' , offset printString).
        newClass compile: (fieldName , ': value self ' , accessorType , 'At: ' , offset printString , ' put: value').
        idx := idx + 1.
    ]!

loadLibrary: libraryName
    "Load a library and generate all its struct classes"
    ^self generateStructsFor: libraryName!

!
