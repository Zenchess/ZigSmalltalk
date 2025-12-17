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
    functions ifNil: [^self].
    functions do: [:funcName |
        | source |
        source := self generateMethodFor: funcName asSymbol in: libraryName.
        source ifNotNil: [
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
    cls ifNotNil: [^cls].
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
!
