"FFI Auto-Generated Bindings Support"
"Add FFI methods to String and Object"

!String methodsFor!

ffiCall: functionName with: args
    "Call an FFI function by name with arguments.
     The receiver is the library name (e.g., 'LibMath', 'LibC').
     functionName is a Symbol naming the C function.
     args is an Array of arguments.

     Examples:
       'LibMath' ffiCall: #sin with: { 1.0 }
       'LibMath' ffiCall: #pow with: { 2.0. 10.0 }
       'LibMath' ffiCall: #sqrt with: { 2.0 }
       'LibC' ffiCall: #abs with: { -42 }
    "
    <primitive: 700>
    ^self primitiveFailed!

ffiFunctions
    "Return an Array of function names available in this FFI library.
     Receiver should be a library name like 'LibMath' or 'LibC'.

     Example: 'LibMath' ffiFunctions  => #('sin' 'cos' 'sqrt' ...)"
    <primitive: 761>
    ^#()!

!

!Object class methodsFor!

ffiLibraries
    "Return an Array of available FFI library names.

     Example: Object ffiLibraries  => #('LibMath' 'LibC')"
    <primitive: 760>
    ^#()!

!
