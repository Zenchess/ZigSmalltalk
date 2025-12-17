"Reinstall primitive doesNotUnderstand: to prevent infinite recursion during bootstrap"

!Object methodsFor: 'exception handling'!

doesNotUnderstand: failedMessage
    "Primitive version - just let the VM handle it"
    <primitive: 800>
    ^self error: 'Message not understood'!

! !
