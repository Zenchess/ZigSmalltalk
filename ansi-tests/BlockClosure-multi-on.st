"Add multi-exception handler support to BlockClosure using nested on:do: calls"

!BlockClosure methodsFor: 'exception handling'!

on: selector1 do: action1 on: selector2 do: action2
    "Handle two exception types using nested handlers"
    ^[self on: selector1 do: action1] on: selector2 do: action2!

on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3
    "Handle three exception types using nested handlers"
    ^[[self on: selector1 do: action1] on: selector2 do: action2] on: selector3 do: action3!

on: selector1 do: action1 on: selector2 do: action2 on: selector3 do: action3 on: selector4 do: action4
    "Handle four exception types using nested handlers"
    ^[[[self on: selector1 do: action1] on: selector2 do: action2] on: selector3 do: action3] on: selector4 do: action4!

! !
