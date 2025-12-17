!SmallInteger methodsFor!

fibonacci
    self <= 2 ifTrue: [^1].
    ^(self - 1) fibonacci + (self - 2) fibonacci!

sieveBenchmark
    | size flags prime k count |
    size := 8190.
    1 to: self do: [:iter |
        count := 0.
        flags := Array new: size.
        1 to: size do: [:i | flags at: i put: true].
        1 to: size do: [:i |
            (flags at: i) ifTrue: [
                prime := i + 1.
                k := i + prime.
                [k <= size] whileTrue: [
                    flags at: k put: false.
                    k := k + prime].
                count := count + 1]]].
    ^count!

!
