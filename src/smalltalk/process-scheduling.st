"Process Scheduling Classes for Zig Smalltalk
 Provides full process scheduling functionality using Dolphin-compatible primitives
 but tailored to our Zig VM architecture.

 Classes defined:
   - Link: Base class for linked list elements
   - LinkedList: Collection of Links
   - Semaphore: Process synchronization primitive
   - Process: Individual thread of execution
   - ProcessorScheduler: The singleton scheduler (Processor global)
   - Delay: Timed process suspension

 Primitive numbers (Dolphin-compatible):
   85 - Semaphore>>signal
   86 - Semaphore>>wait
   87 - Process>>resume
   88 - Process>>suspend
   91 - Process>>primTerminate
   92 - Process>>priority:
   95 - ProcessorScheduler>>enableAsyncEvents:
   98 - Process>>queueInterrupt:with:
   99 - Semaphore>>primSetSignals:
   100 - Delay class>>signalTimerAfter:
   156 - ProcessorScheduler>>yield
   174 - Delay class>>millisecondClockValue
   189 - Delay class>>microsecondClockValue
"

"============================================================================"
"Link - Base class for linked list elements"
"============================================================================"

Object subclass: #Link
    instanceVariableNames: 'nextLink'
    classVariableNames: ''
    poolDictionaries: ''!

!Link methodsFor: 'accessing'!

nextLink
    "Answer the next Link in the chain"
    ^nextLink!

nextLink: aLink
    "Set the next Link in the chain"
    nextLink := aLink.
    ^aLink!

! !

!Link class methodsFor: 'instance creation'!

nextLink: aLink
    "Answer a new Link pointing to aLink"
    ^self new nextLink: aLink!

! !

"============================================================================"
"LinkedList - A collection of Links"
"============================================================================"

SequenceableCollection subclass: #LinkedList
    instanceVariableNames: 'firstLink lastLink'
    classVariableNames: ''
    poolDictionaries: ''!

!LinkedList methodsFor: 'accessing'!

first
    "Answer the first element"
    self emptyCheck.
    ^firstLink!

last
    "Answer the last element"
    self emptyCheck.
    ^lastLink!

size
    "Answer the number of elements"
    | count link |
    count := 0.
    link := firstLink.
    [link notNil] whileTrue: [
        count := count + 1.
        link := link nextLink].
    ^count!

! !

!LinkedList methodsFor: 'testing'!

isEmpty
    "Answer whether the receiver is empty"
    ^firstLink isNil!

includes: anObject
    "Answer whether anObject is in the list"
    | link |
    link := firstLink.
    [link notNil] whileTrue: [
        link == anObject ifTrue: [^true].
        link := link nextLink].
    ^false!

! !

!LinkedList methodsFor: 'adding'!

add: aLink
    "Add aLink to the end of the list. Answer aLink."
    ^self addLast: aLink!

addFirst: aLink
    "Add aLink at the beginning of the list"
    aLink nextLink: firstLink.
    firstLink := aLink.
    lastLink ifNil: [lastLink := aLink].
    ^aLink!

addLast: aLink
    "Add aLink at the end of the list"
    lastLink
        ifNil: [firstLink := aLink]
        ifNotNil: [lastLink nextLink: aLink].
    lastLink := aLink.
    aLink nextLink: nil.
    ^aLink!

! !

!LinkedList methodsFor: 'removing'!

remove: aLink ifAbsent: aBlock
    "Remove aLink from the list. If not found, evaluate aBlock."
    | prevLink currentLink |
    currentLink := firstLink.
    prevLink := nil.
    [currentLink notNil] whileTrue: [
        currentLink == aLink ifTrue: [
            prevLink
                ifNil: [firstLink := currentLink nextLink]
                ifNotNil: [prevLink nextLink: currentLink nextLink].
            currentLink == lastLink ifTrue: [lastLink := prevLink].
            aLink nextLink: nil.
            ^aLink].
        prevLink := currentLink.
        currentLink := currentLink nextLink].
    ^aBlock value!

removeFirst
    "Remove and answer the first element"
    | first |
    self emptyCheck.
    first := firstLink.
    firstLink := first nextLink.
    firstLink ifNil: [lastLink := nil].
    first nextLink: nil.
    ^first!

removeLast
    "Remove and answer the last element"
    | last prevLink currentLink |
    self emptyCheck.
    firstLink == lastLink ifTrue: [
        last := firstLink.
        firstLink := nil.
        lastLink := nil.
        ^last].
    currentLink := firstLink.
    [currentLink nextLink == lastLink] whileFalse: [
        currentLink := currentLink nextLink].
    last := lastLink.
    lastLink := currentLink.
    currentLink nextLink: nil.
    ^last!

! !

!LinkedList methodsFor: 'enumerating'!

do: aBlock
    "Evaluate aBlock for each element"
    | link |
    link := firstLink.
    [link notNil] whileTrue: [
        aBlock value: link.
        link := link nextLink]!

! !

!LinkedList methodsFor: 'private'!

emptyCheck
    "Raise an error if empty"
    self isEmpty ifTrue: [self error: 'LinkedList is empty']!

! !

!LinkedList class methodsFor: 'instance creation'!

new
    ^super new initialize!

! !

!LinkedList methodsFor: 'initialization'!

initialize
    firstLink := nil.
    lastLink := nil!

! !

"============================================================================"
"Semaphore - Process synchronization primitive"
"============================================================================"

LinkedList subclass: #Semaphore
    instanceVariableNames: 'signals'
    classVariableNames: ''
    poolDictionaries: ''!

!Semaphore methodsFor: 'initialization'!

initialize
    "Initialize with zero excess signals"
    super initialize.
    signals := 0!

! !

!Semaphore methodsFor: 'accessing'!

excessSignals
    "Answer the number of excess signals (signals sent when no process was waiting)"
    ^signals!

excessSignals: anInteger
    "Set the excess signal count"
    signals := anInteger!

! !

!Semaphore methodsFor: 'signaling'!

signal
    "Send a signal through the receiver. If processes are waiting, wake the first one.
     Otherwise increment the excess signal count."
    <primitive: 85>
    self primitiveFailed!

wait
    "Wait for a signal. If excess signals exist, decrement and continue.
     Otherwise suspend the current process until signaled."
    | ret |
    ret := Array new: 1.
    self wait: -1 ret: ret.  "-1 = INFINITE"
    ^ret at: 1!

wait: timeout
    "Wait for a signal with timeout (in milliseconds).
     Answer 0 (WAIT_OBJECT_0) on success, 258 (WAIT_TIMEOUT) on timeout."
    | ret |
    ret := Array new: 1.
    self wait: timeout ret: ret.
    ^ret at: 1!

wait: timeout ret: returnValueHolder
    "Primitive wait with timeout and return value holder.
     timeout: -1 = infinite, 0 = poll, >0 = milliseconds
     Result is stored in first slot of returnValueHolder."
    <primitive: 86>
    self primitiveFailed!

! !

!Semaphore methodsFor: 'signaling-extended'!

primSetSignals: anInteger
    "Set the signal count, waking waiting processes if appropriate"
    <primitive: 99>
    self primitiveFailed!

set
    "Set excess signals to 1 (useful for event-style signaling)"
    self primSetSignals: 1!

pulse
    "Signal waiting processes but leave excess signals at 0"
    self primSetSignals: 0!

reset
    "Reset excess signals to zero without affecting waiting processes"
    signals := 0!

! !

!Semaphore methodsFor: 'mutual exclusion'!

critical: aBlock
    "Evaluate aBlock as a critical section protected by the receiver"
    | result |
    self wait.
    ^[aBlock value] ensure: [self signal]!

critical: aBlock ifLocked: lockedBlock
    "Try to enter critical section. If locked, evaluate lockedBlock instead."
    signals > 0
        ifTrue: [
            signals := signals - 1.
            ^[aBlock value] ensure: [self signal]]
        ifFalse: [^lockedBlock value]!

! !

!Semaphore methodsFor: 'process control'!

terminateProcess
    "Terminate the first process waiting on the receiver"
    self isEmpty ifFalse: [self first terminate]!

! !

!Semaphore methodsFor: 'printing'!

printOn: aStream
    "Print a representation of the receiver"
    aStream nextPutAll: 'a Semaphore('.
    signals > 0
        ifTrue: [aStream print: signals; nextPutAll: ' signals']
        ifFalse: [
            self isEmpty
                ifTrue: [aStream nextPutAll: 'ready']
                ifFalse: [aStream print: self size; nextPutAll: ' waiting']].
    aStream nextPut: $)!

! !

!Semaphore class methodsFor: 'instance creation'!

new
    "Answer a new Semaphore with no excess signals"
    ^super new initialize!

forMutualExclusion
    "Answer a new Semaphore suitable for mutual exclusion (one excess signal)"
    ^self new signal; yourself!

! !

"============================================================================"
"Process - An individual thread of execution"
"============================================================================"

Link subclass: #Process
    instanceVariableNames: 'suspendedFrame priority myList exceptionEnvironment name processId'
    classVariableNames: 'NextProcessId'
    poolDictionaries: ''!

"Initialize NextProcessId - must happen before any Process creation"
NextProcessId := 1!

!Process class methodsFor: 'class initialization'!

initialize
    "Initialize class variables"
    NextProcessId := 1!

! !

!Process methodsFor: 'accessing'!

priority
    "Answer the receiver's priority (1=lowest, 10=highest)"
    ^priority!

priority: anInteger
    "Set the receiver's priority. If this changes scheduling, a process switch may occur.
     Answer the previous priority."
    <primitive: 92>
    self primitiveFailed!

name
    "Answer the name of this process"
    ^name ifNil: ['unnamed']!

name: aStringOrSymbol
    "Set the name of this process"
    name := aStringOrSymbol!

id
    "Answer a unique identifier for this process"
    ^processId!

suspendingList
    "Answer the list (Semaphore or ready queue) on which the receiver is waiting, or nil"
    ^myList!

exceptionEnvironment
    "Answer the exception handler chain for this process"
    ^exceptionEnvironment!

exceptionEnvironment: anExceptionHandler
    "Set the exception handler chain"
    exceptionEnvironment := anExceptionHandler!

! !

!Process methodsFor: 'state changing'!

resume
    "Resume a suspended process, adding it to the ready queue"
    <primitive: 87>
    self primitiveFailed!

suspend
    "Suspend the receiver (remove from ready queue or semaphore wait list)"
    <primitive: 88>
    self primitiveFailed!

terminate
    "Terminate the receiver. Run unwind blocks if possible."
    self isDead ifTrue: [^self].
    self isActive
        ifTrue: [self shutdown]
        ifFalse: [self queueInterrupt: 1 "terminate interrupt"]!

primTerminate
    "Primitive to terminate the receiver immediately"
    <primitive: 91>
    self primitiveFailed!

kill
    "Terminate immediately without running unwind blocks"
    self isDead ifTrue: [^self].
    self isActive
        ifTrue: [self shutdown]
        ifFalse: [self queueInterrupt: 9 "kill interrupt"]!

shutdown
    "Final termination - called when process ends"
    [self primTerminate] whileFalse!

yield
    "Yield the processor to other processes at same or higher priority"
    Processor yield!

! !

!Process methodsFor: 'interrupts'!

interruptWith: aBlock
    "Interrupt the receiver to evaluate aBlock, then resume normal execution"
    ^self queueInterrupt: 7 with: aBlock!

queueInterrupt: anInteger
    "Queue an interrupt with no argument"
    ^self queueInterrupt: anInteger with: nil!

queueInterrupt: anInteger with: anObject
    "Queue an interrupt for the receiver"
    <primitive: 98>
    self primitiveFailed!

! !

!Process methodsFor: 'testing'!

isActive
    "Answer whether the receiver is the currently running process"
    ^Processor activeProcess == self!

isDead
    "Answer whether the receiver has terminated"
    ^suspendedFrame isNil and: [myList isNil]!

isReady
    "Answer whether the receiver is ready to run (in a ready queue)"
    ^self state == #ready!

isSuspended
    "Answer whether the receiver is suspended (not in any queue)"
    ^self state == #suspended!

isWaiting
    "Answer whether the receiver is waiting on a Semaphore"
    ^self state == #waiting!

isTerminated
    "Answer whether the receiver has terminated"
    ^self isDead!

state
    "Answer a symbol describing the receiver's current state"
    self isActive ifTrue: [^#running].
    myList isNil ifTrue: [
        suspendedFrame isNil
            ifTrue: [^#dead]
            ifFalse: [^#suspended]].
    (myList isKindOf: Semaphore)
        ifTrue: [^#waiting]
        ifFalse: [^#ready]!

! !

!Process methodsFor: 'printing'!

printOn: aStream
    "Print a description of the receiver"
    aStream
        nextPutAll: 'a Process(';
        nextPutAll: self name;
        nextPutAll: ', id: ';
        print: self id;
        nextPutAll: ', priority: ';
        print: self priority;
        nextPutAll: ', state: ';
        nextPutAll: self state;
        nextPut: $)!

! !

!Process methodsFor: 'initialization'!

initialize
    "Initialize a new process"
    priority := 5.  "Default to user scheduling priority"
    processId := NextProcessId.
    NextProcessId := NextProcessId + 1!

setLaunchBlock: aBlock
    "Initialize the receiver to execute aBlock when resumed.
     Store the block in suspendedFrame - the VM will detect this and
     set up execution to evaluate the block when the process first runs."
    suspendedFrame := aBlock.
    name := 'Process'!

! !

!Process class methodsFor: 'instance creation'!

forContext: aBlock priority: anInteger
    "Create a new process to evaluate aBlock at the given priority"
    | process |
    process := self basicNew initialize.
    process setLaunchBlock: aBlock.
    process priority: anInteger.
    ^process!

! !

"============================================================================"
"ProcessorScheduler - The singleton process scheduler"
"============================================================================"

Object subclass: #ProcessorScheduler
    instanceVariableNames: 'processLists activeProcess pendingTerminations'
    classVariableNames: ''
    poolDictionaries: ''!

!ProcessorScheduler methodsFor: 'accessing'!

activeProcess
    "Answer the currently running process"
    ^activeProcess!

activePriority
    "Answer the priority of the currently running process"
    ^self activeProcess priority!

highestPriority
    "Answer the highest priority level"
    ^10!

! !

!ProcessorScheduler methodsFor: 'priority names'!

timingPriority
    "Priority for timing-critical processes (highest)"
    ^10!

highIOPriority
    "Priority for high-priority I/O"
    ^9!

lowIOPriority
    "Priority for normal I/O"
    ^8!

userInterruptPriority
    "Priority for user interrupt handling"
    ^7!

userSchedulingPriority
    "Priority for normal user processes"
    ^5!

userBackgroundPriority
    "Priority for background user processes"
    ^3!

systemBackgroundPriority
    "Priority for system background processes"
    ^2!

systemBasePriority
    "Lowest priority"
    ^1!

! !

!ProcessorScheduler methodsFor: 'process control'!

yield
    "Yield the processor, allowing other ready processes to run"
    <primitive: 156>
    "If primitive fails, sleep briefly"
    self sleep: 0!

suspendActive
    "Suspend the currently active process"
    activeProcess suspend!

terminateActive
    "Terminate the currently active process"
    activeProcess terminate!

sleep: milliseconds
    "Sleep for the specified number of milliseconds"
    (Delay forMilliseconds: milliseconds) wait!

! !

!ProcessorScheduler methodsFor: 'interrupt control'!

enableAsyncEvents: aBoolean
    "Enable or disable asynchronous events (process switching on signals)"
    <primitive: 95>
    self primitiveFailed!

enableInterrupts
    "Re-enable interrupts and yield if necessary"
    (self enableAsyncEvents: true) ifFalse: [self yield]!

disableInterrupts
    "Disable asynchronous interrupts"
    ^self enableAsyncEvents: false!

! !

!ProcessorScheduler methodsFor: 'interrupt numbers'!

terminateInterrupt
    "Interrupt number for process termination"
    ^1!

genericInterrupt
    "Interrupt number for generic block evaluation"
    ^7!

killInterrupt
    "Interrupt number for immediate process kill"
    ^9!

! !

!ProcessorScheduler methodsFor: 'printing'!

printOn: aStream
    aStream nextPutAll: 'Processor'!

! !

!ProcessorScheduler class methodsFor: 'instance creation'!

new
    "There should only be one ProcessorScheduler"
    self error: 'Use the global Processor instead'!

basicNew
    "Allow creation for bootstrap"
    ^super new initialize!

! !

!ProcessorScheduler methodsFor: 'initialization'!

initialize
    processLists := Array new: 10.
    1 to: 10 do: [:i | processLists at: i put: LinkedList new].
    pendingTerminations := Semaphore new!

! !

"Create the global Processor instance"
Smalltalk at: #Processor put: ProcessorScheduler basicNew!

"============================================================================"
"Delay - Timed process suspension"
"============================================================================"

Object subclass: #Delay
    instanceVariableNames: 'duration resumptionTime waitSemaphore'
    classVariableNames: 'AccessProtect Current Pending TimingSemaphore TimingProcess'
    poolDictionaries: ''!

Delay addClassVariable: 'AccessProtect'!
Delay addClassVariable: 'Current'!
Delay addClassVariable: 'Pending'!
Delay addClassVariable: 'TimingSemaphore'!
Delay addClassVariable: 'TimingProcess'!

!Delay methodsFor: 'initialization'!

initialize
    duration := 0.
    resumptionTime := nil.
    waitSemaphore := Semaphore new!

duration: microseconds
    "Set the delay duration in microseconds"
    duration := microseconds!

resumptionTime: microseconds
    "Set absolute resumption time in microseconds (for untilMilliseconds:)"
    resumptionTime := microseconds.
    duration := nil  "Mark as absolute delay"!

! !

!Delay methodsFor: 'accessing'!

duration
    "Answer the delay duration"
    ^duration!

resumptionTime
    "Answer the absolute resumption time"
    ^resumptionTime!

! !

!Delay methodsFor: 'comparing'!

<= aDelay
    "Answer whether the receiver fires before or at the same time as aDelay"
    ^self resumptionTime <= aDelay resumptionTime!

! !

!Delay methodsFor: 'delaying'!

wait
    "Delay the current process for the receiver's duration.
     Simple busy-wait implementation."
    | targetTime |
    targetTime := Delay microsecondClockValue + duration.
    [Delay microsecondClockValue < targetTime] whileTrue: []!

! !

!Delay methodsFor: 'signaling'!

signal
    "Immediately resume any process waiting on the receiver"
    waitSemaphore signal!

cancel
    "Cancel the delay (waiting process remains waiting)"
    "For now, just clear the resumption time"
    resumptionTime := nil!

! !

!Delay methodsFor: 'printing'!

printOn: aStream
    aStream nextPutAll: 'a Delay('.
    duration
        ifNotNil: [aStream print: duration; nextPutAll: ' us']
        ifNil: [aStream nextPutAll: 'until '; print: resumptionTime].
    aStream nextPut: $)!

! !

!Delay class methodsFor: 'instance creation'!

new
    "Answer a new Delay that fires immediately"
    ^super new initialize!

forMicroseconds: anInteger
    "Answer a Delay for the specified number of microseconds"
    ^self new duration: anInteger!

forMilliseconds: anInteger
    "Answer a Delay for the specified number of milliseconds"
    ^self new duration: anInteger * 1000!

forSeconds: aNumber
    "Answer a Delay for the specified number of seconds"
    ^self forMilliseconds: (aNumber * 1000) truncated!

untilMilliseconds: anInteger
    "Answer a Delay until the millisecond clock reaches anInteger"
    ^self new resumptionTime: anInteger * 1000!

untilMicroseconds: anInteger
    "Answer a Delay until the microsecond clock reaches anInteger"
    ^self new resumptionTime: anInteger!

! !

!Delay class methodsFor: 'timing'!

microsecondClockValue
    "Answer the current microsecond clock value (relative to VM start)"
    <primitive: 189>
    self primitiveFailed!

millisecondClockValue
    "Answer the current millisecond clock value"
    <primitive: 174>
    self primitiveFailed!

signalTimerAfter: milliseconds
    "Request timer to signal the timing semaphore after milliseconds.
     milliseconds < 0 cancels any pending timer.
     milliseconds = 0 signals immediately."
    <primitive: 100>
    self primitiveFailed!

signalTimerAfter: milliseconds semaphore: aSemaphore
    "Request timer to signal aSemaphore after milliseconds"
    "For now, use the simple timer primitive"
    self signalTimerAfter: milliseconds.
    "The primitive signals TimingSemaphore, we need to transfer to aSemaphore"
    "This is a simplified implementation - full version would use TimingProcess"
    aSemaphore signal!

! !

!Delay class methodsFor: 'class initialization'!

initialize
    "Initialize class variables"
    AccessProtect := Semaphore forMutualExclusion.
    Pending := OrderedCollection new.  "Use OrderedCollection instead of SortedCollection"
    Current := nil.
    TimingSemaphore := Semaphore new!

! !

"============================================================================"
"BlockClosure extensions for process creation"
"============================================================================"

!BlockClosure methodsFor: 'scheduling'!

fork
    "Create and schedule a new process to evaluate the receiver"
    | p |
    p := self newProcess.
    p resume.
    ^p!

forkAt: priority
    "Create and schedule a new process at the specified priority"
    | p |
    p := self newProcess.
    p priority: priority.
    p resume.
    ^p!

forkNamed: aString
    "Create and schedule a named process"
    | p |
    p := self newProcess.
    p name: aString.
    p resume.
    ^p!

forkAt: priority named: aString
    "Create and schedule a named process at the specified priority"
    | p |
    p := self newProcess.
    p name: aString.
    p priority: priority.
    p resume.
    ^p!

newProcess
    "Answer a new suspended process to evaluate the receiver"
    ^Process forContext: self priority: 5!

newProcessWith: anArray
    "Answer a new process to evaluate the receiver with arguments from anArray"
    ^[self valueWithArguments: anArray] newProcess!

! !

"============================================================================"
"Initialize classes"
"============================================================================"

Process initialize!
Delay initialize!

"Success message"
Transcript show: 'Process scheduling classes loaded successfully'; cr.
Transcript show: '  - Link, LinkedList'; cr.
Transcript show: '  - Semaphore (signal/wait/critical:)'; cr.
Transcript show: '  - Process (suspend/resume/terminate/priority:)'; cr.
Transcript show: '  - ProcessorScheduler (Processor global)'; cr.
Transcript show: '  - Delay (forMilliseconds:/forSeconds:/wait)'; cr.
Transcript show: '  - BlockClosure>>fork, forkAt:, newProcess'; cr!
