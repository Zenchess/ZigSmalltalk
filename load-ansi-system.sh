#!/bin/bash
FILES=$(grep -v '^$' load-order-local.txt | tr '\n' ' ')
STUBS="dolphin-core/stubs/sunit-stubs.st"
echo "Loading ANSI files from load-order.txt..."
echo "Loading stubs: $STUBS"
echo ""
echo "After loading, you can run tests manually:"
echo "  - TestRunner runTest: ExampleTest"
echo "  - TestRunner runTests: (Array with: ExampleTest)"
echo "  - Or create your own test classes"
echo ""
echo "Loading now... (this may take a while)"
echo "==========================================="

# Load all files with Transcript output
./zig-out/bin/zig-smalltalk $FILES $STUBS << 'EOF'

"Add Transcript output to show progress"
Transcript show: '=== Starting ANSI System Load ==='; cr.
Transcript show: 'Loading files from load-order.txt...'; cr.

"Create a progress tracking variable"
| fileCount totalFiles loadedFiles |
loadedFiles := 0.

"Get total file count"
totalFiles := FILES size.

"Show loading progress"
Transcript show: 'Total files to load: ', totalFiles printString; cr.
Transcript show: 'Files loaded: ', loadedFiles printString, '/', totalFiles printString; cr.

"After loading is complete"
Transcript show: '=== ANSI System Load Complete ==='; cr.
Transcript show: 'You can now run tests manually:'; cr.
Transcript show: '  - ObjectANSITest buildSuite run'; cr.
Transcript show: '  - Or run your own test code'; cr.
Transcript show: '=== Ready for Testing ==='; cr.

EOF