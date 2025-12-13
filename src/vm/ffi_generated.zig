//! Auto-generated FFI bindings
//! Generated from ffi-config.json by tools/gen_ffi.zig
//! DO NOT EDIT - regenerate with: zig build gen-ffi

const std = @import("std");

/// LibC library imports
pub const LibC = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("time.h");
});

/// LibMath library imports
pub const LibMath = @cImport({
    @cInclude("math.h");
});

/// Raylib library imports
pub const Raylib = @cImport({
    @cInclude("raylib.h");
});

/// GLFW library imports
pub const GLFW = @cImport({
    @cInclude("GLFW/glfw3.h");
});

/// GL library imports
pub const GL = @cImport({
    @cInclude("GL/glew.h");
});

/// GLX for glXGetProcAddress
pub const GLX = @cImport({
    @cInclude("GL/glx.h");
});

/// Combined C imports (for backwards compatibility)
pub const c = @cImport({
    @cInclude("stdio.h");
    @cInclude("stdlib.h");
    @cInclude("string.h");
    @cInclude("time.h");
    @cInclude("math.h");
    @cInclude("raylib.h");
    @cInclude("GLFW/glfw3.h");
});

/// List of configured libraries
pub const library_names = [_][]const u8{
    "LibC",
    "LibMath",
    "Raylib",
    "GLFW",
    "GL",
};

/// Function lists for each library (empty = auto-discover)
pub const library_functions = struct {
    pub const LibC = [_][]const u8{
        "puts",
        "putchar",
        "getchar",
        "strlen",
        "strcmp",
        "strncmp",
        "memset",
        "memcpy",
        "memmove",
        "memcmp",
        "malloc",
        "calloc",
        "realloc",
        "free",
        "atoi",
        "atol",
        "atof",
        "abs",
        "labs",
        "rand",
        "srand",
        "time",
        "clock",
        "system",
        "getenv",
        "exit",
    };
    pub const LibMath = [_][]const u8{
        "sin",
        "cos",
        "tan",
        "asin",
        "acos",
        "atan",
        "atan2",
        "sinh",
        "cosh",
        "tanh",
        "exp",
        "log",
        "log10",
        "log2",
        "pow",
        "sqrt",
        "cbrt",
        "ceil",
        "floor",
        "round",
        "trunc",
        "fabs",
        "fmod",
        "fmin",
        "fmax",
        "hypot",
    };
    pub const Raylib = [_][]const u8{
    };
    pub const GLFW = [_][]const u8{
        "glfwInit",
        "glfwTerminate",
        "glfwInitHint",
        "glfwGetVersion",
        "glfwGetVersionString",
        "glfwGetError",
        "glfwSetErrorCallback",
        "glfwGetMonitors",
        "glfwGetPrimaryMonitor",
        "glfwGetMonitorPos",
        "glfwGetMonitorWorkarea",
        "glfwGetMonitorPhysicalSize",
        "glfwGetMonitorContentScale",
        "glfwGetMonitorName",
        "glfwSetMonitorCallback",
        "glfwGetVideoModes",
        "glfwGetVideoMode",
        "glfwSetGamma",
        "glfwGetGammaRamp",
        "glfwSetGammaRamp",
        "glfwDefaultWindowHints",
        "glfwWindowHint",
        "glfwWindowHintString",
        "glfwCreateWindow",
        "glfwDestroyWindow",
        "glfwWindowShouldClose",
        "glfwSetWindowShouldClose",
        "glfwSetWindowTitle",
        "glfwSetWindowIcon",
        "glfwGetWindowPos",
        "glfwSetWindowPos",
        "glfwGetWindowSize",
        "glfwSetWindowSizeLimits",
        "glfwSetWindowAspectRatio",
        "glfwSetWindowSize",
        "glfwGetFramebufferSize",
        "glfwGetWindowFrameSize",
        "glfwGetWindowContentScale",
        "glfwGetWindowOpacity",
        "glfwSetWindowOpacity",
        "glfwIconifyWindow",
        "glfwRestoreWindow",
        "glfwMaximizeWindow",
        "glfwShowWindow",
        "glfwHideWindow",
        "glfwFocusWindow",
        "glfwRequestWindowAttention",
        "glfwGetWindowMonitor",
        "glfwSetWindowMonitor",
        "glfwGetWindowAttrib",
        "glfwSetWindowAttrib",
        "glfwSetWindowUserPointer",
        "glfwGetWindowUserPointer",
        "glfwSetWindowPosCallback",
        "glfwSetWindowSizeCallback",
        "glfwSetWindowCloseCallback",
        "glfwSetWindowRefreshCallback",
        "glfwSetWindowFocusCallback",
        "glfwSetWindowIconifyCallback",
        "glfwSetWindowMaximizeCallback",
        "glfwSetFramebufferSizeCallback",
        "glfwSetWindowContentScaleCallback",
        "glfwPollEvents",
        "glfwWaitEvents",
        "glfwWaitEventsTimeout",
        "glfwPostEmptyEvent",
        "glfwGetInputMode",
        "glfwSetInputMode",
        "glfwRawMouseMotionSupported",
        "glfwGetKeyName",
        "glfwGetKeyScancode",
        "glfwGetKey",
        "glfwGetMouseButton",
        "glfwGetCursorPos",
        "glfwSetCursorPos",
        "glfwCreateCursor",
        "glfwCreateStandardCursor",
        "glfwDestroyCursor",
        "glfwSetCursor",
        "glfwSetKeyCallback",
        "glfwSetCharCallback",
        "glfwSetCharModsCallback",
        "glfwSetMouseButtonCallback",
        "glfwSetCursorPosCallback",
        "glfwSetCursorEnterCallback",
        "glfwSetScrollCallback",
        "glfwSetDropCallback",
        "glfwJoystickPresent",
        "glfwGetJoystickAxes",
        "glfwGetJoystickButtons",
        "glfwGetJoystickHats",
        "glfwGetJoystickName",
        "glfwGetJoystickGUID",
        "glfwSetJoystickUserPointer",
        "glfwGetJoystickUserPointer",
        "glfwJoystickIsGamepad",
        "glfwSetJoystickCallback",
        "glfwUpdateGamepadMappings",
        "glfwGetGamepadName",
        "glfwGetGamepadState",
        "glfwSetClipboardString",
        "glfwGetClipboardString",
        "glfwGetTime",
        "glfwSetTime",
        "glfwGetTimerValue",
        "glfwGetTimerFrequency",
        "glfwMakeContextCurrent",
        "glfwGetCurrentContext",
        "glfwSwapBuffers",
        "glfwSwapInterval",
        "glfwExtensionSupported",
        "glfwGetProcAddress",
    };
    pub const GL = [_][]const u8{
        "glewInit",
        "glClear",
        "glClearColor",
        "glEnable",
        "glDisable",
        "glViewport",
        "glGetError",
        "glGetString",
        "glMatrixMode",
        "glLoadIdentity",
        "glPushMatrix",
        "glPopMatrix",
        "glTranslatef",
        "glRotatef",
        "glScalef",
        "glBegin",
        "glEnd",
        "glVertex2f",
        "glVertex3f",
        "glColor3f",
        "glColor4f",
        "glNormal3f",
        "glTexCoord2f",
        "glFlush",
        "glFinish",
        "glOrtho",
        "glFrustum",
        "glDepthFunc",
        "glCullFace",
        "glFrontFace",
        "glShadeModel",
        "glLightfv",
        "glMaterialfv",
        "glGenTextures",
        "glBindTexture",
        "glTexImage2D",
        "glTexParameteri",
        "glDeleteTextures",
        "glBlendFunc",
        "glPolygonMode",
        "glLineWidth",
        "glPointSize",
        "glCreateShader",
        "glDeleteShader",
        "glShaderSource",
        "glCompileShader",
        "glGetShaderiv",
        "glGetShaderInfoLog",
        "glCreateProgram",
        "glDeleteProgram",
        "glAttachShader",
        "glLinkProgram",
        "glGetProgramiv",
        "glGetProgramInfoLog",
        "glUseProgram",
        "glGetUniformLocation",
        "glUniform1i",
        "glUniform1f",
        "glUniform2f",
        "glUniform3f",
        "glUniform4f",
        "glUniformMatrix4fv",
        "glGenBuffers",
        "glDeleteBuffers",
        "glBindBuffer",
        "glBufferData",
        "glGenVertexArrays",
        "glDeleteVertexArrays",
        "glBindVertexArray",
        "glVertexAttribPointer",
        "glEnableVertexAttribArray",
        "glDisableVertexAttribArray",
        "glDrawArrays",
        "glDrawElements",
        "glActiveTexture",
        "glGenerateMipmap",
        "glDetachShader",
        "glGenFramebuffers",
        "glDeleteFramebuffers",
        "glBindFramebuffer",
        "glFramebufferTexture2D",
        "glCheckFramebufferStatus",
        "glDrawBuffer",
        "glReadBuffer",
        "glClearDepth",
        "glDepthMask",
    };
};

/// Auto-discovery flags (true = discover all functions at compile time)
pub const library_auto = struct {
    pub const LibC = false;
    pub const LibMath = false;
    pub const Raylib = true;
    pub const GLFW = false;
    pub const GL = false;
};

// ============================================================================
// Auto-generated library bindings
// ============================================================================

const ffi_autogen = @import("ffi_autogen.zig");

pub const LibC_binding = ffi_autogen.FFILibrary(LibC, "LibC", &library_functions.LibC);
pub const LibMath_binding = ffi_autogen.FFILibrary(LibMath, "LibMath", &library_functions.LibMath);
pub const Raylib_binding = ffi_autogen.FFILibraryWithStructs(Raylib, "Raylib");
pub const GLFW_binding = ffi_autogen.FFILibrary(GLFW, "GLFW", &library_functions.GLFW);
pub const GL_binding = ffi_autogen.FFILibrary(GL, "GL", &library_functions.GL);

/// Call an FFI function by library and function name
pub fn callFFI(library: []const u8, func_name: []const u8, heap: *ffi_autogen.Heap, args: []const ffi_autogen.Value, alloc: std.mem.Allocator) ffi_autogen.FFIError!ffi_autogen.Value {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.call(func_name, heap, args, alloc);
    if (std.mem.eql(u8, library, "GL")) return GL_binding.call(func_name, heap, args, alloc);
    return ffi_autogen.FFIError.UnknownFunction;
}

/// Get all functions for a library
pub fn getLibraryFunctions(library: []const u8) ?[]const ffi_autogen.FFIFunction {
    if (std.mem.eql(u8, library, "LibC")) return &LibC_binding.functions;
    if (std.mem.eql(u8, library, "LibMath")) return &LibMath_binding.functions;
    if (std.mem.eql(u8, library, "Raylib")) return &Raylib_binding.functions;
    if (std.mem.eql(u8, library, "GLFW")) return &GLFW_binding.functions;
    if (std.mem.eql(u8, library, "GL")) return &GL_binding.functions;
    return null;
}

/// Get function names for a library
pub fn getLibraryFunctionNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "LibC")) return LibC_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "LibMath")) return LibMath_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "GLFW")) return GLFW_binding.getFunctionNames();
    if (std.mem.eql(u8, library, "GL")) return GL_binding.getFunctionNames();
    return null;
}

/// Get struct names for a library
pub fn getLibraryStructNames(library: []const u8) ?[]const []const u8 {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStructNames();
    return null;
}

/// Get struct info by name
pub fn getStructInfo(library: []const u8, struct_name: []const u8) ?ffi_autogen.StructInfo {
    if (std.mem.eql(u8, library, "Raylib")) return Raylib_binding.getStruct(struct_name);
    return null;
}

/// Generate Smalltalk code for all structs in a library
pub fn generateStructCode(library: []const u8, writer: anytype) !bool {
    if (std.mem.eql(u8, library, "Raylib")) {
        try Raylib_binding.generateStructCode(writer);
        return true;
    }
    return false;
}

/// Set glewExperimental to GL_TRUE (required for core profile contexts)
pub fn setGlewExperimental(value: bool) void {
    GL.glewExperimental = if (value) GL.GL_TRUE else GL.GL_FALSE;
}

/// Get an OpenGL function pointer by name using glXGetProcAddress
/// Returns the function pointer address for use with runtime FFI
/// NOTE: Requires a valid OpenGL context to be current
pub fn getGLEWFunctionPointer(name: []const u8) ?usize {
    // Create null-terminated string for glXGetProcAddress
    var name_buf: [256]u8 = undefined;
    if (name.len >= name_buf.len) return null;
    @memcpy(name_buf[0..name.len], name);
    name_buf[name.len] = 0;

    // Use glXGetProcAddress to dynamically get the function pointer
    const proc = GLX.glXGetProcAddress(@ptrCast(&name_buf));
    if (proc) |p| {
        return @intFromPtr(p);
    }
    return null;
}
