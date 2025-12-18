const std = @import("std");

const LibffiResult = struct {
    lib: *std.Build.Step.Compile,
    include_path: std.Build.LazyPath,
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Build option to force-disable FFI (auto-detection can be overridden)
    const force_no_ffi = b.option(bool, "no-ffi", "Force disable FFI support") orelse false;

    // Try to build vendored libffi from source
    const libffi_result = buildLibffi(b, target, optimize);

    // FFI is available if we successfully built libffi OR if system libraries exist
    const ffi_available = !force_no_ffi and (libffi_result != null or detectFFILibraries());

    if (ffi_available) {
        if (libffi_result != null) {
            std.debug.print("FFI: Building vendored libffi from source\n", .{});
        } else {
            std.debug.print("FFI: Using system libffi\n", .{});
        }
    } else if (force_no_ffi) {
        std.debug.print("FFI: Disabled by -Dno-ffi flag\n", .{});
    } else {
        std.debug.print("FFI: Could not build libffi, building without FFI support\n", .{});
    }

    // Generate FFI bindings from config at build time
    const gen_ffi_exe = b.addExecutable(.{
        .name = "gen-ffi",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_ffi.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });

    // Build-time FFI generation with target awareness
    const gen_ffi_run = b.addRunArtifact(gen_ffi_exe);
    const target_str = if (target.result.os.tag == .windows)
        "x86_64-windows"
    else if (target.result.cpu.arch == .aarch64)
        "aarch64-linux"
    else
        "x86_64-linux";
    gen_ffi_run.addArg(target_str);
    // Output to src/vm so existing imports work
    gen_ffi_run.addArg("src/vm");
    gen_ffi_run.setCwd(b.path(".")); // Run from project root

    // Manual gen-ffi step (same as above, for explicit invocation)
    const gen_ffi = b.step("gen-ffi", "Generate FFI bindings from ffi-config.json");
    gen_ffi.dependOn(&gen_ffi_run.step);

    const exe = b.addExecutable(.{
        .name = "zig-smalltalk",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Ensure FFI bindings are generated before compiling
    exe.step.dependOn(&gen_ffi_run.step);

    // Pass FFI enabled/disabled flag to the code
    const options = b.addOptions();
    options.addOption(bool, "ffi_enabled", ffi_available);
    exe.root_module.addOptions("build_options", options);

    // Link C library (always needed for basic operations)
    exe.linkLibC();

    // Only link FFI libraries if they're available
    if (ffi_available) {
        // Link libffi - prefer vendored, fall back to system
        if (libffi_result) |result| {
            exe.linkLibrary(result.lib);
            // Add the generated headers to exe's include path for @cImport
            exe.addIncludePath(result.include_path);
        } else {
            exe.linkSystemLibrary("ffi");
        }

        // Platform-specific additional libraries (based on target, not build host)
        if (target.result.os.tag != .windows) {
            // Linux-specific optional libraries - only link if they exist
            if (libraryExists("floattest", false)) {
                exe.addIncludePath(.{ .cwd_relative = "/tmp" });
                exe.addLibraryPath(.{ .cwd_relative = "/tmp" });
                exe.linkSystemLibrary("floattest");
            }

            // Link GLEW for modern OpenGL (Linux) - optional
            if (libraryExists("GLEW", false)) {
                exe.linkSystemLibrary("GLEW");
            }
        }

        // Read ffi-config.json and link required libraries
        linkFFILibraries(b, exe, target);
    }

    b.installArtifact(exe);

    // Run command
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    if (b.args) |args| {
        run_cmd.addArgs(args);
    }

    const run_step = b.step("run", "Run the Zig Smalltalk REPL");
    run_step.dependOn(&run_cmd.step);

    // Tests
    const unit_tests = b.addTest(.{
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    const run_unit_tests = b.addRunArtifact(unit_tests);
    const test_step = b.step("test", "Run unit tests");
    test_step.dependOn(&run_unit_tests.step);
}

/// Build libffi from vendored source
fn buildLibffi(b: *std.Build, target: std.Build.ResolvedTarget, optimize: std.builtin.OptimizeMode) ?LibffiResult {
    // Get the libffi dependency from build.zig.zon
    const libffi_dep = b.lazyDependency("libffi", .{}) orelse return null;

    const lib = b.addLibrary(.{
        .name = "ffi",
        .linkage = .static,
        .root_module = b.createModule(.{
            .target = target,
            .optimize = optimize,
            .link_libc = true,
        }),
    });

    // Determine platform at comptime using Zig's target info
    const is_windows = target.result.os.tag == .windows;
    const is_x86_64 = target.result.cpu.arch == .x86_64;
    const is_aarch64 = target.result.cpu.arch == .aarch64;

    // Build and run the header generator
    const gen_headers_exe = b.addExecutable(.{
        .name = "gen-ffi-headers",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_ffi_headers.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });

    const gen_headers_step = b.addRunArtifact(gen_headers_exe);
    // Arguments: libffi_src_dir, output_dir, target
    gen_headers_step.addDirectoryArg(libffi_dep.path("."));
    const generated_headers = gen_headers_step.addOutputDirectoryArg("generated-ffi-headers");

    const target_str = if (is_windows and is_x86_64)
        "x86_64-windows"
    else if (is_x86_64)
        "x86_64-linux"
    else if (is_aarch64)
        "aarch64-linux"
    else
        "x86_64-linux";
    gen_headers_step.addArg(target_str);

    // Add generated headers first (takes precedence)
    lib.addIncludePath(generated_headers);

    // Add libffi source include paths
    lib.addIncludePath(libffi_dep.path("src"));
    lib.addIncludePath(libffi_dep.path("include"));

    // Platform-specific C flags with target macro
    const target_macro = if (is_windows and is_x86_64)
        "-DX86_WIN64"
    else if (is_x86_64)
        "-DX86_64"
    else if (is_aarch64)
        "-DAARCH64"
    else
        "-DX86_64";

    const common_cflags = &[_][]const u8{
        "-DFFI_BUILDING",
        "-DHAVE_CONFIG_H",
        target_macro,
    };

    // Common source files (platform independent)
    const base_sources = [_][]const u8{
        "src/prep_cif.c",
        "src/types.c",
        "src/raw_api.c",
        "src/java_raw_api.c",
        "src/closures.c",
    };

    for (base_sources) |src| {
        lib.addCSourceFile(.{
            .file = libffi_dep.path(src),
            .flags = common_cflags,
        });
    }

    // Add trampoline support for Unix (needed for FFI_EXEC_STATIC_TRAMP)
    if (!is_windows) {
        lib.addCSourceFile(.{
            .file = libffi_dep.path("src/tramp.c"),
            .flags = common_cflags,
        });
    }

    // Architecture-specific sources and assembly
    if (is_x86_64) {
        lib.addIncludePath(libffi_dep.path("src/x86"));

        if (is_windows) {
            // Windows x64
            lib.addCSourceFile(.{
                .file = libffi_dep.path("src/x86/ffiw64.c"),
                .flags = common_cflags,
            });
            // Windows x64 assembly (GNU syntax - win64_intel.S uses MASM which Zig doesn't support)
            lib.addAssemblyFile(libffi_dep.path("src/x86/win64.S"));
        } else {
            // Unix x64
            lib.addCSourceFile(.{
                .file = libffi_dep.path("src/x86/ffi64.c"),
                .flags = common_cflags,
            });
            lib.addCSourceFile(.{
                .file = libffi_dep.path("src/x86/ffiw64.c"),
                .flags = common_cflags,
            });
            // Use external assembler for .S files - Zig's integrated assembler doesn't support GNU @rel syntax
            lib.addCSourceFile(.{
                .file = libffi_dep.path("src/x86/unix64.S"),
                .flags = &[_][]const u8{"-fno-integrated-as"},
            });
            lib.addCSourceFile(.{
                .file = libffi_dep.path("src/x86/win64.S"),
                .flags = &[_][]const u8{"-fno-integrated-as"},
            });
        }
    } else if (is_aarch64) {
        lib.addIncludePath(libffi_dep.path("src/aarch64"));
        lib.addCSourceFile(.{
            .file = libffi_dep.path("src/aarch64/ffi.c"),
            .flags = common_cflags,
        });
        // Use external assembler for .S files - Zig's integrated assembler doesn't support all GNU syntax
        lib.addCSourceFile(.{
            .file = libffi_dep.path("src/aarch64/sysv.S"),
            .flags = &[_][]const u8{"-fno-integrated-as"},
        });
    } else {
        // Unsupported architecture
        return null;
    }

    return .{
        .lib = lib,
        .include_path = generated_headers,
    };
}

/// Check if the required FFI libraries are available on the system
fn detectFFILibraries() bool {
    const builtin = @import("builtin");

    // Check for libffi - the core requirement for FFI
    if (builtin.os.tag == .windows) {
        // On Windows, check for common library locations
        const windows_paths = [_][]const u8{
            "C:\\msys64\\mingw64\\lib\\libffi.a",
            "C:\\msys64\\ucrt64\\lib\\libffi.a",
            "C:\\vcpkg\\installed\\x64-windows\\lib\\ffi.lib",
        };
        for (windows_paths) |path| {
            if (std.fs.cwd().access(path, .{})) |_| {
                return true;
            } else |_| {}
        }
        // Also check if ffi.dll exists in PATH (harder to check)
        return false;
    } else {
        // On Linux/macOS, check standard library paths
        const unix_paths = [_][]const u8{
            "/usr/lib/libffi.so",
            "/usr/lib/x86_64-linux-gnu/libffi.so",
            "/usr/lib64/libffi.so",
            "/usr/local/lib/libffi.so",
            "/opt/homebrew/lib/libffi.dylib",
            "/usr/local/lib/libffi.dylib",
        };
        for (unix_paths) |path| {
            if (std.fs.cwd().access(path, .{})) |_| {
                return true;
            } else |_| {}
        }
        return false;
    }
}

fn linkFFILibraries(b: *std.Build, exe: *std.Build.Step.Compile, target: std.Build.ResolvedTarget) void {
    const target_is_windows = target.result.os.tag == .windows;

    // Read ffi-config.json at build time
    const config_content = std.fs.cwd().readFileAlloc(b.allocator, "ffi-config.json", 1024 * 1024) catch {
        return; // No config file, use defaults
    };
    defer b.allocator.free(config_content);

    const parsed = std.json.parseFromSlice(std.json.Value, b.allocator, config_content, .{}) catch {
        return;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return;

    const libs = root.object.get("libraries") orelse return;
    if (libs != .array) return;

    // Track libraries to disable due to missing files
    var libs_to_disable = std.ArrayListUnmanaged([]const u8){};

    for (libs.array.items) |lib| {
        if (lib != .object) continue;

        const lib_name = if (lib.object.get("name")) |n| (if (n == .string) n.string else "unknown") else "unknown";

        const enabled = lib.object.get("enabled");
        if (enabled) |en| {
            if (en == .bool and !en.bool) continue;
        }

        // Process header paths - add include directories
        const headers = lib.object.get("headers");
        if (headers != null and headers.? == .array) {
            for (headers.?.array.items) |header| {
                if (header != .string) continue;
                if (header.string.len == 0) continue;

                // Skip Windows paths on non-Windows targets and vice versa
                const is_windows_path = isWindowsPath(header.string);
                if (is_windows_path and !target_is_windows) {
                    std.debug.print("FFI: Skipping Windows header path '{s}' for non-Windows target\n", .{header.string});
                    continue;
                }
                if (!is_windows_path and isFullPath(header.string) and target_is_windows) {
                    std.debug.print("FFI: Skipping Unix header path '{s}' for Windows target\n", .{header.string});
                    continue;
                }

                // If it's a full path, extract the directory
                if (isFullPath(header.string)) {
                    if (std.fs.path.dirname(header.string)) |dir| {
                        exe.addIncludePath(.{ .cwd_relative = dir });
                        std.debug.print("FFI: Added include path '{s}'\n", .{dir});

                        // Also add parent directory (needed for Vulkan's vk_video includes)
                        if (std.fs.path.dirname(dir)) |parent_dir| {
                            exe.addIncludePath(.{ .cwd_relative = parent_dir });
                            std.debug.print("FFI: Added parent include path '{s}'\n", .{parent_dir});
                        }
                    }
                }
            }
        }

        const link = lib.object.get("link") orelse continue;
        if (link != .string) continue;

        // Skip empty links
        if (link.string.len == 0) continue;

        // Skip libc and libm - they're handled separately by linkLibC()
        if (std.mem.eql(u8, link.string, "c") or std.mem.eql(u8, link.string, "m")) continue;

        // Skip platform-incompatible libraries
        // Note: .a files can be valid on both Unix and Windows (MinGW), so we check the path prefix
        const is_windows_path = isWindowsPath(link.string);
        const is_windows_lib = is_windows_path or std.mem.endsWith(u8, link.string, ".dll") or std.mem.endsWith(u8, link.string, ".lib");
        // .so and .dylib are Unix-only; .a files with Unix paths are Unix-only
        const is_unix_only_lib = std.mem.endsWith(u8, link.string, ".so") or std.mem.endsWith(u8, link.string, ".dylib") or
            (std.mem.endsWith(u8, link.string, ".a") and !is_windows_path);

        if (is_windows_lib and !target_is_windows) {
            std.debug.print("FFI: Skipping Windows library '{s}' ({s}) for non-Windows target\n", .{ lib_name, link.string });
            continue;
        }
        if (is_unix_only_lib and target_is_windows) {
            std.debug.print("FFI: Skipping Unix library '{s}' ({s}) for Windows target\n", .{ lib_name, link.string });
            continue;
        }

        // Link the library
        if (isFullPath(link.string)) {
            // Check if the library file actually exists before linking
            const file_exists = blk: {
                std.fs.cwd().access(link.string, .{}) catch {
                    break :blk false;
                };
                break :blk true;
            };

            if (!file_exists) {
                std.debug.print("FFI: WARNING - Library file not found: '{s}'\n", .{link.string});
                std.debug.print("FFI: Disabling library '{s}' and updating ffi-config.json\n", .{lib_name});
                libs_to_disable.append(b.allocator, lib_name) catch {};
                continue;
            }

            // It's a full path - extract directory and library name
            if (std.fs.path.dirname(link.string)) |dir| {
                exe.addLibraryPath(.{ .cwd_relative = dir });
                std.debug.print("FFI: Added library path '{s}'\n", .{dir});
            }

            const basename = std.fs.path.basename(link.string);
            const libname = extractLibName(basename);

            if (libname.len > 0) {
                exe.linkSystemLibrary(libname);
                std.debug.print("FFI: Linking library '{s}'\n", .{libname});
            }
        } else {
            // It's a system library name
            exe.linkSystemLibrary(link.string);
            std.debug.print("FFI: Linking system library '{s}'\n", .{link.string});
        }
    }

    // If we found missing libraries, update the config file to disable them
    if (libs_to_disable.items.len > 0) {
        updateFFIConfigDisableLibs(b.allocator, libs_to_disable.items);
    }
}

/// Update ffi-config.json to disable libraries with missing files
fn updateFFIConfigDisableLibs(allocator: std.mem.Allocator, libs_to_disable: []const []const u8) void {
    const config_content = std.fs.cwd().readFileAlloc(allocator, "ffi-config.json", 1024 * 1024) catch {
        return;
    };
    defer allocator.free(config_content);

    const parsed = std.json.parseFromSlice(std.json.Value, allocator, config_content, .{}) catch {
        return;
    };
    defer parsed.deinit();

    const root = parsed.value;
    if (root != .object) return;

    const libs = root.object.get("libraries") orelse return;
    if (libs != .array) return;

    // Build a new JSON string with the disabled libraries
    var output = std.ArrayListUnmanaged(u8){};

    output.appendSlice(allocator, "{\n  \"libraries\": [\n") catch return;

    var first = true;
    for (libs.array.items) |lib| {
        if (lib != .object) continue;

        const lib_name = if (lib.object.get("name")) |n| (if (n == .string) n.string else null) else null;
        const lib_headers = lib.object.get("headers");
        const lib_link = lib.object.get("link");
        const lib_enabled = lib.object.get("enabled");

        // Check if this library should be disabled
        var should_disable = false;
        if (lib_name) |name| {
            for (libs_to_disable) |disable_name| {
                if (std.mem.eql(u8, name, disable_name)) {
                    should_disable = true;
                    break;
                }
            }
        }

        if (!first) {
            output.appendSlice(allocator, ",\n") catch return;
        }
        first = false;

        output.appendSlice(allocator, "    {\n") catch return;

        // Name
        output.appendSlice(allocator, "      \"name\": \"") catch return;
        if (lib_name) |name| {
            output.appendSlice(allocator, name) catch return;
        }
        output.appendSlice(allocator, "\",\n") catch return;

        // Headers
        output.appendSlice(allocator, "      \"headers\": [") catch return;
        if (lib_headers != null and lib_headers.? == .array) {
            var header_first = true;
            for (lib_headers.?.array.items) |header| {
                if (header != .string) continue;
                if (!header_first) {
                    output.appendSlice(allocator, ", ") catch return;
                }
                header_first = false;
                output.appendSlice(allocator, "\"") catch return;
                // Escape backslashes in paths
                for (header.string) |c| {
                    if (c == '\\') {
                        output.appendSlice(allocator, "\\\\") catch return;
                    } else {
                        output.append(allocator, c) catch return;
                    }
                }
                output.appendSlice(allocator, "\"") catch return;
            }
        }
        output.appendSlice(allocator, "],\n") catch return;

        // Link
        output.appendSlice(allocator, "      \"link\": \"") catch return;
        if (lib_link != null and lib_link.? == .string) {
            // Escape backslashes in paths
            for (lib_link.?.string) |c| {
                if (c == '\\') {
                    output.appendSlice(allocator, "\\\\") catch return;
                } else {
                    output.append(allocator, c) catch return;
                }
            }
        }
        output.appendSlice(allocator, "\",\n") catch return;

        // Enabled - disable if in our list
        output.appendSlice(allocator, "      \"enabled\": ") catch return;
        if (should_disable) {
            output.appendSlice(allocator, "false") catch return;
            std.debug.print("FFI: Disabled library '{s}' in ffi-config.json\n", .{lib_name orelse "unknown"});
        } else if (lib_enabled != null and lib_enabled.? == .bool) {
            output.appendSlice(allocator, if (lib_enabled.?.bool) "true" else "false") catch return;
        } else {
            output.appendSlice(allocator, "true") catch return;
        }
        output.appendSlice(allocator, "\n    }") catch return;
    }

    output.appendSlice(allocator, "\n  ]\n}\n") catch return;

    // Write the updated config
    const file = std.fs.cwd().createFile("ffi-config.json", .{}) catch {
        std.debug.print("FFI: Failed to update ffi-config.json\n", .{});
        return;
    };
    defer file.close();
    file.writeAll(output.items) catch {
        std.debug.print("FFI: Failed to write ffi-config.json\n", .{});
        return;
    };

    std.debug.print("FFI: Updated ffi-config.json - disabled libraries with missing files\n", .{});
}

/// Check if a string looks like a full file path
fn isFullPath(path: []const u8) bool {
    if (path.len == 0) return false;
    // Unix absolute path
    if (path[0] == '/') return true;
    // Windows absolute path (e.g., C:\...)
    if (path.len >= 3 and path[1] == ':' and (path[2] == '\\' or path[2] == '/')) return true;
    return false;
}

/// Check if a path looks like a Windows path (has drive letter like C:\)
fn isWindowsPath(path: []const u8) bool {
    // A Windows path has a drive letter pattern: X:\ or X:/
    // where X is a letter (a-z or A-Z)
    if (path.len >= 3) {
        const first_char = path[0];
        const is_letter = (first_char >= 'a' and first_char <= 'z') or (first_char >= 'A' and first_char <= 'Z');
        if (is_letter and path[1] == ':' and (path[2] == '\\' or path[2] == '/')) {
            return true;
        }
    }
    return false;
}

/// Extract library name from a filename, stripping prefixes and extensions
fn extractLibName(basename: []const u8) []const u8 {
    var libname = basename;

    // Strip 'lib' prefix
    if (std.mem.startsWith(u8, libname, "lib")) {
        libname = libname[3..];
    }

    // Strip extensions (order matters - check longer ones first)
    if (std.mem.endsWith(u8, libname, ".dll.a")) {
        libname = libname[0 .. libname.len - 6];
    } else if (std.mem.endsWith(u8, libname, ".so")) {
        libname = libname[0 .. libname.len - 3];
    } else if (std.mem.endsWith(u8, libname, ".a")) {
        libname = libname[0 .. libname.len - 2];
    } else if (std.mem.endsWith(u8, libname, ".lib")) {
        libname = libname[0 .. libname.len - 4];
    } else if (std.mem.endsWith(u8, libname, ".dylib")) {
        libname = libname[0 .. libname.len - 6];
    } else if (std.mem.endsWith(u8, libname, ".dll")) {
        libname = libname[0 .. libname.len - 4];
    }

    return libname;
}

/// Check if a library exists on the system
fn libraryExists(lib_name: []const u8, is_windows: bool) bool {
    if (is_windows) {
        // On Windows, check common paths for libraries
        const prefixes = [_][]const u8{
            "C:\\msys64\\mingw64\\lib\\",
            "C:\\msys64\\ucrt64\\lib\\",
            "C:\\vcpkg\\installed\\x64-windows\\lib\\",
        };
        const extensions = [_][]const u8{ ".lib", ".a", ".dll.a" };

        for (prefixes) |prefix| {
            for (extensions) |ext| {
                var path_buf: [512]u8 = undefined;
                const path = std.fmt.bufPrint(&path_buf, "{s}lib{s}{s}", .{ prefix, lib_name, ext }) catch continue;
                if (std.fs.cwd().access(path, .{})) |_| {
                    return true;
                } else |_| {}

                // Also try without lib prefix
                const path2 = std.fmt.bufPrint(&path_buf, "{s}{s}{s}", .{ prefix, lib_name, ext }) catch continue;
                if (std.fs.cwd().access(path2, .{})) |_| {
                    return true;
                } else |_| {}
            }
        }
        return false;
    } else {
        // On Linux/macOS, check standard library paths
        const prefixes = [_][]const u8{
            "/usr/lib/",
            "/usr/lib/x86_64-linux-gnu/",
            "/usr/lib64/",
            "/usr/local/lib/",
            "/opt/homebrew/lib/",
        };
        const extensions = [_][]const u8{ ".so", ".a", ".dylib" };

        for (prefixes) |prefix| {
            for (extensions) |ext| {
                var path_buf: [512]u8 = undefined;
                const path = std.fmt.bufPrint(&path_buf, "{s}lib{s}{s}", .{ prefix, lib_name, ext }) catch continue;
                if (std.fs.cwd().access(path, .{})) |_| {
                    return true;
                } else |_| {}
            }
        }
        return false;
    }
}
