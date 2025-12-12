const std = @import("std");

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    // Generate FFI bindings from config
    const gen_ffi_exe = b.addExecutable(.{
        .name = "gen-ffi",
        .root_module = b.createModule(.{
            .root_source_file = b.path("tools/gen_ffi.zig"),
            .target = b.graph.host,
            .optimize = .Debug,
        }),
    });
    const gen_ffi_step = b.addRunArtifact(gen_ffi_exe);

    // The generator writes to src/vm/ffi_generated.zig
    const gen_ffi = b.step("gen-ffi", "Generate FFI bindings from ffi-config.json");
    gen_ffi.dependOn(&gen_ffi_step.step);

    const exe = b.addExecutable(.{
        .name = "zig-smalltalk",
        .root_module = b.createModule(.{
            .root_source_file = b.path("src/main.zig"),
            .target = target,
            .optimize = optimize,
        }),
    });

    // Link C library for FFI
    exe.linkLibC();

    // Read ffi-config.json and link required libraries
    linkFFILibraries(b, exe);

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

fn linkFFILibraries(b: *std.Build, exe: *std.Build.Step.Compile) void {
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

    for (libs.array.items) |lib| {
        if (lib != .object) continue;

        const enabled = lib.object.get("enabled");
        if (enabled != null and enabled.? == .bool and !enabled.?.bool) continue;

        const link = lib.object.get("link") orelse continue;
        if (link != .string) continue;

        // Link the library
        if (link.string.len > 0) {
            // Check if it's a system library (just name) or path
            if (std.mem.startsWith(u8, link.string, "/")) {
                // It's a path
                exe.addLibraryPath(.{ .cwd_relative = std.fs.path.dirname(link.string) orelse "." });
                const basename = std.fs.path.basename(link.string);
                // Strip lib prefix and .so/.a suffix
                var libname = basename;
                if (std.mem.startsWith(u8, libname, "lib")) {
                    libname = libname[3..];
                }
                if (std.mem.endsWith(u8, libname, ".so")) {
                    libname = libname[0 .. libname.len - 3];
                } else if (std.mem.endsWith(u8, libname, ".a")) {
                    libname = libname[0 .. libname.len - 2];
                }
                exe.linkSystemLibrary(libname);
            } else {
                // It's a system library name
                exe.linkSystemLibrary(link.string);
            }
        }
    }
}
