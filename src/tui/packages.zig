const std = @import("std");
const Heap = @import("../vm/memory.zig").Heap;

/// Package - a collection of classes that can be saved/loaded together
pub const Package = struct {
    allocator: std.mem.Allocator,
    name: []const u8,
    classes: std.ArrayList([]const u8),
    file_path: ?[]const u8 = null, // Path for saving (relative to base directory)
    modified: bool = false,
    is_system: bool = false, // System packages are read-only and saved to system-packages/
    loaded: bool = true, // Whether the package is currently loaded

    pub fn init(allocator: std.mem.Allocator, name: []const u8) Package {
        return .{
            .allocator = allocator,
            .name = name,
            .classes = std.ArrayList([]const u8).empty,
        };
    }

    pub fn deinit(self: *Package, allocator: std.mem.Allocator) void {
        self.classes.deinit(allocator);
        if (self.file_path) |path| {
            allocator.free(path);
        }
        allocator.free(self.name);
    }

    pub fn addClass(self: *Package, class_name: []const u8) !void {
        // Check if already in package
        for (self.classes.items) |existing| {
            if (std.mem.eql(u8, existing, class_name)) return;
        }
        try self.classes.append(self.allocator, class_name);
        self.modified = true;
    }

    pub fn removeClass(self: *Package, class_name: []const u8) void {
        var i: usize = 0;
        while (i < self.classes.items.len) {
            if (std.mem.eql(u8, self.classes.items[i], class_name)) {
                _ = self.classes.orderedRemove(i);
                self.modified = true;
                return;
            }
            i += 1;
        }
    }

    pub fn containsClass(self: *const Package, class_name: []const u8) bool {
        for (self.classes.items) |existing| {
            if (std.mem.eql(u8, existing, class_name)) return true;
        }
        return false;
    }

    /// Get the base directory for this package type
    pub fn getBaseDir(self: *const Package) []const u8 {
        return if (self.is_system) "system-packages" else "packages";
    }
};

/// System package definitions - maps package names to class name patterns
pub const SystemPackageDefinitions = struct {
    pub const Kernel = [_][]const u8{
        "Object", "UndefinedObject", "Boolean", "True", "False",
        "Behavior", "Class", "Metaclass", "ClassDescription",
        "CompiledMethod", "BlockClosure", "Context", "MethodContext",
        "Process", "ProcessorScheduler", "Semaphore", "Delay",
        "Message", "MessageSend", "Symbol", "Pragma",
    };

    pub const Collections = [_][]const u8{
        "Collection", "SequenceableCollection", "ArrayedCollection",
        "Array", "ByteArray", "String", "WordArray",
        "OrderedCollection", "SortedCollection", "LinkedList", "Interval",
        "Set", "IdentitySet", "Dictionary", "IdentityDictionary",
        "Bag", "Association", "Link", "HashedCollection",
    };

    pub const Magnitude = [_][]const u8{
        "Magnitude", "Number", "Integer", "SmallInteger", "LargeInteger",
        "Float", "Fraction", "Character", "Date", "Time", "Duration",
        "DateAndTime", "Timestamp", "Point", "Rectangle",
    };

    pub const Streams = [_][]const u8{
        "Stream", "PositionableStream", "ReadStream", "WriteStream",
        "ReadWriteStream", "FileStream", "ExternalStream",
        "TextStream", "TranscriptStream", "NullStream",
    };

    pub const Exceptions = [_][]const u8{
        "Exception", "Error", "Notification", "Warning",
        "MessageNotUnderstood", "ZeroDivide", "SubscriptOutOfBounds",
        "KeyNotFound", "NotFound", "Halt", "AssertionFailure",
    };

    pub const FFI = [_][]const u8{
        "FFILibrary", "ExternalStructure", "ExternalAddress",
        "ExternalData", "ExternalFunction", "ExternalType",
        "RaylibColor", "RaylibVector2", "RaylibVector3", "RaylibRectangle",
    };

    pub const Testing = [_][]const u8{
        "TestCase", "TestSuite", "TestResult", "TestResource",
        "TestRunner", "SUnitTest",
    };

    pub const ChessScene3D = [_][]const u8{
        "GLConstants", "OBJLoader", "ChessScene3D",
    };

    /// Get all system package names
    pub fn getPackageNames() []const []const u8 {
        return &[_][]const u8{
            "Kernel", "Collections", "Magnitude", "Streams", "Exceptions", "FFI", "Testing", "ChessScene3D",
        };
    }

    /// Check if a class belongs to a system package
    pub fn getPackageForClass(class_name: []const u8) ?[]const u8 {
        for (Kernel) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Kernel";
        }
        for (Collections) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Collections";
        }
        for (Magnitude) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Magnitude";
        }
        for (Streams) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Streams";
        }
        for (Exceptions) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Exceptions";
        }
        for (FFI) |name| {
            if (std.mem.eql(u8, name, class_name)) return "FFI";
        }
        for (Testing) |name| {
            if (std.mem.eql(u8, name, class_name)) return "Testing";
        }
        for (ChessScene3D) |name| {
            if (std.mem.eql(u8, name, class_name)) return "ChessScene3D";
        }
        return null;
    }
};

/// Package Registry - manages all packages
pub const PackageRegistry = struct {
    allocator: std.mem.Allocator,
    packages: std.ArrayList(*Package),
    class_to_package: std.StringHashMapUnmanaged(*Package), // Maps class name to package

    pub fn init(allocator: std.mem.Allocator) PackageRegistry {
        return .{
            .allocator = allocator,
            .packages = std.ArrayList(*Package).empty,
            .class_to_package = .{},
        };
    }

    pub fn deinit(self: *PackageRegistry) void {
        for (self.packages.items) |pkg| {
            pkg.deinit(self.allocator);
            self.allocator.destroy(pkg);
        }
        self.packages.deinit(self.allocator);
        self.class_to_package.deinit(self.allocator);
    }

    /// Initialize system packages
    pub fn initSystemPackages(self: *PackageRegistry) !void {
        for (SystemPackageDefinitions.getPackageNames()) |pkg_name| {
            const pkg = try self.createPackage(pkg_name);
            pkg.is_system = true;
        }
        // Also create Unpackaged for classes that don't fit elsewhere
        const unpackaged = try self.createPackage("Unpackaged");
        unpackaged.is_system = false; // User can modify this
    }

    pub fn createPackage(self: *PackageRegistry, name: []const u8) !*Package {
        // Check if package already exists
        for (self.packages.items) |pkg| {
            if (std.mem.eql(u8, pkg.name, name)) return pkg;
        }

        // Create new package
        const pkg = try self.allocator.create(Package);
        const name_copy = try self.allocator.dupe(u8, name);
        pkg.* = Package.init(self.allocator, name_copy);

        // Set default file path
        const file_name = try std.fmt.allocPrint(self.allocator, "{s}.st", .{name});
        pkg.file_path = file_name;

        try self.packages.append(self.allocator, pkg);
        return pkg;
    }

    pub fn getPackage(self: *PackageRegistry, name: []const u8) ?*Package {
        for (self.packages.items) |pkg| {
            if (std.mem.eql(u8, pkg.name, name)) return pkg;
        }
        return null;
    }

    pub fn removePackage(self: *PackageRegistry, name: []const u8) bool {
        var i: usize = 0;
        while (i < self.packages.items.len) {
            if (std.mem.eql(u8, self.packages.items[i].name, name)) {
                const pkg = self.packages.orderedRemove(i);
                // Remove all class mappings for this package
                var to_remove = std.ArrayList([]const u8).init(self.allocator);
                defer to_remove.deinit();
                var iter = self.class_to_package.iterator();
                while (iter.next()) |entry| {
                    if (entry.value_ptr.* == pkg) {
                        to_remove.append(entry.key_ptr.*) catch continue;
                    }
                }
                for (to_remove.items) |key| {
                    _ = self.class_to_package.remove(key);
                }
                pkg.deinit(self.allocator);
                self.allocator.destroy(pkg);
                return true;
            }
            i += 1;
        }
        return false;
    }

    pub fn addClassToPackage(self: *PackageRegistry, package_name: []const u8, class_name: []const u8) !void {
        const pkg = self.getPackage(package_name) orelse try self.createPackage(package_name);

        // Remove from old package if any
        if (self.class_to_package.get(class_name)) |old_pkg| {
            old_pkg.removeClass(class_name);
        }

        try pkg.addClass(class_name);
        try self.class_to_package.put(self.allocator, class_name, pkg);
    }

    /// Automatically categorize a class into the appropriate system package
    pub fn categorizeClass(self: *PackageRegistry, class_name: []const u8) !void {
        // Check if class matches a system package pattern
        if (SystemPackageDefinitions.getPackageForClass(class_name)) |pkg_name| {
            try self.addClassToPackage(pkg_name, class_name);
        } else {
            // Put in Unpackaged
            try self.addClassToPackage("Unpackaged", class_name);
        }
    }

    pub fn getPackageForClass(self: *PackageRegistry, class_name: []const u8) ?*Package {
        return self.class_to_package.get(class_name);
    }

    pub fn getPackageNames(self: *PackageRegistry) []const []const u8 {
        var names = self.allocator.alloc([]const u8, self.packages.items.len) catch return &[_][]const u8{};
        for (self.packages.items, 0..) |pkg, i| {
            names[i] = pkg.name;
        }
        return names;
    }

    /// File out a package to the appropriate directory
    pub fn fileOutPackage(self: *PackageRegistry, package: *Package, heap: *Heap) !void {
        const file_name = package.file_path orelse return error.NoFilePath;
        const base_dir = package.getBaseDir();

        // Ensure directory exists
        std.fs.cwd().makeDir(base_dir) catch |err| {
            if (err != error.PathAlreadyExists) return err;
        };

        // Build full path
        var path_buf: [256]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}", .{ base_dir, file_name }) catch return error.PathTooLong;

        // Open file for writing
        var file = std.fs.cwd().createFile(full_path, .{}) catch return error.IoError;
        defer file.close();

        var buf: [4096]u8 = undefined;

        // Write header comment
        const len = std.fmt.bufPrint(&buf, "\"Package: {s}\"\n", .{package.name}) catch return error.IoError;
        file.writeAll(len) catch return error.IoError;
        if (package.is_system) {
            file.writeAll("\"System Package - Generated by ZigSmalltalk\"\n\n") catch return error.IoError;
        } else {
            file.writeAll("\"User Package - Generated by ZigSmalltalk\"\n\n") catch return error.IoError;
        }

        // Write each class
        for (package.classes.items) |class_name| {
            self.fileOutClass(file, class_name, heap) catch {};
        }

        package.modified = false;
    }

    /// Load a package from file
    pub fn loadPackageFromFile(self: *PackageRegistry, package_name: []const u8, is_system: bool) !*Package {
        const base_dir = if (is_system) "system-packages" else "packages";
        var path_buf: [256]u8 = undefined;
        const full_path = std.fmt.bufPrint(&path_buf, "{s}/{s}.st", .{ base_dir, package_name }) catch return error.PathTooLong;

        // Check if file exists
        _ = std.fs.cwd().statFile(full_path) catch return error.FileNotFound;

        // Create or get the package
        const pkg = try self.createPackage(package_name);
        pkg.is_system = is_system;
        pkg.loaded = true;

        // Note: Actual file loading would require the FileIn module
        // For now, just mark as loaded
        return pkg;
    }

    fn fileOutClass(_: *PackageRegistry, file: std.fs.File, class_name: []const u8, heap: *Heap) !void {
        // Find class in heap
        const class_val = heap.globals.get(class_name) orelse return;
        if (!class_val.isObject()) return;

        const class_obj = class_val.asObject();
        if (class_obj.header.class_index == 0) return;

        // Get superclass name
        const super_val = class_obj.getField(Heap.CLASS_FIELD_SUPERCLASS, Heap.CLASS_NUM_FIELDS);
        var super_name: []const u8 = "Object";
        if (!super_val.isNil() and super_val.isObject()) {
            const super_obj = super_val.asObject();
            const super_name_val = super_obj.getField(Heap.CLASS_FIELD_NAME, Heap.CLASS_NUM_FIELDS);
            if (!super_name_val.isNil() and super_name_val.isObject()) {
                const super_name_obj = super_name_val.asObject();
                if (super_name_obj.header.class_index == Heap.CLASS_SYMBOL) {
                    super_name = super_name_obj.bytes(super_name_obj.header.size);
                }
            }
        }

        // Get instance variables
        const inst_vars_val = class_obj.getField(Heap.CLASS_FIELD_INST_VARS, Heap.CLASS_NUM_FIELDS);
        var inst_vars: []const u8 = "";
        if (!inst_vars_val.isNil() and inst_vars_val.isObject()) {
            const iv_obj = inst_vars_val.asObject();
            if (iv_obj.header.class_index == Heap.CLASS_STRING) {
                inst_vars = iv_obj.bytes(iv_obj.header.size);
            }
        }

        var buf: [2048]u8 = undefined;

        // Write class definition
        var len = std.fmt.bufPrint(&buf, "{s} subclass: #{s}\n", .{ super_name, class_name }) catch return;
        try file.writeAll(len);
        len = std.fmt.bufPrint(&buf, "\tinstanceVariableNames: '{s}'\n", .{inst_vars}) catch return;
        try file.writeAll(len);
        try file.writeAll("\tclassVariableNames: ''\n");
        try file.writeAll("\tpoolDictionaries: ''\n");
        len = std.fmt.bufPrint(&buf, "\tcategory: '{s}'!\n\n", .{class_name}) catch return;
        try file.writeAll(len);

        // Write instance methods
        fileOutMethodDict(file, class_obj, class_name, false, heap) catch {};

        // Get metaclass and write class methods
        const metaclass_val = class_obj.getField(0, Heap.CLASS_NUM_FIELDS); // Field 0 is usually the class pointer
        if (!metaclass_val.isNil() and metaclass_val.isObject()) {
            const metaclass = metaclass_val.asObject();
            // Check if this is actually a metaclass (has different method dict)
            const meta_methods = metaclass.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
            const inst_methods = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
            if (!meta_methods.isNil() and meta_methods.bits != inst_methods.bits) {
                fileOutMethodDict(file, metaclass, class_name, true, heap) catch {};
            }
        }
    }

    fn fileOutMethodDict(file: std.fs.File, class_obj: *@import("../vm/object.zig").Object, class_name: []const u8, is_class_side: bool, heap: *Heap) !void {
        _ = heap;
        const methods_val = class_obj.getField(Heap.CLASS_FIELD_METHOD_DICT, Heap.CLASS_NUM_FIELDS);
        if (methods_val.isNil() or !methods_val.isObject()) return;

        const dict = methods_val.asObject();
        if (dict.header.class_index != Heap.CLASS_DICTIONARY) return;

        const dict_size = dict.header.size;
        const fields = dict.fields(dict_size);

        var buf: [4096]u8 = undefined;

        // Dictionary stores associations at even indices
        var i: usize = 0;
        while (i < fields.len) : (i += 2) {
            if (i + 1 >= fields.len) break;

            const key_val = fields[i];
            const method_val = fields[i + 1];

            if (key_val.isNil() or method_val.isNil()) continue;
            if (!key_val.isObject() or !method_val.isObject()) continue;

            const key_obj = key_val.asObject();
            const method_obj = method_val.asObject();

            // Get selector name
            if (key_obj.header.class_index != Heap.CLASS_SYMBOL) continue;
            const selector = key_obj.bytes(key_obj.header.size);

            // Get method source
            const cm: *@import("../vm/object.zig").CompiledMethod = @ptrCast(@alignCast(method_obj));

            // Skip primitive methods without source
            if (cm.header.primitive_index != 0 and !cm.header.flags.has_source) continue;

            // Get source from method (if available)
            var source: ?[]const u8 = null;
            if (cm.header.flags.has_source) {
                const lits = cm.getLiterals();
                if (lits.len > 0) {
                    const source_val = lits[lits.len - 1];
                    if (source_val.isObject()) {
                        const source_obj = source_val.asObject();
                        if (source_obj.header.class_index == Heap.CLASS_STRING) {
                            source = source_obj.bytes(source_obj.header.size);
                        }
                    }
                }
            }

            // Write method
            var len: []const u8 = undefined;
            if (is_class_side) {
                len = std.fmt.bufPrint(&buf, "!{s} class methodsFor: 'as yet unclassified'!\n", .{class_name}) catch continue;
            } else {
                len = std.fmt.bufPrint(&buf, "!{s} methodsFor: 'as yet unclassified'!\n", .{class_name}) catch continue;
            }
            try file.writeAll(len);

            if (source) |src| {
                try file.writeAll(src);
            } else {
                // Reconstruct from selector
                len = std.fmt.bufPrint(&buf, "{s}\n\t\"No source available\"\n\t^ self", .{selector}) catch continue;
                try file.writeAll(len);
            }
            try file.writeAll("! !\n\n");
        }
    }
};
