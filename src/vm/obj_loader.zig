//! OBJ File Loader
//! Parses Wavefront OBJ files and returns vertex/index data for OpenGL

const std = @import("std");
const object = @import("object.zig");
const memory = @import("memory.zig");

const Value = object.Value;
const Heap = memory.Heap;

/// Position vector
const Vec3 = struct { x: f32, y: f32, z: f32 };

/// Texture coordinate
const Vec2 = struct { u: f32, v: f32 };

/// Vertex with position, normal, and texture coordinates
pub const Vertex = struct {
    x: f32,
    y: f32,
    z: f32,
    nx: f32,
    ny: f32,
    nz: f32,
    u: f32,
    v: f32,
};

/// Face index (vertex, texture, normal indices)
const FaceIndex = struct { v: i32, vt: i32, vn: i32 };

/// Loaded mesh data
pub const MeshData = struct {
    vertices: []Vertex,
    indices: []u32,
    allocator: std.mem.Allocator,

    pub fn deinit(self: *MeshData) void {
        self.allocator.free(self.vertices);
        self.allocator.free(self.indices);
    }
};

/// Parse a face index (handles v, v/vt, v/vt/vn, v//vn formats)
fn parseFaceIndex(token: []const u8) FaceIndex {
    var result = FaceIndex{ .v = 0, .vt = 0, .vn = 0 };
    var iter = std.mem.splitScalar(u8, token, '/');

    // Vertex index (required)
    if (iter.next()) |v_str| {
        if (v_str.len > 0) {
            result.v = std.fmt.parseInt(i32, v_str, 10) catch 0;
        }
    }

    // Texture coordinate index (optional)
    if (iter.next()) |vt_str| {
        if (vt_str.len > 0) {
            result.vt = std.fmt.parseInt(i32, vt_str, 10) catch 0;
        }
    }

    // Normal index (optional)
    if (iter.next()) |vn_str| {
        if (vn_str.len > 0) {
            result.vn = std.fmt.parseInt(i32, vn_str, 10) catch 0;
        }
    }

    return result;
}

/// Load an OBJ file and return mesh data
pub fn loadOBJ(path: []const u8, allocator: std.mem.Allocator) !MeshData {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.debug.print("Failed to open OBJ file: {s} - {}\n", .{ path, err });
        return err;
    };
    defer file.close();

    var positions: std.ArrayListUnmanaged(Vec3) = .{};
    defer positions.deinit(allocator);
    var normals: std.ArrayListUnmanaged(Vec3) = .{};
    defer normals.deinit(allocator);
    var texcoords: std.ArrayListUnmanaged(Vec2) = .{};
    defer texcoords.deinit(allocator);

    var vertices: std.ArrayListUnmanaged(Vertex) = .{};
    defer vertices.deinit(allocator);
    var indices: std.ArrayListUnmanaged(u32) = .{};
    defer indices.deinit(allocator);

    // Map from (v,vt,vn) to vertex index for deduplication
    var vertex_map: std.AutoHashMapUnmanaged(u64, u32) = .{};
    defer vertex_map.deinit(allocator);

    // Read entire file
    const content = file.readToEndAlloc(allocator, 100 * 1024 * 1024) catch |err| {
        std.debug.print("Failed to read OBJ file: {s} - {}\n", .{ path, err });
        return err;
    };
    defer allocator.free(content);

    var line_count: u32 = 0;
    var lines = std.mem.splitScalar(u8, content, '\n');

    while (lines.next()) |line| {
        line_count += 1;

        // Trim whitespace and carriage return
        const trimmed = std.mem.trim(u8, line, " \t\r");
        if (trimmed.len == 0 or trimmed[0] == '#') continue;

        var tokens = std.mem.tokenizeAny(u8, trimmed, " \t");
        const cmd = tokens.next() orelse continue;

        if (std.mem.eql(u8, cmd, "v")) {
            // Vertex position
            const px = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            const py = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            const pz = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            try positions.append(allocator, .{ .x = px, .y = py, .z = pz });
        } else if (std.mem.eql(u8, cmd, "vn")) {
            // Vertex normal
            const nx = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            const ny = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            const nz = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            try normals.append(allocator, .{ .x = nx, .y = ny, .z = nz });
        } else if (std.mem.eql(u8, cmd, "vt")) {
            // Texture coordinate
            const tu = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            const tv = std.fmt.parseFloat(f32, tokens.next() orelse "0") catch 0;
            try texcoords.append(allocator, .{ .u = tu, .v = tv });
        } else if (std.mem.eql(u8, cmd, "f")) {
            // Face - collect all vertex indices
            var face_indices: [8]FaceIndex = undefined;
            var face_count: usize = 0;

            while (tokens.next()) |token| {
                if (face_count >= 8) break;
                face_indices[face_count] = parseFaceIndex(token);
                face_count += 1;
            }

            // Triangulate the face (fan triangulation)
            if (face_count >= 3) {
                var i: usize = 1;
                while (i < face_count - 1) : (i += 1) {
                    const tri_indices = [3]usize{ 0, i, i + 1 };

                    for (tri_indices) |fi| {
                        const face_idx = face_indices[fi];

                        // Handle negative indices (relative to end)
                        var v_idx = face_idx.v;
                        var vt_idx = face_idx.vt;
                        var vn_idx = face_idx.vn;

                        if (v_idx < 0) v_idx = @as(i32, @intCast(positions.items.len)) + v_idx + 1;
                        if (vt_idx < 0) vt_idx = @as(i32, @intCast(texcoords.items.len)) + vt_idx + 1;
                        if (vn_idx < 0) vn_idx = @as(i32, @intCast(normals.items.len)) + vn_idx + 1;

                        // Create unique key for this vertex combination
                        const key: u64 = (@as(u64, @intCast(@as(u32, @bitCast(v_idx)))) << 40) |
                            (@as(u64, @intCast(@as(u32, @bitCast(vt_idx)))) << 20) |
                            @as(u64, @intCast(@as(u32, @bitCast(vn_idx))));

                        if (vertex_map.get(key)) |existing_idx| {
                            try indices.append(allocator, existing_idx);
                        } else {
                            // Create new vertex
                            var vertex = Vertex{
                                .x = 0, .y = 0, .z = 0,
                                .nx = 0, .ny = 1, .nz = 0,
                                .u = 0, .v = 0,
                            };

                            if (v_idx > 0 and v_idx <= @as(i32, @intCast(positions.items.len))) {
                                const pos = positions.items[@intCast(v_idx - 1)];
                                vertex.x = pos.x;
                                vertex.y = pos.y;
                                vertex.z = pos.z;
                            }

                            if (vn_idx > 0 and vn_idx <= @as(i32, @intCast(normals.items.len))) {
                                const norm = normals.items[@intCast(vn_idx - 1)];
                                vertex.nx = norm.x;
                                vertex.ny = norm.y;
                                vertex.nz = norm.z;
                            }

                            if (vt_idx > 0 and vt_idx <= @as(i32, @intCast(texcoords.items.len))) {
                                const tc = texcoords.items[@intCast(vt_idx - 1)];
                                vertex.u = tc.u;
                                vertex.v = 1.0 - tc.v; // Flip V for OpenGL (origin at bottom-left)
                            }

                            const new_idx: u32 = @intCast(vertices.items.len);
                            try vertices.append(allocator, vertex);
                            try indices.append(allocator, new_idx);
                            try vertex_map.put(allocator, key, new_idx);
                        }
                    }
                }
            }
        }
    }

    std.debug.print("OBJ loaded: {d} vertices, {d} indices from {d} lines\n", .{
        vertices.items.len,
        indices.items.len,
        line_count,
    });

    return MeshData{
        .vertices = try vertices.toOwnedSlice(allocator),
        .indices = try indices.toOwnedSlice(allocator),
        .allocator = allocator,
    };
}

/// Create ByteArray containing vertex data (8 floats per vertex: x,y,z,nx,ny,nz,u,v)
pub fn meshToByteArrays(mesh: *const MeshData, heap: *Heap) !struct { vertices: Value, indices: Value } {
    // Vertex data: 8 floats * 4 bytes = 32 bytes per vertex
    const vertex_bytes = mesh.vertices.len * 32;
    const vertex_arr = try heap.allocateByteArray(vertex_bytes);
    const vertex_data = vertex_arr.asObject().bytes(vertex_bytes);

    for (mesh.vertices, 0..) |v, i| {
        const offset = i * 32;
        std.mem.writeInt(u32, vertex_data[offset..][0..4], @bitCast(v.x), .little);
        std.mem.writeInt(u32, vertex_data[offset + 4 ..][0..4], @bitCast(v.y), .little);
        std.mem.writeInt(u32, vertex_data[offset + 8 ..][0..4], @bitCast(v.z), .little);
        std.mem.writeInt(u32, vertex_data[offset + 12 ..][0..4], @bitCast(v.nx), .little);
        std.mem.writeInt(u32, vertex_data[offset + 16 ..][0..4], @bitCast(v.ny), .little);
        std.mem.writeInt(u32, vertex_data[offset + 20 ..][0..4], @bitCast(v.nz), .little);
        std.mem.writeInt(u32, vertex_data[offset + 24 ..][0..4], @bitCast(v.u), .little);
        std.mem.writeInt(u32, vertex_data[offset + 28 ..][0..4], @bitCast(v.v), .little);
    }

    // Index data: 4 bytes per index
    const index_bytes = mesh.indices.len * 4;
    const index_arr = try heap.allocateByteArray(index_bytes);
    const index_data = index_arr.asObject().bytes(index_bytes);

    for (mesh.indices, 0..) |idx, i| {
        std.mem.writeInt(u32, index_data[i * 4 ..][0..4], idx, .little);
    }

    return .{ .vertices = vertex_arr, .indices = index_arr };
}
