const std = @import("std");

// Custom PeekableStream that supports putBack functionality
pub fn PeekableStream(comptime ReaderType: type) type {
    return struct {
        const Self = @This();

        underlying_reader: ReaderType,
        peek_buffer: [10]?u8 = [_]?u8{null} ** 10, // Buffer for peeked bytes
        peek_buffer_size: usize = 0,
        peek_start: usize = 0, // Start index for circular buffer

        pub fn init(underlying_reader_param: ReaderType) Self {
            return Self{ .underlying_reader = underlying_reader_param };
        }

        pub fn readByte(self: *Self) LispError!u8 {
            // Check if we have peeked bytes available (read from start)
            if (self.peek_buffer_size > 0) {
                const byte = self.peek_buffer[self.peek_start];
                self.peek_start = (self.peek_start + 1) % self.peek_buffer.len;
                self.peek_buffer_size -= 1;
                return byte.?;
            }

            // Otherwise read from the underlying reader
            var byte: [1]u8 = undefined;
            const n = self.underlying_reader.read(&byte) catch |err| {
                switch (err) {
                    error.EndOfStream => return error.EndOfStream,
                    else => return error.NotOpenForReading,
                }
            };
            if (n == 0) return error.EndOfStream;
            return byte[0];
        }

        pub fn putBackByte(self: *Self, byte: u8) void {
            if (self.peek_buffer_size < self.peek_buffer.len) {
                self.peek_start = (self.peek_start + self.peek_buffer.len - 1) % self.peek_buffer.len;
                self.peek_buffer[self.peek_start] = byte;
                self.peek_buffer_size += 1;
            }
        }

        pub fn putBack(self: *Self, bytes: []const u8) void {
            // Put back multiple bytes in reverse order to maintain proper sequence
            var i: usize = bytes.len;
            while (i > 0) {
                i -= 1;
                self.putBackByte(bytes[i]);
            }
        }

        pub fn skipUntilDelimiterOrEof(self: *Self, delimiter: u8) LispError!void {
            while (true) {
                const byte = self.readByte() catch |err| {
                    if (err == error.EndOfStream) return;
                    return err;
                };
                if (byte == delimiter) break;
            }
        }

        pub fn reader(self: *Self) *Self {
            return self;
        }
    };
}

const cell = struct {
    car: ?*atom,
    cdr: ?*atom,
};

const lambda = struct {
    e: ?*env,
    cell: cell,
};

const ref = ?*atom;

const function = struct {
    name: []const u8,
    ptr: *const fn (*env, std.mem.Allocator, *atom) LispError!*atom,
};

const env = struct {
    a: std.mem.Allocator,
    v: std.StringArrayHashMap(*atom),
    p: ?*env,
    err: ?[]const u8,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) Self {
        return Self{
            .a = a,
            .v = std.StringArrayHashMap(*atom).init(a),
            .p = null,
            .err = null,
        };
    }

    pub fn get(self: *Self, key: []const u8) !?*atom {
        var e: *env = self;
        while (true) {
            if (e.v.get(key)) |ev| {
                return ev;
            }
            if (e.p == null) {
                break;
            }
            e = e.p.?;
        }
        try e.raise("invalid symbol");
        unreachable;
    }

    pub fn child(self: *Self) Self {
        const c = Self{
            .a = self.a,
            .v = std.StringArrayHashMap(*atom).init(self.a),
            .p = self,
            .err = null,
        };
        return c;
    }

    pub fn deinit(self: *Self) void {
        // Free all atoms and keys stored in this environment
        var it = self.v.iterator();
        while (it.next()) |entry| {
            entry.value_ptr.*.deinit(self.a, true);
            self.a.free(entry.key_ptr.*);
        }
        self.v.clearAndFree();
        self.v.deinit();
        if (self.err != null) {
            self.a.free(self.err.?);
        }
    }

    pub fn raise(self: *Self, msg: []const u8) LispError!void {
        self.err = try self.a.dupe(u8, msg);
        return error.RuntimeError;
    }

    pub fn printerr(self: *Self, err: anyerror) !void {
        if (self.err != null) {
            std.debug.print("{}: {s}\n", .{ err, self.err.? });
            self.err = null;
        } else {
            std.debug.print("{}\n", .{err});
        }
    }
};

const atom = union(enum) {
    sym: []u8,
    bool: bool,
    num: i64,
    str: []u8,
    lambda: lambda,
    func: *const function,
    quote: ?*atom,
    cell: cell,
    none: ?void,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) !*atom {
        return try a.create(atom);
    }

    pub fn copy(self: *Self, a: std.mem.Allocator) !*Self {
        const n = try atom.init(a);
        n.* = switch (self.*) {
            .sym => |v| atom{ .sym = try a.dupe(u8, v) },
            .str => |v| atom{ .str = try a.dupe(u8, v) },
            .cell => |v| atom{
                .cell = cell{
                    .car = if (v.car) |car| try car.copy(a) else null,
                    .cdr = if (v.cdr) |cdr| try cdr.copy(a) else null,
                },
            },
            .quote => |v| atom{
                .quote = if (v) |val| try val.copy(a) else null,
            },
            .lambda => |v| atom{
                .lambda = lambda{
                    .e = v.e,
                    .cell = cell{
                        .car = if (v.cell.car) |car| try car.copy(a) else null,
                        .cdr = if (v.cell.cdr) |cdr| try cdr.copy(a) else null,
                    },
                },
            },
            else => self.*,
        };
        return n;
    }

    pub fn deinit(self: *Self, a: std.mem.Allocator, final: bool) void {
        switch (self.*) {
            .sym => |v| {
                if (final) {
                    a.free(v);
                }
            },
            .str => |v| {
                if (final) {
                    a.free(v);
                }
            },
            .lambda => |v| {
                if (!final) {
                    return;
                }
                if (v.cell.car != null) {
                    v.cell.car.?.deinit(a, final);
                }
                if (v.cell.cdr != null) {
                    v.cell.cdr.?.deinit(a, final);
                }
            },
            .cell => |v| {
                if (!final) {
                    return;
                }
                if (v.car != null) {
                    v.car.?.deinit(a, final);
                }
                if (v.cdr != null) {
                    v.cdr.?.deinit(a, final);
                }
            },
            .quote => |v| {
                if (final) {
                    v.?.deinit(a, true);
                }
            },
            .bool => {},
            .num => {},
            .func => {},
            .none => {},
        }
        if (final) {
            a.destroy(self);
        }
    }

    pub fn println(self: @This(), w: anytype, quoted: bool) LispError!void {
        try self.print(w, quoted);
        _ = try w.write(&[_]u8{'\n'});
    }

    pub fn print(self: @This(), w: anytype, quoted: bool) LispError!void {
        _ = try w.write(&[_]u8{'\n'});
        try self.princ(w, quoted);
    }

    pub fn princ(self: @This(), w: anytype, quoted: bool) LispError!void {
        switch (self) {
            .none => _ = try w.write("null"),
            .sym => |v| _ = try w.write(v),
            .str => |v| {
                if (quoted) {
                    _ = try w.write("\"");
                    for (v) |c| {
                        switch (c) {
                            '\\' => _ = try w.write("\\\\"),
                            '"' => _ = try w.write("\\\""),
                            '\n' => _ = try w.write("\\n"),
                            '\r' => _ = try w.write("\\r"),
                            else => _ = try w.write(&[_]u8{c}),
                        }
                    }
                    _ = try w.write("\"");
                } else {
                    _ = try w.write(v);
                }
            },
            .func => |v| _ = try w.write(v.name),
            .bool => |v| {
                if (v) {
                    _ = try w.write("T");
                } else {
                    _ = try w.write("nil");
                }
            },
            .num => |v| {
                var num_buffer: [32]u8 = undefined;
                const num_str = try std.fmt.bufPrint(&num_buffer, "{}", .{v});
                _ = try w.write(num_str);
            },
            .lambda => |v| {
                _ = try w.write("(lambda ");
                try v.cell.cdr.?.cell.car.?.cell.cdr.?.princ(w, quoted);
                _ = try w.write(" ");
                try v.cell.cdr.?.cell.car.?.princ(w, quoted);
                _ = try w.write(")");
            },
            .cell => |v| {
                _ = try w.write("(");
                try v.car.?.princ(w, false);
                _ = try w.write(" ");
                if (v.cdr == null) {
                    return;
                }
                var a = v.cdr;
                while (a != null) {
                    if (a.?.cell.car == null)
                        break;
                    try a.?.cell.car.?.princ(w, quoted);
                    if (a.?.cell.cdr == null) {
                        break;
                    }
                    a = a.?.cell.cdr;
                    if (a == null) {
                        break;
                    }
                    _ = try w.write(" ");
                }
                _ = try w.write(")");
            },
            .quote => |v| {
                _ = try w.write("\x27");
                try v.?.princ(w, quoted);
            },
        }
    }
};

fn debug(arg: *atom) !void {
    try arg.println(std.io.getStdOut().writer(), false);
}

fn eval(e: *env, a: std.mem.Allocator, root: *atom) LispError!*atom {
    var arg: ?*atom = root;

    return switch (arg.?.*) {
        atom.sym => |v| blk: {
            var p = e;
            while (true) {
                if (p.v.get(v)) |ev| {
                    break :blk try eval(e, a, ev);
                }
                if (p.p == null) {
                    break;
                }
                p = p.p.?;
            }
            try e.raise("invalid symbol");
            break :blk root;
        },
        atom.str => |v| blk: {
            const str_slice = try a.dupe(u8, v);
            const na = try atom.init(a);
            na.* = atom{
                .str = str_slice,
            };
            break :blk na;
        },
        atom.lambda => try arg.?.copy(a),
        atom.cell => blk: {
            var last = arg.?;
            while (true) {
                last = try switch (arg.?.cell.car.?.*) {
                    atom.func => (arg.?.cell.car.?.func.ptr)(e, a, arg.?.cell.cdr.?),
                    atom.lambda => {
                        var newe = e.child();
                        defer newe.deinit();
                        newe.p = arg.?.cell.car.?.lambda.e.?;
                        var pa = arg.?.cell.car.?.lambda.cell.car;
                        var fa = arg.?.cell.cdr;
                        while (pa != null) {
                            const key = try a.dupe(u8, pa.?.cell.car.?.sym);
                            try newe.v.put(
                                key,
                                try eval(e, a, fa.?.cell.car.?),
                            );
                            pa = pa.?.cell.cdr;
                            fa = fa.?.cell.cdr;
                        }
                        break :blk eval(&newe, a, arg.?.cell.car.?.lambda.cell.cdr.?);
                    },
                    atom.sym => {
                        const funcname = arg.?.cell.car.?.sym;
                        for (builtins, 0..) |b, i| {
                            if (std.mem.eql(u8, b.name, funcname)) {
                                break :blk (builtins[i].ptr)(e, a, arg.?.cell.cdr.?);
                            }
                        }
                        if (try e.get(funcname)) |f| {
                            if (f.cell.cdr.?.* == atom.cell) {
                                var newe = e.child();
                                defer newe.deinit();
                                newe.p = e;
                                if (f.cell.cdr != null) {
                                    var pa = f.cell.cdr.?.cell.car;
                                    var fa = arg.?.cell.cdr;
                                    while (pa != null and fa != null) {
                                        const key = try a.dupe(u8, pa.?.cell.car.?.sym);
                                        try newe.v.put(
                                            key,
                                            try eval(e, a, fa.?.cell.car.?),
                                        );
                                        pa = pa.?.cell.cdr;
                                        fa = fa.?.cell.cdr;
                                    }
                                }
                                break :blk eval(&newe, a, f.cell.cdr.?.cell.cdr.?.cell.car.?);
                            }
                        }
                        try e.raise("invalid function");
                        break :blk undefined;
                    },
                    else => eval(e, a, arg.?.cell.car.?),
                };
                arg = arg.?.cell.cdr;
                if (arg == null) {
                    break :blk last;
                }
            }
            unreachable;
        },
        atom.quote => |v| try v.?.copy(a),
        atom.bool => try arg.?.copy(a),
        atom.num => try arg.?.copy(a),
        atom.func => try arg.?.copy(a),
        atom.none => try arg.?.copy(a),
    };
}

pub fn do_add(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num: i64 = 0;
    while (true) {
        var val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, true);
        if (val.* == atom.num) {
            num += val.num;
        } else {
            try e.raise("invalid type for +");
        }
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .num = num,
            };
            return na;
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_sub(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var val = try eval(e, a, arg.cell.car.?);
    if (val.* != atom.num) {
        try e.raise("invalid type for -");
    }
    var num: i64 = val.num;
    val.deinit(a, false);
    if (arg.cell.cdr == null) {
        const na = try atom.init(a);
        na.* = atom{
            .num = num,
        };
        return na;
    }
    while (true) {
        arg = arg.cell.cdr.?;
        val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, true);
        if (val.* == atom.num) {
            num -= val.num;
        } else {
            try e.raise("invalid type for -");
        }
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .num = num,
            };
            return na;
        }
    }
    unreachable;
}

pub fn do_mat(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num: i64 = 1;
    while (true) {
        var val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, true);
        if (val.* == atom.num) {
            num *= val.num;
        } else {
            try e.raise("invalid type for *");
        }
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .num = num,
            };
            return na;
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_mul(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var val = try eval(e, a, arg.cell.car.?);
    defer val.deinit(a, true);
    if (val.* != atom.num) {
        try e.raise("invalid type for /");
    }
    var num: i64 = val.num;
    if (arg.cell.cdr == null) {
        const na = try atom.init(a);
        na.* = atom{
            .num = num,
        };
        return na;
    }
    while (true) {
        arg = arg.cell.cdr.?;
        val = try eval(e, a, arg.cell.car.?);
        if (val.* == atom.num) {
            num = @divTrunc(num, val.num);
        } else {
            try e.raise("invalid type for /");
        }
        val.deinit(a, false);
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .num = num,
            };
            return na;
        }
    }
    unreachable;
}

pub fn do_lt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for <");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for <");
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num < rhs.num,
    };
    return na;
}

pub fn do_gt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for >");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for >");
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num > rhs.num,
    };
    return na;
}

pub fn do_eq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for =");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for =");
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num == rhs.num,
    };
    return na;
}

pub fn do_le(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for <=");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for <=");
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num <= rhs.num,
    };
    return na;
}

pub fn do_ge(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for >=");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for >=");
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num >= rhs.num,
    };
    return na;
}

pub fn do_1minus(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args.cell.car.?);
    defer arg.deinit(a, true);
    if (arg.* != atom.num) {
        try e.raise("invalid type for 1-");
    }
    const na = try atom.init(a);
    na.* = atom{
        .num = arg.num - 1,
    };
    return na;
}

pub fn do_1plus(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args.cell.car.?);
    defer arg.deinit(a, true);
    if (arg.* != atom.num) {
        try e.raise("invalid type for 1+");
    }
    const na = try atom.init(a);
    na.* = atom{
        .num = arg.num + 1,
    };
    return na;
}

pub fn do_mod(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, true);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for mod");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, true);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for mod");
    }
    const na = try atom.init(a);
    na.* = atom{
        .num = @mod(lhs.num, rhs.num),
    };
    return na;
}

pub fn do_cond(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;

    while (true) {
        var cond = try eval(e, a, arg.cell.car.?.cell.car.?);
        if (cond.* != atom.bool) {
            try e.raise("invalid type for cond");
        }
        defer cond.deinit(a, true);

        if (cond.bool) {
            return try eval(e, a, arg.cell.car.?.cell.cdr.?.cell.car.?);
        }
        arg = arg.cell.cdr.?;
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = false,
    };
    return na;
}

pub fn do_dotimes(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const arg = args;

    const name = arg.cell.car.?.cell.car;
    const count = try eval(e, a, arg.cell.car.?.cell.cdr.?);
    if (count.* != atom.num) {
        try e.raise("invalid type for dotimes");
    }
    var newe = e.child();
    defer newe.deinit();

    const key = try a.dupe(u8, name.?.sym);
    var i: u32 = 0;
    while (i < count.num) : (i += 1) {
        // Check if key already exists and free old value
        if (newe.v.get(key)) |old_val| {
            old_val.deinit(a, true);
        }
        const nv = try atom.init(a);
        nv.* = atom{
            .num = @intCast(i),
        };
        try newe.v.put(key, nv);
        // Evaluate all forms in the body
        var body = arg.cell.cdr;
        while (body != null) {
            const value = try eval(&newe, a, body.?.cell.car.?);
            freeResult(value, &newe, a);
            body = body.?.cell.cdr;
        }
    }
    const na = try atom.init(a);
    na.* = atom{
        .bool = false,
    };
    return na;
}

pub fn do_if(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var cond = try eval(e, a, arg.cell.car.?);
    defer cond.deinit(a, true);
    if (cond.* != atom.bool) {
        try e.raise("invalid type for if");
    }

    arg = arg.cell.cdr.?;
    if (cond.bool) {
        return try eval(e, a, arg.cell.car.?);
    }
    arg = arg.cell.cdr.?;
    return try eval(e, a, arg.cell.car.?);
}

pub fn do_setq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const arg = args;
    const val = try eval(e, a, arg.cell.cdr.?.cell.car.?);
    const name = arg.cell.car.?;
    const key = try a.dupe(u8, name.sym);
    try e.v.put(key, val);
    return val;
}

pub fn do_defun(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const name = args.cell.car.?;
    const key = try a.dupe(u8, name.sym);
    try e.v.put(key, args);
    const sym_slice = try a.dupe(u8, name.sym);
    const p = try atom.init(a);
    p.* = atom{
        .sym = sym_slice,
    };
    return p;
}

pub fn do_princ(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const result = try eval(e, a, args.cell.car.?);
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_stream = std.io.fixedBufferStream(&stdout_buffer);
    try result.princ(stdout_stream.writer(), false);
    const written = stdout_stream.getWritten();
    _ = try std.fs.File.stdout().writeAll(written);
    return result;
}

pub fn do_print(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const result = try eval(e, a, args.cell.car.?);
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_stream = std.io.fixedBufferStream(&stdout_buffer);
    try result.println(stdout_stream.writer(), false);
    const written = stdout_stream.getWritten();
    _ = try std.fs.File.stdout().writeAll(written);
    return result;
}

pub fn do_concatenate(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var total_len: usize = 0;

    // First pass: calculate total length needed
    var temp_arg = args;
    while (true) {
        var val = try eval(e, a, temp_arg.cell.car.?);
        defer val.deinit(a, true);
        if (val.* == atom.str) {
            total_len += val.str.len;
        } else {
            try e.raise("invalid type for concatenate");
        }
        if (temp_arg.cell.cdr == null) {
            break;
        }
        temp_arg = temp_arg.cell.cdr.?;
    }

    // Second pass: build the result
    const result_slice = try a.alloc(u8, total_len);
    var pos: usize = 0;

    while (true) {
        var val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, true);
        if (val.* == atom.str) {
            @memcpy(result_slice[pos..][0..val.str.len], val.str);
            pos += val.str.len;
        } else {
            a.free(result_slice);
            try e.raise("invalid type for concatenate");
        }
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .str = result_slice,
            };
            return na;
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_funcall(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const l = try eval(e, a, args.cell.car.?);
    var p = try atom.init(a);
    p.* = atom{
        .cell = cell{
            .car = l,
            .cdr = args.cell.cdr,
        },
    };
    defer p.deinit(a, true);
    return try eval(e, a, p);
}

pub fn do_lambda(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const p = try atom.init(a);
    p.* = atom{
        .lambda = .{
            .e = e,
            .cell = args.cell,
        },
    };
    return p;
}

pub fn do_length(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args);
    const first_arg = arg;
    defer first_arg.deinit(a, true);
    var n: i64 = 0;
    while (true) {
        n += 1;
        if (arg.cell.cdr == null) {
            const na = try atom.init(a);
            na.* = atom{
                .num = n,
            };
            return na;
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

var builtins = [_]function{
    .{ .name = "+", .ptr = &do_add },
    .{ .name = "-", .ptr = &do_sub },
    .{ .name = "*", .ptr = &do_mat },
    .{ .name = "/", .ptr = &do_mul },
    .{ .name = "<", .ptr = &do_lt },
    .{ .name = ">", .ptr = &do_gt },
    .{ .name = "=", .ptr = &do_eq },
    .{ .name = "<=", .ptr = &do_le },
    .{ .name = ">=", .ptr = &do_ge },
    .{ .name = "1-", .ptr = &do_1minus },
    .{ .name = "1+", .ptr = &do_1plus },
    .{ .name = "mod", .ptr = &do_mod },
    .{ .name = "cond", .ptr = &do_cond },
    .{ .name = "dotimes", .ptr = &do_dotimes },
    .{ .name = "if", .ptr = &do_if },
    .{ .name = "princ", .ptr = &do_princ },
    .{ .name = "print", .ptr = &do_print },
    .{ .name = "setq", .ptr = &do_setq },
    .{ .name = "defun", .ptr = &do_defun },
    .{ .name = "length", .ptr = &do_length },
    .{ .name = "lambda", .ptr = &do_lambda },
    .{ .name = "funcall", .ptr = &do_funcall },
    .{ .name = "concatenate", .ptr = &do_concatenate },
};

const SyntaxError = error{};
const RuntimeError = error{};
const ParseIntError = std.fmt.ParseIntError;
const WriteError = std.fs.File.WriteError;
const LispError = error{ RuntimeError, SyntaxError, OutOfMemory, EndOfStream, NoError, InvalidCharacter, IsDir, ConnectionTimedOut, NotOpenForReading, SocketNotConnected, NetNameDeleted } || ParseIntError || WriteError;

fn skipWhilte(br: anytype) LispError!void {
    loop: while (true) {
        const byte = br.readByte() catch |err| {
            if (err == error.EndOfStream) {
                break :loop;
            }
            return err;
        };
        switch (byte) {
            ' ', '\t', '\r', '\n' => {},
            else => |v| {
                if (v != 0) br.putBackByte(v);
                break :loop;
            },
        }
    }
}

fn parseString(a: std.mem.Allocator, br: anytype) LispError!*atom {
    var byte = try br.readByte();
    if (byte != '"') return error.SyntaxError;

    // Build string manually without ArrayList
    var buffer: [256]u8 = undefined;
    var pos: usize = 0;

    while (true) {
        byte = try br.readByte();
        if (byte == '\\') {
            byte = switch (br.readByte() catch 0) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                else => byte,
            };
        } else if (byte == '"') {
            break;
        }

        // Resize buffer if needed - using a different approach
        if (pos >= buffer.len) {
            // For this case, just stop reading to avoid buffer overflow
            break;
        }

        buffer[pos] = byte;
        pos += 1;
    }

    // Create slice from the built string
    const str_slice = try a.dupe(u8, buffer[0..pos]);

    const p = try atom.init(a);
    p.* = atom{
        .str = str_slice,
    };
    return p;
}

fn parseIdent(a: std.mem.Allocator, br: anytype) LispError!*atom {
    // Build string manually without ArrayList
    var buffer: [64]u8 = undefined;
    var pos: usize = 0;

    loop: while (true) {
        const byte = br.readByte() catch |err| {
            if (err == error.EndOfStream) break :loop;
            return err;
        };
        switch (byte) {
            'a'...'z', '0'...'9', '-', '+', '>', '<', '=' => |v| {
                // Resize buffer if needed - using a different approach
                if (pos >= buffer.len) {
                    // For this case, just stop reading to avoid buffer overflow
                    break;
                }
                buffer[pos] = v;
                pos += 1;
            },
            else => |v| {
                if (v != 0) br.putBackByte(v);
                break :loop;
            },
        }
    }

    const sym_slice = try a.dupe(u8, buffer[0..pos]);

    const p = try atom.init(a);
    p.* = atom{
        .sym = sym_slice,
    };
    return p;
}

fn parseQuote(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const byte = try br.readByte();
    if (byte != '\x27') return error.SyntaxError;

    const c = try parse(a, br);
    const p = try atom.init(a);
    p.* = atom{ .quote = c };
    return p;
}

fn parseCell(a: std.mem.Allocator, br: anytype) LispError!*atom {
    var byte = try br.readByte();
    if (byte != '(') return error.SyntaxError;

    const top = try atom.init(a);
    top.* = atom{
        .cell = cell{
            .car = null,
            .cdr = null,
        },
    };
    var p = top;

    while (true) {
        p.cell.car = try parse(a, br);

        try skipWhilte(br);
        byte = try br.readByte();
        if (byte == ')') {
            break;
        }
        br.putBackByte(byte);

        const cdr = try atom.init(a);
        cdr.* = atom{
            .cell = cell{
                .car = null,
                .cdr = null,
            },
        };
        p.cell.cdr = cdr;
        p = p.cell.cdr.?;
    }
    return top;
}

fn parseNumber(a: std.mem.Allocator, br: anytype) LispError!*atom {
    // Build string manually without ArrayList
    var buffer: [32]u8 = undefined;
    var pos: usize = 0;

    loop: while (true) {
        const byte = br.readByte() catch |err| {
            if (err == error.EndOfStream) break :loop;
            return err;
        };
        switch (byte) {
            '0'...'9', '-', '+', 'e' => |v| {
                if (pos >= buffer.len) break :loop; // Prevent buffer overflow
                buffer[pos] = v;
                pos += 1;
            },
            else => |v| {
                if (v != 0) br.putBackByte(v);
                break :loop;
            },
        }
    }

    if (std.fmt.parseInt(i64, buffer[0..pos], 10)) |num| {
        const p = try atom.init(a);
        p.* = atom{
            .num = num,
        };
        return p;
    } else |_| {
        br.putBack(buffer[0..pos]);
        return parseIdent(a, br);
    }
}

fn parse(a: std.mem.Allocator, br: anytype) LispError!*atom {
    try skipWhilte(br);
    var byte = try br.readByte();
    br.putBackByte(byte);
    while (byte == ';') {
        try br.skipUntilDelimiterOrEof('\n');
        byte = try br.readByte();
        br.putBackByte(byte);
    }
    if (byte == ')') {
        const na = try atom.init(a);
        na.* = atom{
            .cell = cell{
                .car = null,
                .cdr = null,
            },
        };
        return na;
    }

    return switch (byte) {
        '(' => try parseCell(a, br),
        '0'...'9', '-', '+' => try parseNumber(a, br),
        'a'...'z', '>', '<', '=' => try parseIdent(a, br),
        '\x27' => try parseQuote(a, br),
        '"' => try parseString(a, br),
        else => error.SyntaxError,
    };
}

// fn reader(r: anytype) bufReader(@TypeOf(r)) {
//     return std.io.peekStream(2, r);
// }
//
// fn bufReader(comptime r: anytype) type {
//     return std.io.PeekStream(std.fifo.LinearFifoBufferType{ .Static = 2 }, r);
// }

// Helper function to safely free a result atom that might be in env
fn freeResult(result: *atom, e: *env, a: std.mem.Allocator) void {
    var result_in_env = false;
    var it = e.v.iterator();
    while (it.next()) |entry| {
        if (entry.value_ptr.* == result) {
            result_in_env = true;
            break;
        }
    }
    if (!result_in_env) {
        result.deinit(a, true);
    }
}

// Helper function to safely free a root atom that might have children in env
fn freeRoot(root: *atom, e: *env, a: std.mem.Allocator) void {
    var root_or_children_in_env = false;
    var it = e.v.iterator();
    while (it.next()) |entry| {
        const env_atom = entry.value_ptr.*;
        if (env_atom == root) {
            root_or_children_in_env = true;
            break;
        }
        // Check if root's children are in env (for defun case)
        if (root.* == .cell) {
            if (root.cell.cdr == env_atom or root.cell.car == env_atom) {
                root_or_children_in_env = true;
                break;
            }
        }
    }
    
    if (!root_or_children_in_env) {
        root.deinit(a, true);
    } else {
        // If root's children are in env, detach them before freeing root
        if (root.* == .cell) {
            var it2 = e.v.iterator();
            while (it2.next()) |entry| {
                const env_atom = entry.value_ptr.*;
                if (root.cell.cdr == env_atom) {
                    root.cell.cdr = null;
                }
                if (root.cell.car == env_atom) {
                    root.cell.car = null;
                }
            }
        }
        root.deinit(a, true);
    }
}

fn run(a: std.mem.Allocator, reader: anytype, repl: bool) !void {
    var stream = PeekableStream(@TypeOf(reader)).init(reader);
    const br = &stream;
    var e = env.init(a);
    defer e.deinit();

    const t = try atom.init(a);
    t.* = atom{
        .bool = true,
    };
    const key_t = try a.dupe(u8, "t");
    try e.v.put(key_t, t);

    // Using simple counters instead of ArrayList for compatibility with Zig version
    var gcValueCount: usize = 0;
    var gcASTCount: usize = 0;
    // Note: Skipping garbage collection lists for now due to generic type issues in this Zig version
    while (true) {
        if (repl and std.fs.File.stderr().isTty()) {
            std.debug.print("> ", .{});
        }
        if (parse(a, br)) |root| {
            if (eval(&e, a, root)) |result| {
                // Placeholder - skipping append due to Zig version compatibility
                gcValueCount += 1;
                // Output result for REPL
                var stdout_buffer: [1024]u8 = undefined;
                var stdout_stream = std.io.fixedBufferStream(&stdout_buffer);
                try result.println(stdout_stream.writer(), false);
                const written = stdout_stream.getWritten();
                _ = try std.fs.File.stdout().writeAll(written);
                // Free result if not in env
                freeResult(result, &e, a);
            } else |err| {
                try e.printerr(err);
                return;
            }
            gcASTCount += 1;
            // Free root appropriately
            freeRoot(root, &e, a);
        } else |err| {
            if (err == error.EndOfStream)
                break;
            try e.printerr(err);
            if (!repl or !std.fs.File.stdin().isTty()) {
                return err;
            }
        }
    }
}

var buf: [4096]u8 = undefined;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const a = gpa.allocator();

    var args = try std.process.argsAlloc(a);
    defer std.process.argsFree(a, args);

    var stdin_buffer: [1024]u8 = undefined;
    if (args.len == 1) {
        const stdin_file = std.fs.File.stdin();
        const stdin_stream = stdin_file.reader(&stdin_buffer);
        try run(a, stdin_stream, true);
    } else {
        for (args[1..]) |arg| {
            var f = try std.fs.cwd().openFile(arg, .{});
            defer f.close();
            const file_reader = f.reader(&stdin_buffer);
            try run(a, file_reader, false);
        }
    }
}

test "basic test" {
    const a = std.testing.allocator;

    const T = struct { input: []const u8, want: []const u8 };
    const tests = [_]T{
        .{ .input = "1", .want = "1\n" },
        .{ .input = "(+ 1 2)", .want = "3\n" },
        .{ .input = "(setq a 1)", .want = "1\n" },
        .{ .input = "(setq a 1)(+ a 2)", .want = "1\n3\n" },
        .{ .input = "(defun foo (a b) (+ a b))", .want = "foo\n" },
        .{ .input = "(defun foo (a b) (+ a b))(foo 1 2)", .want = "foo\n3\n" },
        .{ .input = "(concatenate \"foo\" \"bar\")", .want = "foobar\n" },
        .{ .input = "'(1 2 3)", .want = "(1 2 3)\n" },
        .{ .input = "(length '(1 2 3))", .want = "3\n" },
    };
    
    for (tests) |t| {
        var fs = std.io.fixedBufferStream(t.input);
        var stream = PeekableStream(@TypeOf(fs.reader())).init(fs.reader());
        const br = &stream;

        var e = env.init(a);
        defer e.deinit();

        var output_buffer: [1024]u8 = undefined;
        var output_stream = std.io.fixedBufferStream(&output_buffer);
        loop: while (true) {
            if (parse(a, br)) |root| {
                var result = try eval(&e, a, root);
                try result.princ(output_stream.writer(), false);
                _ = try output_stream.writer().write("\n");
                // Free result and root appropriately
                freeResult(result, &e, a);
                freeRoot(root, &e, a);
            } else |_| {
                break :loop;
            }
        }
        // Use getWritten() to get the actual written portion
        const written_bytes = output_stream.getWritten();
        // Skipping garbage collection for now due to Zig version compatibility
        try std.testing.expect(std.mem.eql(u8, written_bytes, t.want));
    }
}
