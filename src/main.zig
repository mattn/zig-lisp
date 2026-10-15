const std = @import("std");
const builtin = @import("builtin");

pub const Diagnostics = struct {
    line: usize,
    col: usize,
    byte: u8,
};

// Custom PeekableStream that supports putBack functionality
pub fn PeekableStream(comptime ReaderType: type) type {
    return struct {
        const Self = @This();

        underlying_reader: ReaderType,
        peek_buffer: [10]?u8 = [_]?u8{null} ** 10, // Buffer for peeked bytes
        peek_buffer_size: usize = 0,
        peek_start: usize = 0, // Start index for circular buffer
        // Position of the next byte to read (1-based)
        line: usize = 1,
        col: usize = 1,
        diag: ?Diagnostics = null,

        pub fn init(underlying_reader_param: ReaderType) Self {
            return Self{ .underlying_reader = underlying_reader_param };
        }

        pub fn readByte(self: *Self) LispError!u8 {
            const byte = blk: {
                // Check if we have peeked bytes available (read from start)
                if (self.peek_buffer_size > 0) {
                    const b = self.peek_buffer[self.peek_start];
                    self.peek_start = (self.peek_start + 1) % self.peek_buffer.len;
                    self.peek_buffer_size -= 1;
                    break :blk b.?;
                }

                // Otherwise read from the underlying reader
                break :blk self.underlying_reader.takeByte() catch |err| {
                    switch (err) {
                        error.EndOfStream => return error.EndOfStream,
                        else => return error.NotOpenForReading,
                    }
                };
            };
            if (byte == '\n') {
                self.line += 1;
                self.col = 1;
            } else {
                self.col += 1;
            }
            return byte;
        }

        pub fn putBackByte(self: *Self, byte: u8) void {
            if (self.peek_buffer_size < self.peek_buffer.len) {
                self.peek_start = (self.peek_start + self.peek_buffer.len - 1) % self.peek_buffer.len;
                self.peek_buffer[self.peek_start] = byte;
                self.peek_buffer_size += 1;
                if (byte == '\n') {
                    // The column of the previous line is unknown here, but a
                    // putback '\n' is always re-read before an error can be
                    // reported, so the position is corrected by then
                    self.line -= 1;
                    self.col = 1;
                } else if (self.col > 1) {
                    self.col -= 1;
                }
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

// Symbol names are interned so that environments can compare keys by
// pointer instead of hashing or comparing strings
var interned: std.StringHashMapUnmanaged(void) = .empty;

// Each interned name is stored right after a header holding the builtin
// function of that name (if any), so eval can find a builtin from the
// symbol without a lookup
const sym_header = @sizeOf(?BuiltinFn);

fn intern(a: std.mem.Allocator, name: []const u8) ![]const u8 {
    const gop = try interned.getOrPut(a, name);
    if (!gop.found_existing) {
        const mem = a.alignedAlloc(u8, .of(?BuiltinFn), sym_header + name.len) catch |err| {
            interned.removeByPtr(gop.key_ptr);
            return err;
        };
        std.mem.bytesAsValue(?BuiltinFn, mem[0..sym_header]).* = builtin_map.get(name);
        @memcpy(mem[sym_header..], name);
        gop.key_ptr.* = mem[sym_header..];
    }
    return gop.key_ptr.*;
}

fn symHeader(name: []const u8) []align(@alignOf(?BuiltinFn)) u8 {
    const base: [*]align(@alignOf(?BuiltinFn)) u8 = @ptrCast(@alignCast(@constCast(name.ptr - sym_header)));
    return base[0 .. sym_header + name.len];
}

// The builtin function named by an interned symbol
fn symBuiltin(name: []const u8) ?BuiltinFn {
    return std.mem.bytesAsValue(?BuiltinFn, symHeader(name)[0..sym_header]).*;
}

fn internDeinit(a: std.mem.Allocator) void {
    var it = interned.keyIterator();
    while (it.next()) |k| {
        a.free(symHeader(k.*));
    }
    interned.deinit(a);
    interned = .empty;
}

const Item = struct {
    // Interned name, so only the pointer is needed to identify it
    k: [*]const u8,
    v: *atom,
};

const env = struct {
    a: std.mem.Allocator,
    // Most environments hold only a few bindings (function arguments), so
    // they are kept inline and searched linearly by interned key pointer
    small: [4]Item = undefined,
    n: usize = 0,
    more: std.ArrayListUnmanaged(Item) = .empty,
    p: ?*env,
    err: ?[]const u8,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) Self {
        return Self{
            .a = a,
            .p = null,
            .err = null,
        };
    }

    pub const Iterator = struct {
        e: *env,
        i: usize = 0,

        pub fn next(self: *Iterator) ?*Item {
            if (self.i >= self.e.n) return null;
            defer self.i += 1;
            if (self.i < self.e.small.len) return &self.e.small[self.i];
            return &self.e.more.items[self.i - self.e.small.len];
        }
    };

    pub fn iterator(self: *Self) Iterator {
        return .{ .e = self };
    }

    // Find the binding for key in this environment only
    pub fn slot(self: *Self, key: []const u8) ?*Item {
        for (self.small[0..@min(self.n, self.small.len)]) |*it| {
            if (it.k == key.ptr) return it;
        }
        if (self.n > self.small.len) {
            for (self.more.items) |*it| {
                if (it.k == key.ptr) return it;
            }
        }
        return null;
    }

    pub fn lookup(self: *Self, key: []const u8) ?*atom {
        return if (self.slot(key)) |it| it.v else null;
    }

    // Bind key to val in this environment, replacing an existing binding
    pub fn put(self: *Self, key: []const u8, val: *atom) !void {
        if (self.slot(key)) |it| {
            it.v = val;
            return;
        }
        if (self.n < self.small.len) {
            self.small[self.n] = .{ .k = key.ptr, .v = val };
        } else {
            try self.more.append(self.a, .{ .k = key.ptr, .v = val });
        }
        self.n += 1;
    }

    pub fn get(self: *Self, key: []const u8) !?*atom {
        var e: *env = self;
        while (true) {
            if (e.lookup(key)) |ev| {
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
        return Self{
            .a = self.a,
            .p = self,
            .err = null,
        };
    }

    pub fn deinit(self: *Self) void {
        // Free all atoms stored in this environment; keys are interned
        var it = self.iterator();
        while (it.next()) |entry| {
            entry.v.deinit(self.a, true);
        }
        self.more.deinit(self.a);
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
            self.a.free(self.err.?);
            self.err = null;
        } else {
            std.debug.print("{}\n", .{err});
        }
    }
};

const atom = union(enum) {
    sym: []const u8,
    bool: bool,
    num: i64,
    fnum: f64,
    str: []u8,
    lambda: lambda,
    func: *const function,
    quote: ?*atom,
    cell: cell,
    none: ?void,

    const Self = @This();

    // Freed atoms are kept on a free list and reused in release builds to
    // avoid going through the allocator for every temporary value. Debug
    // builds always use the allocator so leaks are still detected.
    const use_pool = builtin.mode != .Debug;
    var free_list: ?*atom = null;

    pub fn init(a: std.mem.Allocator) !*atom {
        if (use_pool) {
            if (free_list == null) try refill(a);
            const n = free_list.?;
            free_list = n.quote;
            return n;
        }
        return try a.create(atom);
    }

    // Refill the free list with a contiguous block. Allocating atoms one by
    // one spreads them so that hot ones collide in the same cache sets.
    noinline fn refill(a: std.mem.Allocator) !void {
        const block = try a.alloc(atom, 256);
        for (block) |*n| {
            n.* = atom{ .quote = free_list };
            free_list = n;
        }
    }

    fn destroy(self: *Self, a: std.mem.Allocator) void {
        if (use_pool) {
            self.* = atom{ .quote = free_list };
            free_list = self;
            return;
        }
        a.destroy(self);
    }

    pub fn copy(self: *Self, a: std.mem.Allocator) !*Self {
        const n = try atom.init(a);
        n.* = switch (self.*) {
            .sym => |v| atom{ .sym = v },
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
        if (isShared(self)) return;
        switch (self.*) {
            .sym => {},
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
            .fnum => {},
            .func => {},
            .none => {},
        }
        if (final) {
            self.destroy(a);
        }
    }

    pub fn println(self: @This(), w: anytype, quoted: bool) LispError!void {
        try self.print(w, quoted);
        _ = try w.write(&[_]u8{'\n'});
    }

    pub fn print(self: @This(), w: anytype, quoted: bool) LispError!void {
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
            .fnum => |v| {
                var num_buffer: [400]u8 = undefined;
                // Keep a trailing ".0" on integral values so the printed form
                // still reads as a float
                const num_str = if (std.math.isFinite(v) and @floor(v) == v)
                    try std.fmt.bufPrint(&num_buffer, "{d:.1}", .{v})
                else
                    try std.fmt.bufPrint(&num_buffer, "{d}", .{v});
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

// Booleans and small integers are shared preallocated atoms, so producing
// them does not allocate. deinit leaves them alone.
const small_int_min = -16;
const small_int_max = 1024;
var shared_atoms: [2 + small_int_max - small_int_min + 1]atom = blk: {
    @setEvalBranchQuota(10000);
    var atoms: [2 + small_int_max - small_int_min + 1]atom = undefined;
    atoms[0] = atom{ .bool = false };
    atoms[1] = atom{ .bool = true };
    for (atoms[2..], 0..) |*x, i| {
        x.* = atom{ .num = small_int_min + @as(i64, @intCast(i)) };
    }
    break :blk atoms;
};

fn isShared(x: *const atom) bool {
    const p = @intFromPtr(x);
    const base = @intFromPtr(&shared_atoms);
    return p >= base and p < base + @sizeOf(@TypeOf(shared_atoms));
}

fn boolAtom(v: bool) *atom {
    return &shared_atoms[@intFromBool(v)];
}

fn numAtom(a: std.mem.Allocator, v: i64) !*atom {
    if (v >= small_int_min and v <= small_int_max) {
        return &shared_atoms[@intCast(v - small_int_min + 2)];
    }
    const na = try atom.init(a);
    na.* = atom{ .num = v };
    return na;
}

fn debug(arg: *atom) !void {
    try arg.println(std.io.getStdOut().writer(), false);
}

// Each branch returns directly rather than yielding a value out of a switch
// expression; the latter made the result round-trip through the stack
fn eval(e: *env, a: std.mem.Allocator, form: *atom) LispError!*atom {
    var root = form;
    while (root.* == .cell) {
        const head = root.cell.car.?;
        if (head.* != .sym) break;
        const f = symBuiltin(head.sym) orelse break;
        // An if continues with the chosen branch here instead of recursing
        // through do_if
        if (f == &do_if) {
            root = try ifBranch(e, a, root.cell.cdr.?);
            continue;
        }
        return try f(e, a, root.cell.cdr.?);
    }
    switch (root.*) {
        .sym => |*v| {
            var p = e;
            while (true) {
                if (p.lookup(v.*)) |ev| {
                    return try eval(e, a, ev);
                }
                p = p.p orelse break;
            }
            return try evalOther(e, a, root);
        },
        .cell => return try evalCell(e, a, root),
        .bool => |*v| return boolAtom(v.*),
        .num => |*v| return try numAtom(a, v.*),
        .func, .none => return root,
        else => return try evalOther(e, a, root),
    }
}

// The less common cases of eval. Keeping them out of eval keeps its stack
// frame small, which matters for deep recursion.
noinline fn evalOther(e: *env, a: std.mem.Allocator, root: *atom) LispError!*atom {
    switch (root.*) {
        .sym => {
            try e.raise("invalid symbol");
            return root;
        },
        .str => |*v| {
            const str_slice = try a.dupe(u8, v.*);
            const na = try atom.init(a);
            na.* = atom{
                .str = str_slice,
            };
            return na;
        },
        .lambda => return root.copy(a),
        .quote => |*v| return v.*.?.copy(a),
        .fnum => |*v| {
            const na = try atom.init(a);
            na.* = atom{ .fnum = v.* };
            return na;
        },
        else => unreachable,
    }
}

fn evalCell(e: *env, a: std.mem.Allocator, root: *atom) LispError!*atom {
    var arg: ?*atom = root;
    var last = root;
    while (true) {
        const head = arg.?.cell.car.?;
        switch (head.*) {
            .func => |*f| last = try f.*.ptr(e, a, arg.?.cell.cdr.?),
            .lambda => return callLambda(e, a, head, arg.?.cell.cdr),
            .sym => |*name| {
                if (symBuiltin(name.*)) |f| {
                    return try f(e, a, arg.?.cell.cdr.?);
                }
                if (try e.get(name.*)) |f| {
                    if (f.cell.cdr.?.* == atom.cell) {
                        return try callDefun(e, a, f, arg.?.cell.cdr);
                    }
                }
                try e.raise("invalid function");
                unreachable;
            },
            else => last = try eval(e, a, head),
        }
        arg = arg.?.cell.cdr;
        if (arg == null) {
            return last;
        }
    }
}

// Function calls live outside eval so that their child environment does not
// enlarge eval's stack frame, which every evaluation pays for
noinline fn callLambda(e: *env, a: std.mem.Allocator, f: *atom, args: ?*atom) LispError!*atom {
    var newe = e.child();
    defer newe.deinit();
    newe.p = f.lambda.e.?;
    var pa = f.lambda.cell.car;
    var fa = args;
    while (pa != null) {
        const key = pa.?.cell.car.?.sym;
        try newe.put(
            key,
            try eval(e, a, fa.?.cell.car.?),
        );
        pa = pa.?.cell.cdr;
        fa = fa.?.cell.cdr;
    }
    return try eval(&newe, a, f.lambda.cell.cdr.?);
}

noinline fn callDefun(e: *env, a: std.mem.Allocator, f: *atom, args: ?*atom) LispError!*atom {
    var newe = e.child();
    defer newe.deinit();
    // defun bodies see the global environment rather than the caller's, so
    // the lookup chain doesn't grow with recursion depth
    var g = e;
    while (g.p) |gp| g = gp;
    newe.p = g;
    var pa = f.cell.cdr.?.cell.car;
    var fa = args;
    while (pa != null and fa != null) {
        const key = pa.?.cell.car.?.sym;
        try newe.put(
            key,
            try eval(e, a, fa.?.cell.car.?),
        );
        pa = pa.?.cell.cdr;
        fa = fa.?.cell.cdr;
    }
    return try eval(&newe, a, f.cell.cdr.?.cell.cdr.?.cell.car.?);
}

// Accumulator for arithmetic that starts as an integer and is promoted to a
// float as soon as one operand is a float
const Number = struct {
    i: i64 = 0,
    f: f64 = 0,
    isf: bool = false,

    fn from(val: *atom) ?Number {
        // Capture by pointer so the whole atom is not copied just to read
        // its tag
        return switch (val.*) {
            .num => |*v| Number{ .i = v.* },
            .fnum => |*v| Number{ .f = v.*, .isf = true },
            else => null,
        };
    }

    fn asF(self: Number) f64 {
        return if (self.isf) self.f else @as(f64, @floatFromInt(self.i));
    }

    fn promote(self: *Number) void {
        if (!self.isf) {
            self.f = @floatFromInt(self.i);
            self.isf = true;
        }
    }

    fn toAtom(self: Number, a: std.mem.Allocator) LispError!*atom {
        if (!self.isf) return try numAtom(a, self.i);
        const na = try atom.init(a);
        na.* = atom{ .fnum = self.f };
        return na;
    }
};

pub fn do_add(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num = Number{};
    while (true) {
        const n = try evalNumber(e, a, arg.cell.car.?, "invalid type for +");
        if (num.isf or n.isf) {
            num.promote();
            num.f += n.asF();
        } else {
            num.i += n.i;
        }
        if (arg.cell.cdr == null) {
            return try num.toAtom(a);
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_sub(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num = Number{};
    var first = true;
    while (true) {
        const n = try evalNumber(e, a, arg.cell.car.?, "invalid type for -");
        if (first) {
            num = n;
            first = false;
        } else if (num.isf or n.isf) {
            num.promote();
            num.f -= n.asF();
        } else {
            num.i -= n.i;
        }
        if (arg.cell.cdr == null) {
            return try num.toAtom(a);
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_mul(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num = Number{ .i = 1 };
    while (true) {
        const n = try evalNumber(e, a, arg.cell.car.?, "invalid type for *");
        if (num.isf or n.isf) {
            num.promote();
            num.f *= n.asF();
        } else {
            num.i *= n.i;
        }
        if (arg.cell.cdr == null) {
            return try num.toAtom(a);
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_div(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num = Number{};
    var first = true;
    while (true) {
        const n = try evalNumber(e, a, arg.cell.car.?, "invalid type for /");
        if (first) {
            num = n;
            first = false;
        } else if (num.isf or n.isf) {
            if (n.asF() == 0) {
                try e.raise("division by zero");
            }
            num.promote();
            num.f /= n.asF();
        } else {
            if (n.i == 0) {
                try e.raise("division by zero");
            }
            num.i = @divTrunc(num.i, n.i);
        }
        if (arg.cell.cdr == null) {
            return try num.toAtom(a);
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

fn compareNums(l: Number, r: Number) std.math.Order {
    if (!l.isf and !r.isf) {
        return std.math.order(l.i, r.i);
    }
    return std.math.order(l.asF(), r.asF());
}

// Evaluate x as a number. Literals and variables bound to numbers are read
// in place, avoiding the copy that eval would allocate.
inline fn evalNumber(e: *env, a: std.mem.Allocator, x: *atom, msg: []const u8) LispError!Number {
    switch (x.*) {
        .num => |*v| return Number{ .i = v.* },
        .fnum => |*v| return Number{ .f = v.*, .isf = true },
        .sym => |*name| {
            var p = e;
            while (true) {
                if (p.lookup(name.*)) |v| {
                    if (Number.from(v)) |n| return n;
                    break;
                }
                p = p.p orelse break;
            }
        },
        else => {},
    }
    return evalNumberSlow(e, a, x, msg);
}

noinline fn evalNumberSlow(e: *env, a: std.mem.Allocator, x: *atom, msg: []const u8) LispError!Number {
    const val = try eval(e, a, x);
    defer val.deinit(a, true);
    return Number.from(val) orelse {
        try e.raise(msg);
        unreachable;
    };
}

pub fn do_lt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const lhs = try evalNumber(e, a, args.cell.car.?, "invalid type for <");
    const rhs = try evalNumber(e, a, args.cell.cdr.?.cell.car.?, "invalid type for <");
    const ord = compareNums(lhs, rhs);
    return boolAtom(ord == .lt);
}

pub fn do_gt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const lhs = try evalNumber(e, a, args.cell.car.?, "invalid type for >");
    const rhs = try evalNumber(e, a, args.cell.cdr.?.cell.car.?, "invalid type for >");
    const ord = compareNums(lhs, rhs);
    return boolAtom(ord == .gt);
}

pub fn do_eq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const lhs = try evalNumber(e, a, args.cell.car.?, "invalid type for =");
    const rhs = try evalNumber(e, a, args.cell.cdr.?.cell.car.?, "invalid type for =");
    const ord = compareNums(lhs, rhs);
    return boolAtom(ord == .eq);
}

pub fn do_le(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const lhs = try evalNumber(e, a, args.cell.car.?, "invalid type for <=");
    const rhs = try evalNumber(e, a, args.cell.cdr.?.cell.car.?, "invalid type for <=");
    const ord = compareNums(lhs, rhs);
    return boolAtom(ord != .gt);
}

pub fn do_ge(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const lhs = try evalNumber(e, a, args.cell.car.?, "invalid type for >=");
    const rhs = try evalNumber(e, a, args.cell.cdr.?.cell.car.?, "invalid type for >=");
    const ord = compareNums(lhs, rhs);
    return boolAtom(ord != .lt);
}

pub fn do_1minus(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args.cell.car.?);
    defer arg.deinit(a, true);
    const v = switch (arg.*) {
        .num => |v| atom{ .num = v - 1 },
        .fnum => |v| atom{ .fnum = v - 1 },
        else => blk: {
            try e.raise("invalid type for 1-");
            break :blk undefined;
        },
    };
    const na = try atom.init(a);
    na.* = v;
    return na;
}

pub fn do_1plus(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args.cell.car.?);
    defer arg.deinit(a, true);
    const v = switch (arg.*) {
        .num => |v| atom{ .num = v + 1 },
        .fnum => |v| atom{ .fnum = v + 1 },
        else => blk: {
            try e.raise("invalid type for 1+");
            break :blk undefined;
        },
    };
    const na = try atom.init(a);
    na.* = v;
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
    return boolAtom(false);
}

pub fn do_dotimes(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const arg = args;

    const name = arg.cell.car.?.cell.car;
    const count = try eval(e, a, arg.cell.car.?.cell.cdr.?);
    defer count.deinit(a, true);
    if (count.* != atom.num) {
        try e.raise("invalid type for dotimes");
    }
    var newe = e.child();
    defer newe.deinit();

    const key = name.?.sym;
    var i: u32 = 0;
    while (i < count.num) : (i += 1) {
        // Check if key already exists and free old value
        if (newe.lookup(key)) |old_val| {
            old_val.deinit(a, true);
        }
        const nv = try atom.init(a);
        nv.* = atom{
            .num = @intCast(i),
        };
        try newe.put(key, nv);
        // Evaluate all forms in the body
        var body = arg.cell.cdr;
        while (body != null) {
            const value = try eval(&newe, a, body.?.cell.car.?);
            freeResult(value, &newe, a);
            body = body.?.cell.cdr;
        }
    }
    return boolAtom(false);
}

pub fn do_if(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    return try eval(e, a, try ifBranch(e, a, args));
}

// Evaluate the condition of an if and return the branch to evaluate
fn ifBranch(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var cond = try eval(e, a, arg.cell.car.?);
    defer cond.deinit(a, true);
    if (cond.* != atom.bool) {
        try e.raise("invalid type for if");
    }

    arg = arg.cell.cdr.?;
    if (cond.bool) {
        return arg.cell.car.?;
    }
    arg = arg.cell.cdr.?;
    return arg.cell.car.?;
}

pub fn do_setq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const arg = args;
    const val = try eval(e, a, arg.cell.cdr.?.cell.car.?);
    const name = arg.cell.car.?;
    if (e.slot(name.sym)) |it| {
        // Free the previous value before overwriting it.
        it.v.deinit(a, true);
        it.v = val;
    } else {
        try e.put(name.sym, val);
    }
    return val;
}

pub fn do_defun(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    const name = args.cell.car.?;
    try e.put(name.sym, args);
    const p = try atom.init(a);
    p.* = atom{
        .sym = name.sym,
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

const builtins = [_]function{
    .{ .name = "+", .ptr = &do_add },
    .{ .name = "-", .ptr = &do_sub },
    .{ .name = "*", .ptr = &do_mul },
    .{ .name = "/", .ptr = &do_div },
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

const BuiltinFn = *const fn (*env, std.mem.Allocator, *atom) LispError!*atom;

// Name -> function lookup built at compile time to avoid a linear scan of
// builtins on every call
const builtin_map = blk: {
    var kvs: [builtins.len]struct { []const u8, BuiltinFn } = undefined;
    for (builtins, 0..) |b, i| {
        kvs[i] = .{ b.name, b.ptr };
    }
    break :blk std.StaticStringMap(BuiltinFn).initComptime(kvs);
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
            'a'...'z', '0'...'9', '-', '+', '*', '/', '>', '<', '=' => |v| {
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

    const sym_slice = try intern(a, buffer[0..pos]);

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
    errdefer c.deinit(a, true);
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
    errdefer top.deinit(a, true);
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
            '0'...'9', '-', '+', '.', 'e' => |v| {
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
    } else |_| {}
    if (std.fmt.parseFloat(f64, buffer[0..pos])) |num| {
        const p = try atom.init(a);
        p.* = atom{
            .fnum = num,
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
        'a'...'z', '*', '/', '>', '<', '=' => try parseIdent(a, br),
        '\x27' => try parseQuote(a, br),
        '"' => try parseString(a, br),
        else => {
            br.diag = Diagnostics{ .line = br.line, .col = br.col, .byte = byte };
            return error.SyntaxError;
        },
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
    var it = e.iterator();
    while (it.next()) |entry| {
        if (entry.v == result) {
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
    var it = e.iterator();
    while (it.next()) |entry| {
        const env_atom = entry.v;
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
            var it2 = e.iterator();
            while (it2.next()) |entry| {
                const env_atom = entry.v;
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

fn run(a: std.mem.Allocator, reader: anytype, repl: bool, name: []const u8) !void {
    var stream = PeekableStream(@TypeOf(reader)).init(reader);
    const br = &stream;
    var e = env.init(a);
    defer e.deinit();

    const t = try atom.init(a);
    t.* = atom{
        .bool = true,
    };
    try e.put(try intern(a, "t"), t);

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
                // Output result only in REPL mode
                if (repl) {
                    var stdout_buffer: [1024]u8 = undefined;
                    var stdout_stream = std.io.fixedBufferStream(&stdout_buffer);
                    try result.println(stdout_stream.writer(), false);
                    const written = stdout_stream.getWritten();
                    _ = try std.fs.File.stdout().writeAll(written);
                }
                // Free result if not in env
                freeResult(result, &e, a);
            } else |err| {
                try e.printerr(err);
                if (!repl or !std.fs.File.stdin().isTty()) {
                    freeRoot(root, &e, a);
                    return err;
                }
            }
            gcASTCount += 1;
            // Free root appropriately
            freeRoot(root, &e, a);
        } else |err| {
            if (err == error.EndOfStream)
                break;
            if (br.diag) |d| {
                if (std.ascii.isPrint(d.byte)) {
                    std.debug.print("{s}:{d}:{d}: syntax error: unexpected character '{c}'\n", .{ name, d.line, d.col, d.byte });
                } else {
                    std.debug.print("{s}:{d}:{d}: syntax error: unexpected byte 0x{x:0>2}\n", .{ name, d.line, d.col, d.byte });
                }
                br.diag = null;
            } else {
                try e.printerr(err);
            }
            if (!repl or !std.fs.File.stdin().isTty()) {
                return err;
            }
            // Discard the rest of the line so the offending bytes are not
            // re-parsed forever
            br.skipUntilDelimiterOrEof('\n') catch break;
        }
    }
}

var buf: [4096]u8 = undefined;

fn usage() void {
    std.debug.print(
        \\usage: zig-lisp [options] [file ...]
        \\
        \\With no file, read from stdin (interactive REPL on a terminal).
        \\
        \\options:
        \\  -h, --help    show this help
        \\
    , .{});
}

pub fn main() u8 {
    // The debug allocator is slow but detects leaks, so only use it in debug
    // builds
    var gpa = std.heap.DebugAllocator(.{}){};
    defer if (builtin.mode == .Debug) {
        _ = gpa.deinit();
    };
    const a = if (builtin.mode == .Debug) gpa.allocator() else std.heap.smp_allocator;
    defer internDeinit(a);

    var args = std.process.argsAlloc(a) catch return 1;
    defer std.process.argsFree(a, args);

    for (args[1..]) |arg| {
        if (std.mem.eql(u8, arg, "-h") or std.mem.eql(u8, arg, "--help")) {
            usage();
            return 0;
        }
    }

    var stdin_buffer: [1024]u8 = undefined;
    if (args.len == 1) {
        const stdin_file = std.fs.File.stdin();
        var stdin_stream = stdin_file.reader(&stdin_buffer);
        run(a, &stdin_stream.interface, true, "stdin") catch return 1;
    } else {
        for (args[1..]) |arg| {
            var f = std.fs.cwd().openFile(arg, .{}) catch |err| {
                std.debug.print("zig-lisp: cannot open {s}: {s}\n", .{ arg, @errorName(err) });
                return 1;
            };
            defer f.close();
            var file_reader = f.reader(&stdin_buffer);
            run(a, &file_reader.interface, false, arg) catch return 1;
        }
    }
    return 0;
}

test "basic test" {
    const a = std.testing.allocator;
    defer internDeinit(a);

    const T = struct { input: []const u8, want: []const u8 };
    const tests = [_]T{
        .{ .input = "1", .want = "1\n" },
        .{ .input = "(+ 1 2)", .want = "3\n" },
        .{ .input = "(* 2 3)", .want = "6\n" },
        .{ .input = "(/ 6 2)", .want = "3\n" },
        .{ .input = "(/ 12 3 2)", .want = "2\n" },
        .{ .input = "1.5", .want = "1.5\n" },
        .{ .input = "-2.5", .want = "-2.5\n" },
        .{ .input = "1e3", .want = "1000.0\n" },
        .{ .input = "(+ 1 2.5)", .want = "3.5\n" },
        .{ .input = "(+ 1.5 1.5)", .want = "3.0\n" },
        .{ .input = "(- 10 2.5)", .want = "7.5\n" },
        .{ .input = "(* 2.5 2)", .want = "5.0\n" },
        .{ .input = "(/ 5 2)", .want = "2\n" },
        .{ .input = "(/ 5 2.0)", .want = "2.5\n" },
        .{ .input = "(< 1 1.5)", .want = "T\n" },
        .{ .input = "(>= 1.5 1.5)", .want = "T\n" },
        .{ .input = "(= 2 2.0)", .want = "T\n" },
        .{ .input = "(1+ 0.5)", .want = "1.5\n" },
        .{ .input = "(setq f 0.5)(+ f f)", .want = "0.5\n1.0\n" },
        .{ .input = "(* (+ 1 2) (- 5 1))", .want = "12\n" },
        .{ .input = "(setq a 1)", .want = "1\n" },
        .{ .input = "(setq a 1)(+ a 2)", .want = "1\n3\n" },
        .{ .input = "(defun foo (a b) (+ a b))", .want = "foo\n" },
        .{ .input = "(defun foo (a b) (+ a b))(foo 1 2)", .want = "foo\n3\n" },
        .{ .input = "(concatenate \"foo\" \"bar\")", .want = "foobar\n" },
        .{ .input = "'(1 2 3)", .want = "(1 2 3)\n" },
        .{ .input = "(length '(1 2 3))", .want = "3\n" },
    };
    
    for (tests) |t| {
        var fs = std.Io.Reader.fixed(t.input);
        var stream = PeekableStream(@TypeOf(&fs)).init(&fs);
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

test "syntax error diagnostics" {
    const a = std.testing.allocator;
    defer internDeinit(a);

    const T = struct { input: []const u8, line: usize, col: usize, byte: u8 };
    const tests = [_]T{
        .{ .input = "@", .line = 1, .col = 1, .byte = '@' },
        .{ .input = "  @", .line = 1, .col = 3, .byte = '@' },
        .{ .input = "\xe3\x81\x82", .line = 1, .col = 1, .byte = 0xe3 },
        .{ .input = "'@", .line = 1, .col = 2, .byte = '@' },
        .{ .input = "(+ 1\n @)", .line = 2, .col = 2, .byte = '@' },
        .{ .input = "'(1 2 @)", .line = 1, .col = 7, .byte = '@' },
        .{ .input = "; comment\n@", .line = 2, .col = 1, .byte = '@' },
        .{ .input = "(+ 1 2)\n(+ 3\n   @)", .line = 3, .col = 4, .byte = '@' },
    };

    for (tests) |t| {
        var fs = std.Io.Reader.fixed(t.input);
        var stream = PeekableStream(@TypeOf(&fs)).init(&fs);
        const br = &stream;

        // Parse until the error; leaks of partially built ASTs are caught by
        // the testing allocator
        while (parse(a, br)) |root| {
            root.deinit(a, true);
        } else |err| {
            try std.testing.expectEqual(error.SyntaxError, err);
        }

        try std.testing.expect(stream.diag != null);
        const d = stream.diag.?;
        try std.testing.expectEqual(t.line, d.line);
        try std.testing.expectEqual(t.col, d.col);
        try std.testing.expectEqual(t.byte, d.byte);
    }
}

test "unterminated expression frees partial ast" {
    const a = std.testing.allocator;
    defer internDeinit(a);

    const inputs = [_][]const u8{ "(", "(+ 1", "(a (b c", "'", "'(1 2", "\"abc" };
    for (inputs) |input| {
        var fs = std.Io.Reader.fixed(input);
        var stream = PeekableStream(@TypeOf(&fs)).init(&fs);

        // Must fail with EndOfStream and must not leak the partial AST
        try std.testing.expectError(error.EndOfStream, parse(a, &stream));
    }
}

test "parse recovers after skipping the error line" {
    const a = std.testing.allocator;
    defer internDeinit(a);

    var fs = std.Io.Reader.fixed("@ (this is skipped)\n(+ 1 2)");
    var stream = PeekableStream(@TypeOf(&fs)).init(&fs);
    const br = &stream;

    try std.testing.expectError(error.SyntaxError, parse(a, br));

    // Same recovery as the REPL: discard the rest of the line and continue
    try br.skipUntilDelimiterOrEof('\n');
    stream.diag = null;

    const root = try parse(a, br);
    defer root.deinit(a, true);
    try std.testing.expect(root.* == .cell);

    var e = env.init(a);
    defer e.deinit();
    const result = try eval(&e, a, root);
    defer freeResult(result, &e, a);
    try std.testing.expect(result.* == .num);
    try std.testing.expectEqual(@as(i64, 3), result.num);
}
