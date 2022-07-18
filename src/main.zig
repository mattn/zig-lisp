const std = @import("std");

const cell = struct {
    car: ?*atom,
    cdr: ?*atom,
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
    c: ?*env,
    err: ?[]const u8,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) Self {
        return Self{
            .a = a,
            .v = std.StringArrayHashMap(*atom).init(a),
            .p = null,
            .c = null,
            .err = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.v.clearAndFree();
        self.v.deinit();
        if (self.c != null) {
            self.c.?.deinit();
        }
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
            try std.io.getStdErr().writer().print("{}: {s}\n", .{ err, self.err.? });
            self.err = null;
        } else {
            try std.io.getStdErr().writer().print("{}\n", .{err});
        }
    }
};

const atom = union(enum) {
    sym: std.ArrayList(u8),
    num: i64,
    str: std.ArrayList(u8),
    func: *const function,
    quote: ?*atom,
    cell: cell,
    none: ?void,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) !*atom {
        return try a.create(atom);
    }

    pub fn copy(self: *Self, a: std.mem.Allocator) !*Self {
        var n = try atom.init(a);
        n.* = self.*;
        return n;
    }

    pub fn deinit(self: *Self, a: std.mem.Allocator, final: bool) void {
        switch (self.*) {
            .sym => |v| v.deinit(),
            .str => |v| v.deinit(),
            .cell => |v| {
                if (!final) {
                    return;
                }
                if (v.car != null) {
                    v.car.?.deinit(a, final);
                    self.cell.car = null;
                }
                if (v.cdr != null) {
                    v.cdr.?.deinit(a, final);
                    self.cell.cdr = null;
                }
            },
            .quote => |v| {
                if (final) {
                    v.?.deinit(a, true);
                }
            },
            .num => {},
            .func => {},
            .none => {},
        }
        a.destroy(self);
    }

    pub fn println(self: @This(), w: anytype, quoted: bool) LispError!void {
        try self.print(w, quoted);
        try w.writeByte('\n');
    }

    pub fn print(self: @This(), w: anytype, quoted: bool) LispError!void {
        switch (self) {
            .none => try w.writeAll("null"),
            .sym => |v| try w.writeAll(v.items),
            .str => |v| {
                if (quoted) {
                    try w.writeByte('"');
                    for (v.items) |c| {
                        switch (c) {
                            '\\' => try w.writeAll("\\\\"),
                            '"' => try w.writeAll("\\\""),
                            '\n' => try w.writeAll("\\n"),
                            '\r' => try w.writeAll("\\r"),
                            else => try w.writeByte(c),
                        }
                    }
                    try w.writeByte('"');
                } else {
                    try w.writeAll(v.items);
                }
            },
            .func => |v| try w.writeAll(v.name),
            .num => |v| try w.print("{}", .{v}),
            .cell => |v| {
                try w.writeByte('(');
                try v.car.?.print(w, false);
                try w.writeByte(' ');
                if (v.cdr == null) {
                    return;
                }
                var a = v.cdr;
                while (a != null) {
                    if (a.?.cell.car == null)
                        break;
                    try a.?.cell.car.?.print(w, quoted);
                    if (a.?.cell.cdr == null) {
                        break;
                    }
                    a = a.?.cell.cdr;
                    if (a == null) {
                        break;
                    }
                    try w.writeByte(' ');
                }
                try w.writeByte(')');
            },
            .quote => |v| {
                try w.writeByte('\x27');
                try v.?.print(w, quoted);
            },
        }
    }
};

fn debug(arg: *atom) !void {
    try arg.println(std.io.getStdOut().writer());
}

fn eval(e: *env, a: std.mem.Allocator, root: *atom) LispError!*atom {
    var arg: ?*atom = root;

    //try debug(arg.?);
    return switch (arg.?.*) {
        atom.sym => |v| {
            var p = e;
            while (true) {
                if (p.v.get(v.items)) |ev| {
                    return try ev.copy(a);
                }
                if (p.p == null) {
                    break;
                }
                p = p.p.?;
            }
            return error.RuntimeError;
        },
        atom.str => |v| {
            var bytes = std.ArrayList(u8).init(a);
            try bytes.writer().writeAll(v.items);
            var na = try atom.init(a);
            na.* = atom{
                .str = bytes,
            };
            return na;
        },
        atom.cell => {
            var last = arg.?;
            while (true) {
                last = try switch (arg.?.cell.car.?.*) {
                    atom.func => arg.?.cell.car.?.func.ptr.*(e, a, arg.?.cell.cdr.?),
                    atom.sym => {
                        var funcname = arg.?.cell.car.?.sym.items;
                        for (builtins) |b, i| {
                            if (std.mem.eql(u8, b.name, funcname)) {
                                return builtins[i].ptr.*(e, a, arg.?.cell.cdr.?);
                            }
                        }
                        if (e.v.get(funcname)) |f| {
                            if (f.cell.cdr.?.* == atom.cell) {
                                var newe = env.init(a);
                                defer newe.deinit();
                                newe.p = e;
                                var pa = f.cell.cdr.?.cell.car;
                                var fa = arg.?.cell.cdr;
                                while (pa != null) {
                                    try newe.v.put(
                                        pa.?.cell.car.?.sym.items,
                                        fa.?.cell.car.?,
                                    );
                                    pa = pa.?.cell.cdr;
                                    fa = fa.?.cell.cdr;
                                }
                                return eval(&newe, a, f.cell.cdr.?.cell.cdr.?.cell.car.?);
                            }
                        }
                        return error.RuntimeError;
                    },
                    else => eval(e, a, arg.?.cell.car.?),
                };
                arg = arg.?.cell.cdr;
                if (arg == null) {
                    return last;
                }
            }
            unreachable;
        },
        atom.quote => |v| v.?,
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
        defer val.deinit(a, false);
        if (val.* == atom.num) {
            num += val.num;
        } else {
            try e.raise("invalid type for +");
        }
        if (arg.cell.cdr == null) {
            var na = try atom.init(a);
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
    defer val.deinit(a, false);
    if (val.* != atom.num) {
        try e.raise("invalid type for -");
    }
    var num: i64 = val.num;
    if (arg.cell.cdr == null) {
        var na = try atom.init(a);
        na.* = atom{
            .num = num,
        };
        return na;
    }
    while (true) {
        arg = arg.cell.cdr.?;
        val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, false);
        if (val.* == atom.num) {
            num -= val.num;
        } else {
            try e.raise("invalid type for -");
        }
        if (arg.cell.cdr == null) {
            var na = try atom.init(a);
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
        defer val.deinit(a, false);
        if (val.* == atom.num) {
            num *= val.num;
        } else {
            try e.raise("invalid type for *");
        }
        if (arg.cell.cdr == null) {
            var na = try atom.init(a);
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
    defer val.deinit(a, false);
    if (val.* != atom.num) {
        try e.raise("invalid type for /");
    }
    var num: i64 = val.num;
    if (arg.cell.cdr == null) {
        var na = try atom.init(a);
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
            var na = try atom.init(a);
            na.* = atom{
                .num = num,
            };
            return na;
        }
    }
    unreachable;
}

pub fn do_setq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var val = try eval(e, a, arg.cell.cdr.?.cell.car.?);
    var name = arg.cell.car.?;
    try e.v.put(name.sym.items, val);
    return val;
}

pub fn do_defun(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var name = args.cell.car.?;
    try e.v.put(name.sym.items, args);
    var bytes = std.ArrayList(u8).init(a);
    try bytes.writer().writeAll(name.sym.items);
    var p = try atom.init(a);
    p.* = atom{
        .sym = bytes,
    };
    return p;
}

pub fn do_print(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var result = try eval(e, a, args);
    try result.println(std.io.getStdOut().writer(), false);
    return result;
}

pub fn do_concatenate(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var bytes = std.ArrayList(u8).init(a);
    while (true) {
        var val = try eval(e, a, arg.cell.car.?);
        defer val.deinit(a, false);
        if (val.* == atom.str) {
            try bytes.writer().writeAll(val.str.items);
        } else {
            try e.raise("invalid type for concatenate");
        }
        if (arg.cell.cdr == null) {
            var na = try atom.init(a);
            na.* = atom{
                .str = bytes,
            };
            return na;
        }
        arg = arg.cell.cdr.?;
    }
    unreachable;
}

pub fn do_length(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = try eval(e, a, args);
    defer arg.deinit(a, false);
    var n: i64 = 0;
    while (true) {
        n += 1;
        if (arg.cell.cdr == null) {
            var na = try atom.init(a);
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
    .{ .name = "print", .ptr = &do_print },
    .{ .name = "setq", .ptr = &do_setq },
    .{ .name = "defun", .ptr = &do_defun },
    .{ .name = "length", .ptr = &do_length },
    .{ .name = "concatenate", .ptr = &do_concatenate },
};

const SyntaxError = error{};
const RuntimeError = error{};
const ParseIntError = std.fmt.ParseIntError;
const WriteError = std.os.WriteError;
const LispError = error{ RuntimeError, SyntaxError, OutOfMemory, EndOfStream, NoError, InvalidCharacter, IsDir, ConnectionTimedOut, NotOpenForReading } || ParseIntError || WriteError;

fn skipWhilte(br: anytype) LispError!void {
    const r = br.reader();
    loop: while (true) {
        const byte = r.readByte() catch 0;
        switch (byte) {
            ' ', '\t', '\r', '\n' => {},
            else => |v| {
                if (v != 0) try br.putBackByte(byte);
                break :loop;
            },
        }
    }
}

fn parseString(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const r = br.reader();
    var byte = try r.readByte();
    if (byte != '"') return error.SyntaxError;
    var bytes = std.ArrayList(u8).init(a);
    errdefer bytes.deinit();
    while (true) {
        byte = try r.readByte();
        if (byte == '\\') {
            byte = switch (r.readByte() catch 0) {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                else => byte,
            };
        } else if (byte == '"') {
            break;
        }
        try bytes.append(byte);
    }
    var p = try atom.init(a);
    p.* = atom{
        .str = bytes,
    };
    return p;
}

fn parseIdent(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const r = br.reader();
    var bytes = std.ArrayList(u8).init(a);
    errdefer bytes.deinit();
    loop: while (true) {
        const byte = r.readByte() catch 0;
        switch (byte) {
            'a'...'z', '0'...'9', '-', '+' => {
                try bytes.append(byte);
            },
            else => {
                try br.putBackByte(byte);
                break :loop;
            },
        }
    }
    var p = try atom.init(a);
    p.* = atom{
        .sym = bytes,
    };
    return p;
}

fn parseQuote(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const r = br.reader();
    var byte = try r.readByte();
    if (byte != '\x27') return error.SyntaxError;

    var c = try parseCell(a, br);
    var p = try atom.init(a);
    p.* = atom{ .quote = c };
    return p;
}

fn parseCell(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const r = br.reader();
    var byte = try r.readByte();
    if (byte != '(') return error.SyntaxError;

    var top = try atom.init(a);
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
        byte = try r.readByte();
        if (byte == ')') {
            break;
        }
        try br.putBackByte(byte);

        var cdr = try atom.init(a);
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
    const r = br.reader();
    var bytes = std.ArrayList(u8).init(a);
    defer bytes.deinit();
    loop: while (true) {
        const byte = r.readByte() catch 0;
        switch (byte) {
            '0'...'9', '-', '+', 'e' => |b| try bytes.append(b),
            else => {
                try br.putBackByte(byte);
                break :loop;
            },
        }
    }

    if (std.fmt.parseInt(i64, bytes.items, 10)) |num| {
        var p = try atom.init(a);
        p.* = atom{
            .num = num,
        };
        return p;
    } else |_| {
        try br.putBack(bytes.items);
        return parseIdent(a, br);
    }
}

fn parse(a: std.mem.Allocator, br: anytype) LispError!*atom {
    try skipWhilte(br);
    const r = br.reader();
    const byte = try r.readByte();
    try br.putBackByte(byte);
    return switch (byte) {
        '(' => try parseCell(a, br),
        '0'...'9', '-', '+' => try parseNumber(a, br),
        'a'...'z' => try parseIdent(a, br),
        '\x27' => try parseQuote(a, br),
        '"' => try parseString(a, br),
        else => error.SyntaxError,
    };
}

fn reader(r: anytype) bufReader(@TypeOf(r)) {
    return std.io.peekStream(2, r);
}

fn bufReader(r: anytype) type {
    return std.io.PeekStream(std.fifo.LinearFifoBufferType{ .Static = 2 }, r);
}

fn run(a: std.mem.Allocator, br: anytype) !void {
    var e = env.init(a);
    defer e.deinit();

    var gcValue = std.ArrayList(*atom).init(a);
    var gcAST = std.ArrayList(*atom).init(a);
    defer {
        for (gcValue.items) |value| {
            value.deinit(a, false);
        }
        for (gcAST.items) |value| {
            value.deinit(a, false);
        }
        gcValue.deinit();
    }
    while (true) {
        if (parse(a, br)) |root| {
            if (eval(&e, a, root)) |result| {
                try gcValue.append(result);
            } else |err| {
                try e.printerr(err);
                return;
            }
            try gcAST.append(root);
        } else |err| {
            if (err == error.EndOfStream)
                break;
            try std.io.getStdErr().writer().print("{}\n", .{err});
            return err;
        }
    }
}

pub fn main() anyerror!void {
    const a = std.heap.page_allocator;

    var args = try std.process.argsWithAllocator(a);
    defer args.deinit();

    _ = args.next();

    while (args.next()) |arg| {
        var f = try std.fs.cwd().openFile(arg, .{});
        defer f.close();
        var bufr = reader(f.reader());
        try run(a, &bufr);
    } else {
        var bufr = reader(std.io.getStdIn().reader());
        try run(a, &bufr);
    }
}

test "basic test" {
    var a = std.testing.allocator;

    const T = struct { input: []const u8, want: []const u8 };
    var tests = [_]T{
        // .{ .input = "1", .want = "1\n" },
        // .{ .input = "(+ 1 2)", .want = "3\n" },
        // .{ .input = "(setq a 1)", .want = "1\n" },
        // .{ .input = "(setq a 1)(+ a 2)", .want = "1\n3\n" },
        // .{ .input = "(defun foo (a b) (+ a b))", .want = "foo\n" },
        // .{ .input = "(defun foo (a b) (+ a b))(foo 1 2)", .want = "foo\n3\n" },
        // .{ .input = "(concatenate \"foo\" \"bar\")", .want = "foobar\n" },
        .{ .input = "'(1 2 3)", .want = "(1 2 3)\n" },
        .{ .input = "(length '(1 2 3))", .want = "3\n" },
    };
    for (tests) |t| {
        var br = reader(std.io.fixedBufferStream(t.input).reader());

        var e = env.init(a);
        defer e.deinit();

        var bytes = std.ArrayList(u8).init(a);
        defer bytes.deinit();

        var gcValue = std.ArrayList(*atom).init(a);
        var gcAST = std.ArrayList(*atom).init(a);
        loop: while (true) {
            if (parse(a, &br)) |root| {
                var result = try eval(&e, a, root);
                try result.println(bytes.writer(), false);
                try gcValue.append(result);
                try gcAST.append(root);
            } else |_| {
                break :loop;
            }
        }
        for (gcValue.items) |value| {
            value.deinit(a, false);
        }
        for (gcAST.items) |value| {
            value.deinit(a, true);
        }
        gcValue.deinit();
        gcAST.deinit();
        try std.testing.expect(std.mem.eql(u8, bytes.items, t.want));
    }
}
