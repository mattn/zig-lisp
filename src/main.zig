const std = @import("std");

const cell = struct {
    car: ?*atom,
    cdr: ?*atom,
};

const env = struct {
    v: std.StringArrayHashMap(*atom),
    p: ?*env,
    c: ?*env,

    const Self = @This();

    pub fn init(a: std.mem.Allocator) Self {
        return Self{
            .v = std.StringArrayHashMap(*atom).init(a),
            .p = null,
            .c = null,
        };
    }

    pub fn deinit(self: *Self) void {
        for (self.v.keys()) |key| {
            self.v.allocator.free(key);
        }
        self.v.deinit();
        if (self.c != null) {
            self.c.?.deinit();
        }
    }
};

const function = struct {
    name: []const u8,
    ptr: *const fn (*env, std.mem.Allocator, *atom) LispError!*atom,
};

const atom = union(enum) {
    sym: std.ArrayList(u8),
    num: i64,
    func: *const function,
    cell: cell,
    none: ?void,

    const Self = @This();
    var allocator: std.mem.Allocator = undefined;

    pub fn init(a: std.mem.Allocator) !*atom {
        allocator = a;
        var me = try a.create(atom);
        me.* = .{
            .none = {},
        };
        return me;
    }

    pub fn deinit(self: *Self) void {
        switch (self.*) {
            .sym => |v| v.deinit(),
            .cell => |v| {
                if (v.car != null) v.car.?.deinit();
                if (v.cdr != null) v.cdr.?.deinit();
            },
            else => {},
        }
        allocator.destroy(self);
    }

    pub fn println(self: @This(), w: anytype) LispError!void {
        try self.print(w);
        try w.writeByte('\n');
    }

    pub fn print(self: @This(), w: anytype) LispError!void {
        switch (self) {
            .none => try w.writeAll("null"),
            .sym => |v| try w.writeAll(v.items),
            .func => |v| try w.writeAll(v.name),
            .num => |v| try w.print("{}", .{v}),
            .cell => |v| {
                try w.writeByte('(');
                try v.car.?.print(w);
                try w.writeByte(' ');
                if (v.cdr == null) {
                    return;
                }
                var a = v.cdr;
                while (a != null) {
                    if (a.?.cell.car == null)
                        break;
                    try a.?.cell.car.?.print(w);
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
                    return ev;
                }
                if (p.p == null) {
                    break;
                }
                p = p.p.?;
            }
            std.log.warn("{s}", .{v.items});
            return error.RuntimeError;
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
        else => arg.?,
    };
}

pub fn do_add(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var num: i64 = 0;
    while (true) {
        var val = try eval(e, a, arg.cell.car.?);
        if (val.* == atom.num) {
            num += val.num;
        } else {
            return error.RuntimeError;
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
    if (val.* != atom.num) {
        return error.RuntimeError;
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
            num -= val.num;
        } else {
            return error.RuntimeError;
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
        if (val.* == atom.num) {
            num *= val.num;
        } else {
            return error.RuntimeError;
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
    if (val.* != atom.num) {
        return error.RuntimeError;
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

pub fn do_setq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var name = arg.cell.car.?;
    var val = try eval(e, a, arg.cell.cdr.?.cell.car.?);
    try e.v.put(name.sym.items, val);
    return args;
}

pub fn do_defun(e: *env, _: std.mem.Allocator, args: *atom) LispError!*atom {
    var name = args.cell.car.?;
    try e.v.put(name.sym.items, args);
    return args;
}

pub fn do_print(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var result = try eval(e, a, args);
    try result.println(std.io.getStdOut().writer());
    return result;
}

var builtins = [_]function{
    .{ .name = "+", .ptr = &do_add },
    .{ .name = "-", .ptr = &do_sub },
    .{ .name = "*", .ptr = &do_mat },
    .{ .name = "/", .ptr = &do_mul },
    .{ .name = "print", .ptr = &do_print },
    .{ .name = "setq", .ptr = &do_setq },
    .{ .name = "defun", .ptr = &do_defun },
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
            else => {
                try br.putBackByte(byte);
                break :loop;
            },
        }
    }
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

fn parseFunc(a: std.mem.Allocator, br: anytype) LispError!*atom {
    const r = br.reader();
    var byte = try r.readByte();
    if (byte != '(') return error.SyntaxError;

    var bytes = std.ArrayList(u8).init(a);
    defer bytes.deinit();

    while (true) {
        byte = try r.readByte();
        if (byte == ' ') {
            break;
        }
        try bytes.append(byte);
    }
    var found: usize = 0;
    for (builtins) |b, i| {
        if (std.mem.eql(u8, b.name, bytes.items)) {
            found = i;
            break;
        }
    }

    var fa = try atom.init(a);
    fa.* = atom{
        .func = &builtins[found],
    };
    var top = try atom.init(a);
    top.* = atom{
        .cell = cell{
            .car = fa,
            .cdr = null,
        },
    };
    var p = top;

    var cdr = try atom.init(a);
    cdr.* = atom{
        .cell = cell{
            .car = null,
            .cdr = null,
        },
    };
    p.cell.cdr = cdr;
    p = p.cell.cdr.?;

    try skipWhilte(br);
    while (true) {
        var value = try parse(a, br);
        p.cell.car = value;

        try skipWhilte(br);
        byte = try r.readByte();
        try br.putBackByte(byte);
        if (byte == ')') break;

        cdr = try atom.init(a);
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
        else => error.SyntaxError,
    };
}

fn reader(r: anytype) bufReader(@TypeOf(r)) {
    return std.io.peekStream(2, r);
}

fn bufReader(r: anytype) type {
    return std.io.PeekStream(std.fifo.LinearFifoBufferType{ .Static = 2 }, r);
}

fn run(a: std.mem.Allocator, br: anytype) LispError!void {
    var e = env.init(a);
    defer e.deinit();
    loop: while (true) {
        if (parse(a, br)) |root| {
            _ = try eval(&e, a, root);
        } else |_| {
            break :loop;
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
        _ = try run(a, &bufr);
    } else {
        var bufr = reader(std.io.getStdIn().reader());
        _ = try run(a, &bufr);
    }
}

test "basic test" {
    var a = std.testing.allocator;

    const T = struct { input: []const u8, want: []const u8 };
    var tests = [_]T{
        .{ .input = "1", .want = "1" },
    };
    for (tests) |t| {
        var e = env.init(a);
        defer e.deinit();
        var br = reader(std.io.fixedBufferStream(t.input).reader());
        if (parse(a, &br)) |root| {
            var result = try eval(&e, a, root);
            defer result.deinit();
            var bytes = std.ArrayList(u8).init(a);
            defer bytes.deinit();
            try result.print(bytes.writer());
            try std.testing.expect(std.mem.eql(u8, bytes.items, t.want));
        } else |_| {
            @panic("bad!");
        }
    }
}
