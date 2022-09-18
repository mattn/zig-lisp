const std = @import("std");

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
        var c = Self{
            .a = self.a,
            .v = std.StringArrayHashMap(*atom).init(self.a),
            .p = self,
            .err = null,
        };
        return c;
    }

    pub fn deinit(self: *Self) void {
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
            try std.io.getStdErr().writer().print("{}: {s}\n", .{ err, self.err.? });
            self.err = null;
        } else {
            try std.io.getStdErr().writer().print("{}\n", .{err});
        }
    }
};

const atom = union(enum) {
    sym: std.ArrayList(u8),
    bool: bool,
    num: i64,
    str: std.ArrayList(u8),
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
        var n = try atom.init(a);
        n.* = self.*;
        return n;
    }

    pub fn deinit(self: *Self, a: std.mem.Allocator, final: bool) void {
        switch (self.*) {
            .sym => |v| v.deinit(),
            .str => |v| v.deinit(),
            .lambda => |v| {
                if (!final) {
                    return;
                }
                if (v.cell.car != null) {
                    v.cell.car.?.deinit(a, final);
                    self.cell.car = null;
                }
                if (v.cell.cdr != null) {
                    v.cell.cdr.?.deinit(a, final);
                    self.cell.cdr = null;
                }
            },
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
            .bool => {},
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
        try w.writeByte('\n');
        try self.princ(w, quoted);
    }

    pub fn princ(self: @This(), w: anytype, quoted: bool) LispError!void {
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
            .bool => |v| {
                if (v) {
                    try w.writeAll("T");
                } else {
                    try w.writeAll("nil");
                }
            },
            .num => |v| try w.print("{}", .{v}),
            .lambda => |v| {
                try w.writeAll("(lambda ");
                try v.cell.cdr.?.cell.car.?.cell.cdr.?.princ(w, quoted);
                try w.writeByte(' ');
                try v.cell.cdr.?.cell.car.?.princ(w, quoted);
                try w.writeByte(')');
            },
            .cell => |v| {
                try w.writeByte('(');
                try v.car.?.princ(w, false);
                try w.writeByte(' ');
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
                    try w.writeByte(' ');
                }
                try w.writeByte(')');
            },
            .quote => |v| {
                try w.writeByte('\x27');
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
                if (p.v.get(v.items)) |ev| {
                    break :blk try eval(e, a, ev);
                }
                if (p.p == null) {
                    break;
                }
                p = p.p.?;
            }
            try e.raise("invalid symbol");
        },
        atom.str => |v| blk: {
            var bytes = std.ArrayList(u8).init(a);
            try bytes.writer().writeAll(v.items);
            var na = try atom.init(a);
            na.* = atom{
                .str = bytes,
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
                            try newe.v.put(
                                pa.?.cell.car.?.sym.items,
                                try eval(e, a, fa.?.cell.car.?),
                            );
                            pa = pa.?.cell.cdr;
                            fa = fa.?.cell.cdr;
                        }
                        break :blk eval(&newe, a, arg.?.cell.car.?.lambda.cell.cdr.?);
                    },
                    atom.sym => {
                        var funcname = arg.?.cell.car.?.sym.items;
                        for (builtins) |b, i| {
                            if (std.mem.eql(u8, b.name, funcname)) {
                                break :blk (builtins[i].ptr)(e, a, arg.?.cell.cdr.?);
                            }
                        }
                        if (try e.get(funcname)) |f| {
                            if (f.cell.cdr.?.* == atom.cell) {
                                var newe = e.child();
                                defer newe.deinit();
                                newe.p = e;
                                var pa = f.cell.cdr.?.cell.car;
                                var fa = arg.?.cell.cdr;
                                while (pa != null) {
                                    try newe.v.put(
                                        pa.?.cell.car.?.sym.items,
                                        try eval(e, a, fa.?.cell.car.?),
                                    );
                                    pa = pa.?.cell.cdr;
                                    fa = fa.?.cell.cdr;
                                }
                                break :blk eval(&newe, a, f.cell.cdr.?.cell.cdr.?.cell.car.?);
                            }
                        }
                        break :blk error.RuntimeError;
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
        atom.quote => |v| v.?,
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
    if (val.* != atom.num) {
        try e.raise("invalid type for -");
    }
    var num: i64 = val.num;
    val.deinit(a, false);
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

pub fn do_lt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, false);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for <");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, false);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for <");
    }
    var na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num < rhs.num,
    };
    return na;
}

pub fn do_gt(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, false);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for >");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, false);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for >");
    }
    var na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num > rhs.num,
    };
    return na;
}

pub fn do_eq(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, false);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for =");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, false);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for =");
    }
    var na = try atom.init(a);
    na.* = atom{
        .bool = lhs.num == rhs.num,
    };
    return na;
}

pub fn do_mod(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var lhs = try eval(e, a, arg.cell.car.?);
    defer lhs.deinit(a, false);
    if (lhs.* != atom.num) {
        try e.raise("invalid type for mod");
    }
    arg = arg.cell.cdr.?;
    var rhs = try eval(e, a, arg.cell.car.?);
    defer rhs.deinit(a, false);
    if (rhs.* != atom.num) {
        try e.raise("invalid type for mod");
    }
    var na = try atom.init(a);
    na.* = atom{
        .num = @mod(lhs.num, rhs.num),
    };
    return na;
}

// pub fn do_cond(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
//     var arg = args;
//     var cond = try eval(e, a, arg.cell.car.?);
//     defer cond.deinit(a, false);
//     if (cond.* != atom.bool) {
//         try e.raise("invalid type for cond");
//     }
//
//     arg = arg.cell.cdr.?;
//     if (cond.bool) {
//         return try eval(e, a, arg.cell.car.?);
//     }
//     arg = arg.cell.cdr.?;
//     return try eval(e, a, arg.cell.car.?);
// }

pub fn do_if(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var arg = args;
    var cond = try eval(e, a, arg.cell.car.?);
    defer cond.deinit(a, false);
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

pub fn do_princ(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var result = try eval(e, a, args);
    try result.princ(std.io.getStdOut().writer(), false);
    return result;
}

pub fn do_print(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var result = try eval(e, a, args);
    try result.print(std.io.getStdOut().writer(), false);
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

pub fn do_funcall(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var l = try eval(e, a, args.cell.car.?);
    var p = try atom.init(a);
    p.* = atom{
        .cell = cell{
            .car = l,
            .cdr = args.cell.cdr,
        },
    };
    defer p.deinit(a, false);
    return try eval(e, a, p);
}

pub fn do_lambda(e: *env, a: std.mem.Allocator, args: *atom) LispError!*atom {
    var p = try atom.init(a);
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
    .{ .name = "<", .ptr = &do_lt },
    .{ .name = ">", .ptr = &do_gt },
    .{ .name = "=", .ptr = &do_eq },
    .{ .name = "mod", .ptr = &do_mod },
    //.{ .name = "cond", .ptr = &do_cond },
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
const WriteError = std.os.WriteError;
const LispError = error{ RuntimeError, SyntaxError, OutOfMemory, EndOfStream, NoError, InvalidCharacter, IsDir, ConnectionTimedOut, NotOpenForReading } || ParseIntError || WriteError;

fn skipWhilte(br: anytype) LispError!void {
    const r = br.reader();
    loop: while (true) {
        switch (r.readByte() catch 0) {
            ' ', '\t', '\r', '\n' => {},
            else => |v| {
                if (v != 0) try br.putBackByte(v);
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
        switch (r.readByte() catch 0) {
            'a'...'z', '0'...'9', '-', '+', '>', '<' => |v| {
                try bytes.append(v);
            },
            else => |v| {
                if (v != 0) try br.putBackByte(v);
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
        switch (r.readByte() catch 0) {
            '0'...'9', '-', '+', 'e' => |v| try bytes.append(v),
            else => |v| {
                if (v != 0) try br.putBackByte(v);
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
    var byte = try r.readByte();
    try br.putBackByte(byte);
    while (byte == ';') {
        try br.reader().skipUntilDelimiterOrEof('\n');
        byte = try r.readByte();
        try br.putBackByte(byte);
    }

    return switch (byte) {
        '(' => try parseCell(a, br),
        '0'...'9', '-', '+' => try parseNumber(a, br),
        'a'...'z', '>', '<' => try parseIdent(a, br),
        '\x27' => try parseQuote(a, br),
        '"' => try parseString(a, br),
        else => error.SyntaxError,
    };
}

fn reader(r: anytype) bufReader(@TypeOf(r)) {
    return std.io.peekStream(2, r);
}

fn bufReader(comptime r: anytype) type {
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
        if (std.io.getStdIn().isTty()) {
            try std.io.getStdErr().writer().writeAll("> ");
        }
        if (parse(a, br)) |root| {
            if (eval(&e, a, root)) |result| {
                try gcValue.append(result);
            } else |err| {
                try e.printerr(err);
                return;
            }
            try gcAST.append(root);
            try std.io.getStdErr().writer().writeAll("\n");
        } else |err| {
            if (err == error.EndOfStream)
                break;
            try std.io.getStdErr().writer().print("{}\n", .{err});
            if (!std.io.getStdIn().isTty()) {
                return err;
            }
        }
    }
}

pub fn main() anyerror!void {
    const a = std.heap.page_allocator;

    var args = try std.process.argsAlloc(a);
    defer std.process.argsFree(a, args);

    if (args.len == 1) {
        var bufr = reader(std.io.getStdIn().reader());
        try run(a, &bufr);
    } else {
        for (args[1..]) |arg| {
            var f = try std.fs.cwd().openFile(arg, .{});
            defer f.close();
            var bufr = reader(f.reader());
            try run(a, &bufr);
        }
    }
}

test "basic test" {
    var a = std.testing.allocator;

    const T = struct { input: []const u8, want: []const u8 };
    var tests = [_]T{
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
        var br = reader(fs.reader());

        var e = env.init(a);
        defer e.deinit();

        var bytes = std.ArrayList(u8).init(a);
        defer bytes.deinit();

        var gcValue = std.ArrayList(*atom).init(a);
        var gcAST = std.ArrayList(*atom).init(a);
        loop: while (true) {
            if (parse(a, &br)) |root| {
                var result = try eval(&e, a, root);
                try result.princ(bytes.writer(), false);
                try bytes.writer().writeByte('\n');
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
