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

    fn init(a: std.mem.Allocator) Self {
        return Self{
            .v = std.StringArrayHashMap(*atom).init(a),
            .p = null,
            .c = null,
        };
    }

    pub fn deinit(self: *Self) void {
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

    pub fn println(self: @This(), w: anytype) std.os.WriteError!void {
        try self.print(w);
        try w.writeByte('\n');
    }

    pub fn print(self: @This(), w: anytype) std.os.WriteError!void {
        switch (self) {
            .sym => |v| {
                try w.writeAll(v.items);
            },
            .none => {},
            .func => |v| {
                try w.writeAll(v.name);
            },
            .num => |v| {
                try w.print("{}", .{v});
            },
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

fn eval(e: *env, a: std.mem.Allocator, root: *atom) LispError!*atom {
    var arg: ?*atom = root;

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
            var na = try a.create(atom);
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
        var na = try a.create(atom);
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
            var na = try a.create(atom);
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
            var na = try a.create(atom);
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
        var na = try a.create(atom);
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
            var na = try a.create(atom);
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

const ByteReader = struct {
    str: []const u8,
    curr: usize,

    const Error = error{NoError};
    const Self = @This();
    const Reader = std.io.Reader(*Self, Error, read);

    fn init(str: []const u8) Self {
        return Self{
            .str = str,
            .curr = 0,
        };
    }

    fn unget(self: *Self) void {
        self.curr -= 1;
    }

    fn read(self: *Self, dest: []u8) Error!usize {
        if (self.str.len <= self.curr or dest.len == 0) return 0;
        dest[0] = self.str[self.curr];
        self.curr += 1;
        return 1;
    }

    fn reader(self: *Self) Reader {
        return .{ .context = self };
    }
};

const SyntaxError = error{};
const RuntimeError = error{};
const ParseIntError = std.fmt.ParseIntError;
const WriteError = std.os.WriteError;
const LispError = error{ RuntimeError, SyntaxError, OutOfMemory, EndOfStream, NoError, InvalidCharacter } || ParseIntError || WriteError;

fn skipWhilte(br: *ByteReader) LispError!void {
    const r = br.reader();
    while (true) {
        const byte = r.readByte() catch 0;
        if (byte != ' ' and byte != '\t' and byte != '\r' and byte != '\n') {
            br.unget();
            break;
        }
    }
}

fn parseIdent(a: std.mem.Allocator, br: *ByteReader) LispError!*atom {
    const r = br.reader();
    var bytes = std.ArrayList(u8).init(a);
    errdefer bytes.deinit();
    while (true) {
        const byte = switch (r.readByte() catch 0) {
            'a'...'z', '0'...'9', '-', '+' => |b| b,
            else => 0,
        };
        if (byte == 0) {
            br.unget();
            break;
        }
        try bytes.append(byte);
    }
    var p = try a.create(atom);
    p.* = atom{
        .sym = bytes,
    };
    return p;
}

fn parseCell(a: std.mem.Allocator, br: *ByteReader) LispError!*atom {
    const r = br.reader();
    var byte = try r.readByte();
    if (byte != '(') return error.SyntaxError;

    var top = try a.create(atom);
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
        br.unget();

        var cdr = try a.create(atom);
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

fn parseFunc(a: std.mem.Allocator, br: *ByteReader) LispError!*atom {
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

    var fa = try a.create(atom);
    fa.* = atom{
        .func = &builtins[found],
    };
    var top = try a.create(atom);
    top.* = atom{
        .cell = cell{
            .car = fa,
            .cdr = null,
        },
    };
    var p = top;

    var cdr = try a.create(atom);
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
        br.unget();
        if (byte == ')') break;

        cdr = try a.create(atom);
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

fn parseNumber(a: std.mem.Allocator, br: *ByteReader) LispError!*atom {
    const r = br.reader();
    var bytes = std.ArrayList(u8).init(a);
    defer bytes.deinit();
    while (true) {
        const byte = switch (r.readByte() catch 0) {
            '0'...'9', '-', '+', 'e' => |b| b,
            else => 0,
        };
        if (byte == 0) {
            br.unget();
            break;
        }
        try bytes.append(byte);
    }

    if (std.fmt.parseInt(i64, bytes.items, 10)) |num| {
        var p = try a.create(atom);
        p.* = atom{
            .num = num,
        };
        return p;
    } else |_| {
        br.unget();
        return parseIdent(a, br);
    }
}

fn parse(a: std.mem.Allocator, br: *ByteReader) LispError!*atom {
    try skipWhilte(br);
    const r = br.reader();
    const byte = try r.readByte();
    br.unget();
    return switch (byte) {
        '(' => try parseCell(a, br),
        '0'...'9', '-', '+' => try parseNumber(a, br),
        'a'...'z' => try parseIdent(a, br),
        else => error.SyntaxError,
    };
}

pub fn main() anyerror!void {
    const a = std.heap.page_allocator;

    var args = try std.process.argsWithAllocator(a);
    defer args.deinit();

    _ = args.next();

    while (args.next()) |aa| {
        var f = try std.fs.cwd().openFile(aa, .{});
        defer f.close();
        var bufr = std.io.bufferedReader(f.reader());
        //var content = try bufr.reader().readAllAlloc(a, 8192);

        //var br = ByteReader.init(content);
        var root = try parse(a, bufr.fifo);

        var e = env.init(a);
        defer e.deinit();
        _ = try eval(&e, a, root);
    }
}
