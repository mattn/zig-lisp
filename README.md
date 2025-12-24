# zig-lisp

Toy implementation of Lisp written in Zig.

## Usage

```
$ zig run -- file.lisp
```

Or run the REPL:

```
$ zig run
> (+ 1 2)
3
```

## Requirements

* Zig

## Build

For release builds (recommended):

```
$ zig build -Doptimize=ReleaseFast
$ ./zig-out/bin/zig-lisp example/fib.lisp
```

Or build and run directly:

```
$ zig build-exe src/main.zig -O ReleaseFast
$ ./main example/fib.lisp
```

### Note on Debug Builds

Debug builds will report memory leaks due to the lack of garbage collection in this toy implementation. The leaks occur during recursive function calls where temporary atoms are created but not fully tracked for cleanup.

**For production use, always build with `-Doptimize=ReleaseFast`**, which disables memory leak detection and provides better performance.

The memory leaks are not problematic for:
- Script execution (memory is reclaimed by OS on process exit)
- Short REPL sessions

However, long-running REPL sessions may accumulate memory over time.

## Examples

See the `example/` directory for sample Lisp programs:

- `fib.lisp` - Fibonacci sequence
- `tak.lisp` - Takeuchi function
- `fizzbuzz.lisp` - FizzBuzz

## License

MIT

## Author

Yasuhiro Matsumoto (a.k.a. mattn)
