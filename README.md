![Delta Logo](./assets/logo.svg)

Delta is a statically typed, imperative language that uses LLVM as its backend.

## Features

- [x] Struct access
- [ ] String allocation / array of chars
- [ ] If statement early return function type matching
- [ ] Array slices?
- [ ] GC

### Building

Build source.

```sh
dune build
```

Compile and dump LLVM IR to `llvm_bin/output.ll`.

```sh
dune exec -- delta examples/example.dx
```

Compile dynamically linked C library.

```sh
gcc -shared -fPIC -o libsocket.so socket.c
```

Generate executable from LLVM IR linked with a specified library.

```sh
clang-19 -target x86_64-pc-linux-gnu llvm_bin/output.ll -o llvm_bin/output -L. -lsocket -Wl,-rpath=.
```

Generate executable from LLVM IR linked with libc.

```sh
clang-19 -target x86_64-pc-linux-gnu llvm_bin/output.ll -o llvm_bin/output -lc -Wl,-rpath=.
```

Format all OCaml and dune files.

```sh
opam exec -- dune fmt
```

Lint opam file(s).

```sh
opam lint
```

## Contributing

TBD - Not open for contributions until stable version is implemented.

## License

Delta source code is released under the [Apache License 2.0](./LICENSE).
