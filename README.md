# A Chez program, compiled into a standalone executable.

The `compile.scm` script is the result of me trying to understand [chez-exe](https://github.com/gwatt/chez-exe)
by stripping away everything, that is non-essential for my usecase, and merging everything
into a single scheme script.

## Environment variables
The SCHEME_DIRS environment variable needs to be set to run `compile.scm`.
It should be the path of the directories which contain the following files:
- `scheme.h`
- `scheme.boot`
- `petite.boot`
- `kernel.o` or `libkernel.a`
- `liblz4.a` (optional)
- `libz.a` (optional)

On Debian:
```bash
sudo apt install chezscheme chezscheme-dev uuid-dev
export SCHEME_DIRS=$(echo /usr/lib/csv*/ta6le/)
```

## Usage
```bash
./compile.scm ./examples/hello.scm
```

## Using the flake
```bash
nix run github:Blugatroff/selfcontained-chez ./examples/hello.scm
```

## Exposing more C functions
You can expose C functions to scheme by setting the *FOREIGN_SYMBOLS* variable.
Every symbol name in the comma separated list, gets registered using [*Sregister_symbol*](https://cisco.github.io/ChezScheme/csug/foreign.html#./foreign:s319).

Every additional argument to compile.ss after the entry point is passed along to gcc.

Here is an example using for exposing [Raylib](https://www.raylib.com/) functions:
```bash
FOREIGN_SYMBOLS=InitWindow,SetTargetFPS,WindowShouldClose,BeginDrawing,EndDrawing,ClearBackground,CloseWindow \
  ./compile.scm ./examples/raylib.scm -lraylib

./examples/raylib
```

