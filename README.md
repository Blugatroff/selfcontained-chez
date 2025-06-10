# A Chez program, compiled into a standalone executable.

The `compile.ss` script is the result of me trying to understand [chez-exe](https://github.com/gwatt/chez-exe)
by stripping away everything, that is non-essential for my usecase, and merging everything
into a single scheme script.

## Environment variables
The SCHEME_DIRS environment variable needs to be set to run `compile.ss`.
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
./compile.ss ./main.ss
```

## Using the flake
```bash
nix run github:Blugatroff/selfcontained-chez
```

