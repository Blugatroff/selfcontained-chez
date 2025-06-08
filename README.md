# A Chez program, compiled into a standalone executable.

The `compile.ss` script is the result of me trying to understand [chez-exe](https://github.com/gwatt/chez-exe)
by stripping away everything, that is non-essential for my usecase, and merging everything
into a single scheme script.

## Environment variables
The SCHEME_DIR environment variable needs to be set to run `compile.ss`.
It should be the path of the directory which contain the following files:
- `scheme.h`
- `scheme.boot`
- `petite.boot`
- `liblz4.a`
- `libz.a`

On NixOS:
```bash
nix-shell -p chez
export SCHEME_DIR=$(nix-instantiate --eval --expr '"${(import <nixpkgs> {}).chez}/lib/csv10.2.0/ta6le/"' | jq -r)
```

On Debian:
```bash
sudo apt install chezscheme chezscheme-dev uuid-dev
```

## Usage
```bash
./compile.ss gcc $SCHEME_DIR ./main.ss
```
