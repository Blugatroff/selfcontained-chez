{
    inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
        flake-utils.url = "github:numtide/flake-utils";
    };
    outputs = { self, nixpkgs, flake-utils }:
        flake-utils.lib.eachDefaultSystem (system: let
            pkgs = import nixpkgs { inherit system; };
            buildInputs = with pkgs; [ libz lz4 libuuid ];

            selfcontained-chez = pkgs.stdenv.mkDerivation {
                inherit buildInputs;
                name = "selfcontained-chez";
                src = ./.;
                buildPhase = '''';
                installPhase = ''
                    mkdir -p $out/bin
                    cp ./compile.scm $out/bin/
                    echo "#!/usr/bin/env sh" > $out/bin/selfcontained-chez
                    echo "export SCHEME_DIRS=$(echo ${pkgs.chez}/lib/csv*/ta6le/)" >> $out/bin/selfcontained-chez
                    echo "${pkgs.chez}/bin/scheme --script $out/bin/compile.scm \"\''${@:1}\"" >> $out/bin/selfcontained-chez
                    chmod +x $out/bin/selfcontained-chez
                '';
            };
        in {
            devShells.default = pkgs.mkShell {
                nativeBuildInputs = (builtins.concatLists [ buildInputs [ selfcontained-chez ] ]);
            };
            packages.default = selfcontained-chez;
        });
}

