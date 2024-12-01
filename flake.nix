{
  description = "Rust flake template";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    treefmt-nix = {
      url = "github:unionlabs/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane";
    };
    aoc-inputs = {
      url = "git+ssh://git@github.com/benluelo/advent-of-code-inputs.git";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, rust-overlay, flake-parts, aoc-inputs, treefmt-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems =
        [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" "x86_64-darwin" ];

      imports = [
        treefmt-nix.flakeModule
      ];

      perSystem = { config, self', inputs', pkgs, system, ... }:
        let
          crane = rec {

            lib = self.inputs.crane.mkLib pkgs;
            nightly = lib.overrideToolchain rust-nightly;
          };

          rust-nightly = pkgs.rust-bin.fromRustupToolchainFile ./rust/rust-toolchain.toml;

          vendored = crane.lib.vendorMultipleCargoDeps {
            inherit (crane.lib.findCargoFiles ./rust) cargoConfigs;
            cargoLockList = [
              ./rust/Cargo.lock
              "${rust-nightly.passthru.availableComponents.rust-src}/lib/rustlib/src/rust/library/Cargo.lock"
            ];
          };

          CARGO_BUILD_TARGET =
            if system == "aarch64-linux" then "aarch64-unknown-linux-gnu"
            else if system == "x86_64-linux" then "x86_64-unknown-linux-gnu"
            else if system == "aarch64-darwin" then "aarch64-apple-darwin"
            else if system == "x86_64-darwin" then "x86_64-apple-darwin"
            else throw "unsupported system `${system}`";

          years = [ 2022 2023 ];
          days = [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ];

          mkAocDay = year: day: const:
            let
              dayYear = "${toString year}-${toString day}";
              suffix = "${if const then "-const" else ""}";
              pname = "advent-of-code-${dayYear}${suffix}";
            in
            pkgs.stdenv.mkDerivation {
              name = pname;
              buildInputs = [ ];
              src = pkgs.stdenv.mkDerivation {
                name = pname;
                src = crane.lib.cleanCargoSource ./.;
                buildInputs = [ rust-nightly ] ++ (
                  pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.MacOSX-SDK ]
                );
                nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [ ];
                buildPhase = ''
                  echo $PATH
                  cp -r --no-preserve=mode . $out

                  cp ${crane.lib.configureCargoVendoredDepsHook}/nix-support/setup-hook $out/setup-hook
                  source $out/setup-hook

                  mkdir $out/.cargo
                  touch "$out/.cargo/config.toml"

                  configureCargoVendoredDepsHook ${vendored} "$out/.cargo/config.toml"

                  ls -al ${aoc-inputs}/*

                  cp -r ${aoc-inputs} $out/inputs
                  # schrodinger's directory: this only exists if i print it's contents
                  ls $out/inputs/*
                  cd $out/rust

                  ${
                    if pkgs.stdenv.isDarwin
                    then ''
                      echo ${pkgs.darwin.apple_sdk.MacOSX-SDK}

                      export SDKROOT="${pkgs.darwin.apple_sdk.MacOSX-SDK}"
                    ''
                    else ""
                  }
                  export RUSTC_LOG=rustc_codegen_ssa::back::link=info

                  cargo \
                    rustc \
                    -vvv \
                    --release \
                    --no-default-features \
                    -F ${if const then "const" else ""},${dayYear} \
                    --target ${CARGO_BUILD_TARGET} \
                    -j1 \
                    -Z build-std=alloc,core \
                    -Z build-std-features=core/panic_immediate_abort,compiler-builtins-mem \
                '';
              };
              installPhase = pkgs.lib.concatStringsSep "\n" [
                ''
                  mkdir -p $out/bin

                  cp --no-preserve=mode $src/rust/target/${CARGO_BUILD_TARGET}/release/advent-of-code "$out/bin/${pname}"
                ''
                # (pkgs.lib.optionalString pkgs.stdenv.isLinux ''
                #   ls -l $out/bin

                #   strip "$out/bin/${pname}"
                # '')
                ''
                  ls -l $out/bin

                  chmod +x "$out/bin/${pname}"
                ''
              ];

              meta.mainProgram = pname;
            };
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = with inputs; [
              rust-overlay.overlays.default
            ];
          };

          packages = builtins.listToAttrs (
            map
              ({ year, day, const }: {
                name = "rust-${toString year}-${toString day}${if const then "-const" else ""}";
                value = mkAocDay year day const;
              })
              (pkgs.lib.cartesianProduct {
                year = years;
                day = days;
                const = [ true false ];
              })
          );

          devShells = {
            default = pkgs.mkShell {
              buildInputs = [ rust-nightly ]
                ++ (with pkgs; [
                marksman
                nil
                cargo-flamegraph
                strace
              ]);
              nativeBuildInputs = [ config.treefmt.build.wrapper ]
                ++ pkgs.lib.attrsets.attrValues config.treefmt.build.programs;
            };
          };

          treefmt = {
            projectRootFile = "flake.nix";
            programs.nixpkgs-fmt.enable = true;
            programs.taplo.enable = true;
            programs.rustfmt = {
              enable = true;
              package = rust-nightly;
            };
          };
        };
    };
}
