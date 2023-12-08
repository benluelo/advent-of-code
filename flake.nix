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
      inputs.nixpkgs.follows = "nixpkgs";
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
            lib = self.inputs.crane.lib.${system};
            nightly = lib.overrideToolchain rust-nightly;
          };

          rust-nightly = pkgs.rust-bin.fromRustupToolchainFile ./rust/rust-toolchain.toml;

          vendored = crane.lib.vendorMultipleCargoDeps {
            inherit (crane.lib.findCargoFiles ./rust) cargoConfigs;
            cargoLockList = [
              ./rust/Cargo.lock
              "${rust-nightly.passthru.availableComponents.rust-src}/lib/rustlib/src/rust/Cargo.lock"
            ];
          };

          years = [ 2022 2023 ];
          days = [ 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 ];

          mkAocDay = year: day: pkgs.stdenv.mkDerivation {
            name = "advent-of-code-${toString year}-${toString day}";
            buildInputs = [ pkgs.elfkickers ];
            src = pkgs.stdenv.mkDerivation {
              name = "${toString year}-${toString day}";
              src = crane.lib.cleanCargoSource ./.;
              buildInputs = [ rust-nightly ];
              buildPhase = ''
                cp -r --no-preserve=mode . $out

                cp ${crane.lib.configureCargoVendoredDepsHook}/nix-support/setup-hook $out/setup-hook
                source $out/setup-hook

                mkdir $out/.cargo
                touch "$out/.cargo/config.toml"

                configureCargoVendoredDepsHook ${vendored} "$out/.cargo/config.toml"

                cp -r ${aoc-inputs} $out/inputs
                # schrodinger's directory: this only exists if i print it's contents
                ls $out/inputs/*
                cd $out/rust

                # RUSTFLAGS="-C link-args=-lc -C target-feature=+crt-static -Z location-detail=none -C relocation-model=static"
                # cargo build --release --no-default-features -F ${toString year}-${toString day} -Z build-std=core -Z build-std-features=panic_immediate_abort --target="x86_64-unknown-linux-musl" -j1
                # cargo rustc --release --no-default-features -F ${toString year}-${toString day} --target x86_64-unknown-linux-musl -j1 -Z build-std=std,core -Z build-std-features=panic_immediate_abort,core/panic_immediate_abort -- -C link-arg=-nostartfiles -C link-arg=-znoseparate-code
                # cargo rustc --release --no-default-features -F 2023-1 --target x86_64-unknown-linux-gnu -j1 -Z build-std=std,core -Z build-std-features=panic_immediate_abort,core/panic_immediate_abort -- -C link-arg=-nostartfiles -C link-arg="-Wl,-znoseparate-code" -C link-arg=-Wl,--no-eh-frame-hdr -C link-arg=-Wl,-znorelro -C link-arg=-flinker-output=exec -C link-args='-nodefaultlibs -nostdlib -nolibc -s'
                RUSTC_LOG=rustc_codegen_ssa::back::link=info cargo rustc -vvv --release --no-default-features -F ${toString year}-${toString day} --target x86_64-unknown-linux-gnu -j1 -Z build-std=alloc,core -Z build-std-features=core/panic_immediate_abort -- -C linker=rust-lld -C link-args='-v --no-eh-frame-hdr -z norelro -nostdlib --disable-new-dtags --no-dynamic-linker -z nodefaultlib --hash-style=sysv --no-rosegment -z nognustack -N --icf=all --ignore-data-address-equality --ignore-data-address-equality --noinhibit-exec --print-gc-sections --print-icf-sections'
              '';
            };
            installPhase = ''
              mkdir -p $out/bin

              cp --no-preserve=mode $src/rust/target/x86_64-unknown-linux-gnu/release/advent-of-code "$out/bin/advent-of-code-${toString year}-${toString day}"

              ls -l $out/bin

              sstrip -z "$out/bin/advent-of-code-${toString year}-${toString day}"

              ls -l $out/bin

              chmod +x "$out/bin/advent-of-code-${toString year}-${toString day}"
            '';

            meta.mainProgram = "advent-of-code-${toString year}-${toString day}";
          };
        in {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = with inputs; [
              rust-overlay.overlays.default
            ];
          };
        
          packages = builtins.listToAttrs (
            map
              ({ year, day }: {
                name = "rust-${toString year}-${toString day}";
                value = mkAocDay year day;
              })
              (pkgs.lib.cartesianProductOfSets {
                year = years;
                day = days;
              })
            );

          devShells = {
            default = pkgs.mkShell {
              buildInputs = [ rust-nightly ]
                ++ (with pkgs; [
                  rnix-lsp
                  cargo-flamegraph
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
