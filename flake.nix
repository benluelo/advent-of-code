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
          craneLib = (self.inputs.crane.mkLib pkgs).overrideToolchain rust-nightly;

          rust-nightly = pkgs.rust-bin.fromRustupToolchainFile ./rust/rust-toolchain.toml;

          cargoVendorDir = craneLib.vendorMultipleCargoDeps {
            inherit (craneLib.findCargoFiles ./rust) cargoConfigs;
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

          fs = pkgs.lib.fileset;

          filteredSrc =
            fs.toSource {
              root = ./.;
              fileset = fs.unions [
                ./rust/Cargo.toml
                ./rust/Cargo.lock
                ./rust/src
                (craneLib.fileset.commonCargoSources ./.)
              ];
            };

          # TODO: Filter out days >12 for years >=2025
          years = [ 2021 2022 2023 2024 2025 ];
          days = builtins.genList (builtins.add 1) 25;

          mkAocDay = year: day: const:
            let
              dayYear = "${toString year}-${toString day}";
              suffix = "${if const then "-const" else ""}";
              pname = "advent-of-code-${dayYear}${suffix}";

              attrs = {
                inherit pname;
                version = "0.0.0";

                src = builtins.trace "${filteredSrc}" filteredSrc;

                buildInputs = [ rust-nightly ] ++ (
                  pkgs.lib.optionals pkgs.stdenv.isDarwin [ pkgs.darwin.apple_sdk.MacOSX-SDK ]
                );
                nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.isDarwin [ ];

                inherit cargoVendorDir;

                preBuild = ''
                  cp -r ${aoc-inputs} inputs
                  cd rust

                  ${
                    if pkgs.stdenv.isDarwin
                    then ''
                      echo ${pkgs.darwin.apple_sdk.MacOSX-SDK}

                      export SDKROOT="${pkgs.darwin.apple_sdk.MacOSX-SDK}"
                    ''
                    else ""
                  }
                  # export RUSTC_LOG=rustc_codegen_ssa::back::link=info
                '';

                cargoCheckExtraArgs = "--no-default-features -F ${if const then "const," else ""}${dayYear}";
                cargoTestExtraArgs = "--no-default-features -F ${if const then "const," else ""}${dayYear}";

                cargoBuildCommand = "cargo rustc --release --no-default-features -F ${if const then "const" else ""},${dayYear} --target ${CARGO_BUILD_TARGET} -j1 -Z build-std=std,alloc,core -Z build-std-features=compiler-builtins-mem";

                meta.mainProgram = pname;
              };
            in
            craneLib.buildPackage (attrs // {
              cargoArtifacts = craneLib.buildDepsOnly ((builtins.removeAttrs attrs [ "src" ]) // {
                dummySrc = attrs.src;
              });
            });
        in
        {
          _module.args.pkgs = import nixpkgs {
            inherit system;
            overlays = [
              rust-overlay.overlays.default
            ];
          };

          packages = (builtins.listToAttrs (
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
          )) // {
            rust-full =
              let
                crateInfo = craneLib.crateNameFromCargoToml { cargoToml = ./rust/Cargo.toml; };
              in
              craneLib.buildPackage (
                crateInfo
                  // {
                  src = craneLib.cleanCargoSource ./rust;
                  doCheck = false;
                }
              );
          };

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
            programs = {
              nixpkgs-fmt.enable = true;
              taplo.enable = true;
              dprint = {
                enable = true;
                settings = {
                  lineWidth = 80;
                  includes = [ "*.md" ];
                  # markdown = {
                  #   textWrap = "always";
                  # };
                  plugins = [
                    "https://plugins.dprint.dev/markdown-0.17.8.wasm"
                  ];
                };
              };
              rustfmt = {
                enable = true;
                package = rust-nightly;
                edition = "2024";
              };
            };
          };
        };
    };
}
