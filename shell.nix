{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell rec {
  packages = with pkgs; [
    rustc
    cargo
    rustPlatform.rustLibSrc
    rust-analyzer
    cargo-watch
    rustfmt
    fceux
  ];

  # Allows rust-analyzer to find the rust source
  RUST_SRC_PATH = "${pkgs.rustPlatform.rustLibSrc}";
}
