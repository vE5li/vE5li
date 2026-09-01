{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = {
    self,
    flake-utils,
    nixpkgs,
  }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = (import nixpkgs) {inherit system;};

        generate = pkgs.writeShellScriptBin "generate" ''
          ${pkgs.lib.getExe pkgs.lean4} --run main.lean > README.md
        '';
      in {
        formatter = pkgs.alejandra;

        devShell =
          pkgs.mkShell
          {
            nativeBuildInputs = with pkgs; [
              lean4
              generate
            ];
          };
      }
    );
}
