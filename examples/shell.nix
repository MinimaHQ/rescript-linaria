with (import <nixpkgs> {});

mkShell {
  buildInputs = [
    nodejs-16_x
  ];

  shellHook = ''
    export PATH="$PWD/node_modules/.bin/:$PATH";
  '';
}
