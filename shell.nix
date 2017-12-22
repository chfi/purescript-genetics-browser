let
  env = import ./default.nix {};
  shell = env.shell;
  hook = ''
    export PATH=$PWD/node_modules/.bin/:$PATH
  '';
in
shell.override { shellHook = shell.shellHook + hook; }
