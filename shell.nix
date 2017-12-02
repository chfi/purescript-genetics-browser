let
  env = import ./default.nix {};
  shell = env.shell;
  hook = ''
    export PATH=./node_modules/.bin/:$PATH
  '';
in
shell.override { shellHook = shell.shellHook + hook; }
