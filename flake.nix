rec {
  description = "halogen-xterm";
  inputs = {
    env.url = "github:grybiena/purescript-environment";  
    xterm = {
      url = "github:grybiena/xterm"; 
      inputs = {
        env.follows = "env";
      };
    };
  };

  outputs = inputs@{ env, ... }:
    env.flake-utils.lib.eachDefaultSystem (system:
      env.build-package { inherit system;
                          name = description;
                          src = ./.;
                          overlays = inputs; 
                          derive-package = ./package.nix;
                        }                
   );
}

