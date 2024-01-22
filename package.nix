{ ps-pkgs, pkgs, name, ... }:
  with ps-pkgs;
  { version = "1.0.0";
    dependencies =
      [ halogen
        halogen-css
        profunctor-lenses
        options
        xterm
      ];
    src = "src";
    pursuit = {
      inherit name; 
      repo = "https://github.com/grybiena/halogen-xterm.git";
      license = pkgs.lib.licenses.mit;
    };

  }
