final: prev:

let
  vscodeExtensions = final.pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "copilot";
      publisher = "github";
      version = "1.280.1421";
      hash = "sha256-DfIyCod0IVIhjwBYwqtctDA9V116T974O6wa3tPOrmo=";
    }
    
    {
      name = "vscode-pull-request-github";
      publisher = "github";
      version = "0.107.2025030604";
      hash = "sha256-3gXu6powUBYNBhBHA+8zvPQLdBFrwC1LML+NXPEVdIY=";
    }
    
    {
      name = "nix-ide";
      publisher = "jnoortheen";
      version = "0.4.12";
      hash = "sha256-3pXypgAwg/iEBUqPeNsyoX2oYqlKMVdemEhmhy1PuGU=";
    }
    
    {
      name = "noctis";
      publisher = "liviuschera";
      version = "10.43.3";
      hash = "sha256-RMYeW1J3VNiqYGj+2+WzC5X4Al9k5YWmwOyedFnOc1I=";
    }
    
    {
      name = "remote-containers";
      publisher = "ms-vscode-remote";
      version = "0.403.0";
      hash = "sha256-3y/YNajhA65SQQp38DVYUHnpWFb0z6ZtPMDU+RdLzEM=";
    }
    
    {
      name = "remote-ssh";
      publisher = "ms-vscode-remote";
      version = "0.119.2025030615";
      hash = "sha256-zEnkL3uFLW7fWpT1RhMFvD0jUhE2OsxtV4GGj9dJDMs=";
    }
    
    {
      name = "hexeditor";
      publisher = "ms-vscode";
      version = "1.11.1";
      hash = "sha256-RB5YOp30tfMEzGyXpOwPIHzXqZlRGc+pXiJ3foego7Y=";
    }
    
    {
      name = "vs-keybindings";
      publisher = "ms-vscode";
      version = "0.2.1";
      hash = "sha256-NnLjx3fKldg6DSA4ssUt0Vevm1w8KnjEZTINZxqM7cA=";
    }
    
    {
      name = "vsliveshare";
      publisher = "ms-vsliveshare";
      version = "1.0.5948";
      hash = "sha256-KOu9zF5l6MTLU8z/l4xBwRl2X3uIE15YgHEZJrKSHGY=";
    }
  ];
in
{
  vscode-with-extensions = prev.vscode-with-extensions.override {
    inherit vscodeExtensions;
  };

  vscode-insiders-with-extensions = prev.vscode-insiders-with-extensions.override {
    inherit vscodeExtensions;
  };
}
