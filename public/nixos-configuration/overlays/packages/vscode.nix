final: prev:

let
  vscodeExtensions = final.pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "copilot";
      publisher = "github";
      version = "1.294.1475";
      hash = "sha256-iL1FkkLiOXFY5X/bj/HaXxewRrqMG7Pd4lnSoJ9vDG0=";
    }
    
    {
      name = "vscode-pull-request-github";
      publisher = "github";
      version = "0.107.2025032704";
      hash = "sha256-dLg9GAQ9Gu3YZE4PH6UuH0aQitsZJGD4nHv3yqK6nTQ=";
    }
    
    {
      name = "nix-ide";
      publisher = "jnoortheen";
      version = "0.4.16";
      hash = "sha256-MdFDOg9uTUzYtRW2Kk4L8V3T/87MRDy1HyXY9ikqDFY=";
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
      version = "0.408.0";
      hash = "sha256-k/c0Ylot3DUJ2UNZDozNmDwuaUAZgPWfuVT16h9eZZI=";
    }
    
    {
      name = "remote-ssh";
      publisher = "ms-vscode-remote";
      version = "0.119.2025032715";
      hash = "sha256-gdfEEX1OC9F0Sk+QSR/EvDvowDTEo9x7DgrxkLHCVwc=";
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
