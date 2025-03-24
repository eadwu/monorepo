final: prev:

let
  vscodeExtensions = final.pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "copilot";
      publisher = "github";
      version = "1.290.1456";
      hash = "sha256-3vltaYqFfAVuzmezDjgLSjsP9UKNUUKkN36hYJh2+C8=";
    }
    
    {
      name = "vscode-pull-request-github";
      publisher = "github";
      version = "0.107.2025032404";
      hash = "sha256-oK3NehAHaQWrbmm92KkoUWp76de3wYIBKy4KsMjauVI=";
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
      version = "0.407.0";
      hash = "sha256-WJo8dXGgziLd2hSr6YgkAqhe8S3VyH0A/o0m4WopaC4=";
    }
    
    {
      name = "remote-ssh";
      publisher = "ms-vscode-remote";
      version = "0.119.2025031815";
      hash = "sha256-8hyMfRxXGDJ3W4z+Um6wUj1Jjz7vhCBMCHELANfORVU=";
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
