final: prev:

let
  vscodeExtensions = final.pkgs.vscode-utils.extensionsFromVscodeMarketplace [
    {
      name = "better-comments";
      publisher = "aaron-bond";
      version = "3.0.2";
      hash = "sha256-hQmA8PWjf2Nd60v5EAuqqD8LIEu7slrNs8luc3ePgZc=";
    }

    {
      name = "bookmarks";
      publisher = "alefragnani";
      version = "13.5.0";
      hash = "sha256-oKhd5BLa2wuGNrzW9yKsWWzaU5hNolw2pBcqPlql9Ro=";
    }

    {
      name = "path-intellisense";
      publisher = "christian-kohler";
      version = "2.10.0";
      hash = "sha256-bE32VmzZBsAqgSxdQAK9OoTcTgutGEtgvw6+RaieqRs=";
    }

    {
      name = "githistory";
      publisher = "donjayamanne";
      version = "0.6.20";
      hash = "sha256-nEdYS9/cMS4dcbFje23a47QBZr9eDK3dvtkFWqA+OHU=";
    }

    {
      name = "gitlens";
      publisher = "eamodio";
      version = "2025.3.204";
      hash = "sha256-4L30w8Ht/zSf2yuF1V0go6IanlNGaKW6+HEvakSwy1M=";
    }

    {
      name = "copilot";
      publisher = "github";
      version = "1.276.1405";
      hash = "sha256-DAtSxsllj4hsYPoyDLgSe6I4vJhyGSUCcWKCgtQeAMU=";
    }

    {
      name = "vscode-pull-request-github";
      publisher = "github";
      version = "0.105.2025022704";
      hash = "sha256-SgNuPiLtWOAk0w9YJOS0vwk1ruvBkZwnDRgbmXJFhDU=";
    }

    {
      name = "todo-tree";
      publisher = "gruntfuggly";
      version = "0.0.226";
      hash = "sha256-Fj9cw+VJ2jkTGUclB1TLvURhzQsaryFQs/+f2RZOLHs=";
    }

    {
      name = "nix-ide";
      publisher = "jnoortheen";
      version = "0.4.12";
      hash = "sha256-3pXypgAwg/iEBUqPeNsyoX2oYqlKMVdemEhmhy1PuGU=";
    }

    {
      name = "magit";
      publisher = "kahole";
      version = "0.6.66";
      hash = "sha256-RLSb5OKWmSZB2sgi+YRkaOEaypI5hV6PGItqRw+ihts=";
    }

    {
      name = "noctis";
      publisher = "liviuschera";
      version = "10.43.3";
      hash = "sha256-RMYeW1J3VNiqYGj+2+WzC5X4Al9k5YWmwOyedFnOc1I=";
    }

    {
      name = "git-graph";
      publisher = "mhutchie";
      version = "1.30.0";
      hash = "sha256-sHeaMMr5hmQ0kAFZxxMiRk6f0mfjkg2XMnA4Gf+DHwA=";
    }

    {
      name = "vscode-filesize";
      publisher = "mkxml";
      version = "3.2.2";
      hash = "sha256-RVhgCt/zY155oeL7EbVBokNFFBB9xvGL3j3zySdjGRg=";
    }

    {
      name = "remote-containers";
      publisher = "ms-vscode-remote";
      version = "0.400.0";
      hash = "sha256-UXgyFzzM19Elpdtza6zwXxSGg69ddBwVIe+m0anc9AE=";
    }

    {
      name = "remote-ssh";
      publisher = "ms-vscode-remote";
      version = "0.118.2025022515";
      hash = "sha256-bhutaqCmyJTw+biiNxWQ6cdbIRRSeIcNxh2NferCr9g=";
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

    {
      name = "gremlins";
      publisher = "nhoizey";
      version = "0.26.0";
      hash = "sha256-ML04SccSOrj5qY0HHJ5jiNbWkPElU1+zZNSX2i1K2uk=";
    }

    {
      name = "explorer-exclude";
      publisher = "peterschmalfeldt";
      version = "1.3.2";
      hash = "sha256-B7zdf3Artolz5K6hwA+bIdLP0v4w9K+SsQ/QVorRG84=";
    }

    {
      name = "errorlens";
      publisher = "usernamehw";
      version = "3.23.0";
      hash = "sha256-mz3JU4+/P6nM/SEJcVG5gq5K1Ym9L8N2pXbfw8a5DoA=";
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
