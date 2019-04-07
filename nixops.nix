{
  defaults = { pkgs, lib, ... }: {
    documentation.nixos.enable = false;
    users.defaultUserShell = "${pkgs.zsh}/bin/zsh";

    programs.zsh = {
      enable = true;
      interactiveShellInit = ''
        setopt histignorespace
      '';

      ohMyZsh = {
        enable = true;
        customPkgs = lib.singleton pkgs.spaceship-prompt;
        theme = "spaceship";
      };
    };

    # See https://github.com/NixOS/nixops/issues/931
    system.activationScripts.nixops-vm-fix-931 = {
      deps = [];
      text = ''
        mount -o remount,rw /nix/store
        chown -R root:root /nix/store
      '';
    };
  };

  local = { pkgs, lib, ... }: {
    deployment = {
      targetEnv = "libvirtd";

      libvirtd = {
        vcpu = 2;
        headless = true;
        memorySize = 8192;
        baseImageSize = 64;
      };
    };

    environment = {
      shells = lib.singleton "${pkgs.zsh}/bin/zsh";
      systemPackages = let
        nixShell = import ./shell.nix { };
      in with pkgs; [
        git
        vim

        binutils-unwrapped
      ] ++ nixShell.buildInputs;
    };
  };
}
