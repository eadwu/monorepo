# Post Installation
Setup your wifi with [netctl](../resources/netctl.md).

## Sources
LearnLinux.tv's [Installing Arch Linux (Post Install)](https://www.youtube.com/watch?v=GCUmGtCYPWM)

Tech Linux's [Ep4: How to setup network manager, usb, and more](https://www.youtube.com/watch?v=DtohxreWjVg)

#### Graphic Drivers
This is assuming that you have Intel Graphics.
If not look [here](https://wiki.archlinux.org/index.php/xorg#Driver_installation) or for a spreadsheet made by other Linux users look [here](https://docs.google.com/spreadsheets/d/1nG9Y9nhA615IkjNUE_ew7JmiVHD7ORA4BExW0-teQ40/edit#gid=897452601).

Run `pacman -S mesa` to install the drivers for Intel cards. I use the modesetting driver.

If you want the 32-bit drivers as well then run `pacman -S lib32-intel-dri lib32-mesa lib32-libgl`

### Battery Configuration
Run `sudo nano /etc/systemd/system/powertop.service` and put the following inside it

    [Unit]
    Description=Powertop tunings

    [Service]
    Type=oneshot
    ExecStart=/usr/bin/powertop --auto-tune

    [Install]
    WantedBy=multi-user.target

Enable the services with `sudo systemctl enable powertop`, `sudo systemctl enable tlp`, and `sudo systemctl enable tlp-sleep`. Edit `/usr/lib/systemd/system/tlp.service` to remove `NetworkManager.service` from the `Wants=` line.

Run `sudo powertop --calibrate`

### Git Configuration
Generate SSH Key by

    ssh-keygen -t rsa -b 4096 -C "your_email@example.com"
    eval "$(ssh-agent -s)"
    ssh-add ~/.ssh/id_rsa
    xclip -sel clip < ~/.ssh/id_rsa.pub

Generate GPG Key by

    gpg --gen-key
    gpg --list-secret-keys --keyid-format LONG
    gpg --armor --export GPG_IDENTIFIER

Git Config

    git config --global user.name NAME_HERE
    git config --global user.email EMAIL_HERE
    git config --global core.editor "vim"
    git config --global user.signingkey GPG_IDENTIFIER
    git config --global commit.gpgsign true

#### Chrome Extensions in Opera
Use the following link `https://clients2.google.com/service/update2/crx?response=redirect&os=mac&arch=x86-64&nacl_arch=x86-64&prod=chromiumcrx&prodchannel=unknown&prodversion=61.0.3163.79&x=id%3D${EXTENSION_ID}%26uc` and replace `${EXTENSION_ID}` with the extension id.

#### VS Code
    extensions=($(cat external/code.txt | grep -Po '^.*$'))

    for extension in "${extensions[@]}"
    do
      code-insiders --install-extension ${extension}
    done
