# Post Installation
Setup your wifi with [netctl](https://github.com/eadwu/arch-x-os/blob/master/resources/netctl.md) and run `ping -c 3 google.com` to verify that you have a connection.

## Sources
LearnLinux.tv's [Installing Arch Linux (Post Install)](https://www.youtube.com/watch?v=GCUmGtCYPWM)

LearnLinux.tv's [Getting Started with Arch Linux (Second Edition) Part 6: Setting up NTP and Bluetooth](https://www.youtube.com/watch?v=TNDisr6z7Rc)

Tech Linux's [Ep4: How to setup network manager, usb, and more](https://www.youtube.com/watch?v=DtohxreWjVg)

wiki.archlinux.org [CUPS](https://wiki.archlinux.org/index.php/CUPS)

wiki.archlinux.org [Avahi](https://wiki.archlinux.org/index.php/Avahi)

wiki.archlinux.org [PulseAudio/Troubleshooting](https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting)

wiki.archlinux.org [Bluetooth_headset](https://wiki.archlinux.org/index.php/Bluetooth_headset)

#### Audio
Run `pacman -Sy pulseaudio alsa-utils` and then `alsamixer`. Unmute by pressing M and bring the scales up until the dB gain is 0.00. "The MM label below a channel indicates that the channel is muted, and 00 indicates that it is open." -- [wiki.archlinux.org](https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting)

#### Editing /etc/pacman.conf
Run `curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/pacman.conf" > /etc/pacman.conf`

OR

Run `nano /etc/pacman.conf`
Uncomment

    [multilib]
    Include=/etc/pacman.d/mirrorlist

for 32-bit package installation.
Add the following for AUR package support

    [archlinuxfr]
    SigLevel = Never
    Server = http://repo.archlinux.fr/$arch

Run `pacman -Sy yaourt` to install yaourt. Verify yaourt is installed by `which yaourt`.

#### Wireless Connection
Run `pacman -Sy networkmanager network-manager-applet wireless_tools wpa_supplicant gnome-keyring`

#### XOrg and Trackpad Driver
Run `pacman -Sy xf86-input-libinput xorg-server xorg-xinit mesa`

#### Graphic Drivers
This is assuming that you have Intel Graphics.
If not look [here](https://wiki.archlinux.org/index.php/xorg#Driver_installation) or for a spreadsheet made by other Linux users look [here](https://docs.google.com/spreadsheets/d/1nG9Y9nhA615IkjNUE_ew7JmiVHD7ORA4BExW0-teQ40/edit#gid=897452601).
**Remember that the distro is Arch Linux.**

Run `pacman -Sy mesa` to install the drivers for Intel cards. I use the modesetting driver.

If you want the 32-bit drivers as well then run `pacman -Sy lib32-intel-dri lib32-mesa lib32-libgl`

#### NTP (Network Time Protocol)
Basically checks your time with internet time servers to verify it's accurate. See [here](https://wiki.archlinux.org/index.php/Network_Time_Protocol_daemon) for more information.

Run `pacman -Sy ntp` to install NTP.

Run `systemctl daemon-reload` and then `systemctl enable ntpd`.

#### Bluetooth

Run `pacman -Sy bluez bluez-utils blueman pulseaudio-bluetooth`.

Then run `systemctl daemon-reload` and then `systemctl enable bluetooth`.

**Headset connected but no sound**
Find your headset from running `pacmd ls` and then get the index value of your headset and run `pacmd set-card-profile INDEX_HERE a2dp_sink`.

**Headset Output Autoswitch**
Edit /etc/pulse/default.pa by running

`curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/default.pa" > /etc/pulse/default.pa`

OR

`nano /etc/pulse/default.pa` and add the following snippet to it.

    ### automatically switch to newly-connected devices
    load-module module-switch-on-connect

#### Trackpad Configuration
The default Apple trackpad movements is clickfinger based (see [here](https://wayland.freedesktop.org/libinput/doc/latest/clickpad_softbuttons.html#clickfinger)). This changes it to a button area based format (see [here](https://wayland.freedesktop.org/libinput/doc/latest/clickpad_softbuttons.html#software_buttons)). This also changes the Middle Mouse to be clicking the left and right buttons at the same time inside of the center of the trackpad.

Generate `30-touchpad.conf` by running

`curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/30-touchpad.conf" > /etc/X11/xorg.conf.d/30-touchpad.conf`

OR

`nano /etc/X11/xorg.conf.d/30-touchpad.conf` and put the following snippet inside it.

    Section "InputClass"
      Identifier "touchpad"
      Driver "libinput"
      MatchIsTouchpad "on"
      Option "ClickMethod" "buttonareas"
      Option "DisableWhileTyping" "off"
      Option "MiddleEmulation" "on"
    EndSection

#### Packages
Pacman Stuff: `pacman -Sy conky git openssh nodejs npm php redshift python-gobject python-xdg librsvg blender chromium screenfetch compton ark p7zip zip unzip unrar nitrogen rofi lsb-release ranger feh powerline processing xclip glslang i3status luarocks lm_sensors powertop tlp cups cups-pdf avahi hplip docker jenkins noto-fonts-cjk ttf-liberation`

Screen recorder: `pacman -Sy simplescreenrecorder`

Disable XFCE's default compositor and enable compton.

Edit `~/.bashrc` and add `screenfetch` to it.

Enable cups by running `systemctl enable org.cups.cupsd.service`

Enable avahi by running `systemctl enable avahi-daemon.service`

Enable docker by running `systemctl enable docker`.

Enable jenkins by running `systemctl enable jenkins`

Edit `/etc/nsswitch.conf` to change the `hosts` line to be

    hosts: ... mdns_minimal [NOTFOUND=return] resolve [!UNAVAIL=return] dns ...

For the driver I prefer `hpijs` over `hpcups`

#### USB Mounting support
Run `pacman -Sy thunar-volman gvfs`.

#### Graphical Enviornment
###### LightDM
Run `pacman -Sy lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings`. Then enable it by running `systemctl enable lightdm.service`
###### XFCE
To install XFCE run `pacman -Sy xfce4 xfce4-notifyd xfce4-screenshooter xfce4-taskmanager`.
###### i3
Run yaourt in your user `su $USER`

To install i3 run `yaourt i3-gaps`.

#### Enabling NetworkManager
Run `yaourt -S xfce4-indicator-plugin` then `systemctl disable netctl.service` to disable netctl and then to enable NetworkManager run `systemctl enable NetworkManager.service`.

#### Misc/Other Applications
oblogout-blurlock: `yaourt oblogout-blurlock` (i3lock blurlock function)

pa-applet: `yaourt pa-applet`

monitorix: `yaourt monitorix`

Tilix: `yaourt tilix` (tilix-bin)

Atom Beta: `yaourt atom-editor-beta-bin`

Flow: `yaourt flow javascript` (flow-bin)

Discord: `yaourt discord` (discord)

Pamac: `yaourt pamac-aur`

Paper Icons: `yaourt paper-icon-theme-git`

Adapta: `yaourt adapta-gtk-theme` (for notifications)

Arc Dark: `yaourt gtk-theme-arc-git` (normal theme)

ngrok: `yaourt ngrok` (for exposing localhost) [or download manually from [ngrok](https://ngrok.com/download) and move to /usr/bin and `sudo chmod +x path/to/ngrok`]

Moonscript: `sudo luarocks install moonscript`

### Battery Configuration
Run `curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/powertop.service" > /etc/systemd/system/powertop.service`

OR

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
    git config --global core.editor "atom-beta --wait"
    git config --global user.signingkey GPG_IDENTIFIER
    git config --global commit.gpgsign true

#### Atom Dependencies

##### Theme
`apm-beta install northem-dark-atom-ui northem-dark-atom-syntax`

##### Language Support
`apm-beta install atom-typescript language-babel language-glsl language-lua language-moonscript language-pug processing-language`

##### Linter
`apm-beta install busy-signal intentions linter linter-ui-default`

##### Linters
`apm-beta install linter-glsl linter-less linter-clang linter-xmllint linter-jsonlint linter-moonscript linter-js-standard-engine`

##### Other
`apm-beta install atom-clock atom-ternjs autocomplete-glsl docblockr file-icons filesize flow-ide git-plus highlight-selected keyboard-sounds minimap minimap-git-diff minimap-highlight-selected processing sync-settings toggle-packages termination tool-bar tool-bar-atom`

##### Dead Weight Packages
`apm-beta install pigments color-picker minimap-pigments`

##### Scala
`apm-beta install ensime language-scala`

`pacman -S scala sbt jre8-openjdk` and if you don't have jdk8-openjdk `pacman -S jdk8-openjdk`

##### Manual Installation
Go into the atom packages folder `cd ~/.atom/packages`.

Building from source:

1) Uninstall the package currently installed

2) Clone repo by `git clone HTTP_REPO_GIT`

3) Cd to the folder `cd FOLDER_NAME`

4) Install dependencies `npm i`

Build `Browser Plus` from source.
