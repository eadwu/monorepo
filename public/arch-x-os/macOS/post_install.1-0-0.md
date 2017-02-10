# Post Installation
Setup your wifi with [netctl](https://github.com/Kutoru/arch-x-os/blob/master/macOS/netctl.1-0-0.md) and run ```ping -c 3 google.com``` to verify that you have a connection.

## Sources
LearnLinux.tv, see [here](https://www.youtube.com/watch?v=GCUmGtCYPWM)

Tech Linux, see [here](https://www.youtube.com/watch?v=DtohxreWjVg)

#### Audio
Run ```pacman -Sy alsa-utils``` and then ```alsamixer```. Unmute by pressing M and bring the scales up until the dB gain is 0.00. "The MM label below a channel indicates that the channel is muted, and 00 indicates that it is open." -- [wiki.archlinux.org](https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting)

#### Editing /etc/pacman.conf
Run ```nano /etc/pacman.conf```
Uncomment
```
[multilib]
Include=/etc/pacman.d/mirrorlist
```
for 32-bit package installation.
Add the following for AUR package support
```
[archlinuxfr]
SigLevel = Never
Server = http://repo.archlinux.fr/$arch
```
Run ```pacman -Sy yaourt``` to install yaourt. Verify yaourt is installed by ```which yaourt```.

#### Wireless Connection
Run ```pacman -Sy networkmanager network-manager-applet wireless_tools wpa_supplicant gnome-keyring```

#### XOrg and Trackpad Driver
Run ```pacman -Sy xf86-input-libinput xorg-server xorg-xinit xorg-server-utils mesa```

#### Graphic Drivers
This is assuming that you have Intel Graphics.
If not look [here](https://wiki.archlinux.org/index.php/xorg#Driver_installation) or for a spreadsheet made by other Linux users look [here](https://docs.google.com/spreadsheets/d/1nG9Y9nhA615IkjNUE_ew7JmiVHD7ORA4BExW0-teQ40/edit#gid=897452601).
**Remember that the distro is Arch Linux.**

Run ```pacman -Sy xf86-video-intel mesa-libgl``` to install the drivers for Intel cards.

If you want the 32-bit drivers as well then run ```pacman -Sy lib32-intel-dri lib32-mesa lib32-libgl```

#### Optional
If you're planning to run XFCE you should also run the following. Run ```yaourt -S xfce4-indicator-plugin``` then ```systemctl disable netctl.service``` to disable netctl and then to enable NetworkManager run ```systemctl enable NetworkManager.service```. For USB Mounting support run ```pacman -Sy thunar-volman gvfs```.

#### NTP (Network Time Protocol)
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/ntp.1-0-0.md).

#### Bluetooth
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/bluetooth.1-0-0.md).

#### Trackpad Configuration
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/trackpad.1-0-0.md)

#### Git, Node.js, Coffeescript, Atom, and Redshift
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/extra.1-0-1.md).

#### Desktop Manager/Environment
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/graphical-environment.1-0-0.md)

#### Conky
Follow the instructions [here](https://github.com/Kutoru/arch-x-os/blob/master/macOS/conky.1-0-0.md)
