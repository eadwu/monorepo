#!/usr/bin/env bash

# USAGE:
#   curl -sS https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/post.bash > post.bash && \
#     chmod +x post.bash && \
#     sudo ./post.bash virtualbox && \
#     rm post.bash

# Variables
virtualbox=${1} # |true

# Pacman Repositories
curl -sS "https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/pacman.conf.patch" | patch -p1 /etc/pacman.conf
pacman -Syy
pacman -Syu
# Core Packages
pacman -S kcore-meta
# Graphical Environment
pacman -S kenv-meta
# Additional Packages
pacman -S kother-meta
## Virtualbox Guest Utils
if [ "${virtualbox}" = true ]; then
  pacman -S virtualbox-guest-utils
  modprobe -a vboxguest vboxsf vboxvideo
  echo 'vboxguest
vboxsf
vboxvideo' > /etc/modules-load.d/virtualbox.conf
  cp /etc/X11/xinit/xinitrc ~/.xinitrc
  perl -0777 -i -pe 's/(#!\/bin\/sh)/\1\n\/usr\/bin\/VBoxClient-all/' ~/.xinitrc
fi
# Patches
curl -sS "https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/nsswitch.conf.patch" | patch -p1 /etc/nsswitch.conf
curl -sS "https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/default.pa.patch" | patch -p1 /etc/pulse/default.pa
curl -sS "https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/tlp.service.patch" | patch -p1 /usr/lib/systemd/system/tlp.service
# Systemctl daemons/services
systemctl daemon-reload
systemctl enable avahi-daemon.service
systemctl enable bluetooth
systemctl enable docker
systemctl enable fcron
systemctl enable fstrim.timer
systemctl enable lightdm.service
systemctl enable NetworkManager
systemctl enable ntpd
systemctl enable org.cups.cupsd.service
systemctl enable tlp
systemctl enable tlp-sleep
