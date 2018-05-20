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
curl -sS "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kcore-meta-0.1.1-1-x86_64.pkg.tar.xz" -o kcore-meta.pkg.tar.xz
pacman -U kcore-meta.pkg.tar.xz
# Graphical Environment
curl -sS "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kenv-meta-0.1.0-2-x86_64.pkg.tar.xz" -o kenv-meta.pkg.tar.xz
pacman -U kenv-meta.pkg.tar.xz
# Additional Packages
curl -sS "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kother-meta-0.1.1-1-x86_64.pkg.tar.xz" -o kother-meta.pkg.tar.xz
pacman -U kother-meta.pkg.tar.xz
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
# Systemctl daemons/services
systemctl daemon-reload
systemctl enable fcron
systemctl enable fstrim.timer
systemctl enable ntpd
systemctl enable bluetooth
systemctl enable org.cups.cupsd.service
systemctl enable avahi-daemon.service
systemctl enable docker
systemctl enable lightdm.service
systemctl enable NetworkManager
# Cleanup
rm k*-meta.pkg.tar.xz
