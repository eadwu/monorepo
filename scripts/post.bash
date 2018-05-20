#!/usr/bin/env bash

# USAGE:
#   curl https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/post.bash > post.bash && \
#     chmod +x post.bash && \
#     sudo ./post.bash virtualbox && \
#     rm post.bash

# Variables
virtualbox=${1} # |true

# Pacman Repositories
perl -0777 -i -pe 's/#(\[multilib\]\n)#(Include = \/etc\/pacman.d\/mirrorlist)/\1\2\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch\n\n\[herecura\]\nServer = https:\/\/repo.herecura.be\/herecura\/x86_64/s' /etc/pacman.conf
pacman -Syy
pacman -Syu
# Core Packages
curl "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kcore-meta-0.1.0-1-x86_64.pkg.tar.xz" -o kcore-meta-0.1.0-1-x86_64.pkg.tar.xz
pacman -U kcore-meta-0.1.0-1-x86_64.pkg.tar.xz
# Graphical Environment
curl "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kenv-meta-0.1.0-1-x86_64.pkg.tar.xz" -o kenv-meta-0.1.0-1-x86_64.pkg.tar.xz
pacman -U kenv-meta-0.1.0-1-x86_64.pkg.tar.xz
# Additional Packages
curl "https://gitlab.com/eadwu/pkgbuilds/raw/master/x86_64/kother-meta-0.1.0-2-x86_64.pkg.tar.xz" -o kother-meta-0.1.0-2-x86_64.pkg.tar.xz
pacman -U kother-meta-0.1.0-2-x86_64.pkg.tar.xz
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
