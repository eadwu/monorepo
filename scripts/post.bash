#!/usr/bin/env bash

# USAGE:
#   curl https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/post.bash > post.bash && \
#     chmod +x post.bash && \
#     sudo ./post.bash virtualbox && \
#     rm post.bash

# Variables
virtualbox=${1} # |true

# Pacman Repositories
perl -0777 -i -pe 's/(\[extra\]\nInclude = \/etc\/pacman.d\/mirrorlist)(.+?)#(\[multilib\]\n)#(Include = \/etc\/pacman.d\/mirrorlist)/\1\n\n\[haskell-core\]\nSigLevel = Required TrustedOnly\nServer = http:\/\/xsounds.org\/~haskell\/core\/\$arch\2\3\4\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch\n\n\[herecura\]\nServer = https:\/\/repo.herecura.be\/herecura\/x86_64/s' /etc/pacman.conf
pacman-key -r 4209170B
pacman-key --lsign-key 4209170B
pacman -Syy
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
# Configuration
perl -0777 -i -pe 's/hosts: (.+)(resolve \[!UNAVAIL=return\])(.+)\n/hosts: \1mdns_minimal \[NOTFOUND=return\] \2\3\n/' /etc/nsswitch.conf
echo '
### Automatically switch to newly-connected devices
load-module module-switch-on-connect' >> /etc/pulse/default.pa
echo 'Section "InputClass"
  Identifier "touchpad"
  Driver "libinput"
  MatchIsTouchpad "on"
  Option "ClickMethod" "buttonareas"
  Option "DisableWhileTyping" "off"
  Option "MiddleEmulation" "on"
EndSection' > /etc/X11/xorg.conf.d/30-touchpad.conf
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
echo '[Unit]
Description=Powertop tunings

[Service]
Type=oneshot
ExecStart=/usr/bin/powertop --auto-tune

[Install]
WantedBy=multi-user.target' > /etc/systemd/system/powertop.service
