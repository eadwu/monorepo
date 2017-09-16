#!/usr/bin/env bash

# USAGE:
#   curl https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/post.bash > post.bash && \
#     chmod +x post.bash && \
#     sudo ./post.bash virtualbox && \
#     rm post.bash

# Variables
virtualbox=${1} # |true

# Pacman Repositories
perl -0777 -i -pe 's/#(\[multilib\])\n#(Include = \/etc\/pacman.d\/mirrorlist)/\1\n\2\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch\n\n\[herecura\]\nServer = https:\/\/repo.herecura.be\/herecura\/x86_64/' /etc/pacman.conf
pacman -Sy
# Core Packages
pacman -S \
  pulseaudio pulseaudio-alsa pulseaudio-bluetooth \
  yaourt \
  networkmanager network-manager-applet gnome-keyring \
  xf86-input-libinput xorg-server xorg-xinit xorg-xwininfo \
  ntp \
  bluez bluez-libs bluez-utils
# Graphical Environment
pacman -S \
  lightdm lightdm-gtk-greeter \
  xmonad xmonad-contrib xmobar hlint \
  compton thunar gvfs thunar-volman nitrogen rxvt-unicode \
  xfce4-screenshooter xfce4-taskmanager
# Additional Packages
pacman -S \
  zsh vim \
  ark p7zip zip unzip unrar \
  cmake clang xclip openssh \
  broadcom-wl-dkms pepper-flash \
  powertop tlp cups avahi hplip \
  git npm yarn nodejs php glslang \
  adapta-gtk-theme deepin-gtk-theme \
  conky docker openvpn redshift rofi ranger feh cmus \
  luarocks lm_sensors python-fonttools lsb-release scrot cmatrix \
  blender opera-developer opera-developer-ffmpeg-codecs \
  noto-fonts-cjk ttf-liberation
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
load-module module-switch-on-connect

### Support for audio in Docker containers
load-module module-native-protocol-unix' >> /etc/pulse/default.pa
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
