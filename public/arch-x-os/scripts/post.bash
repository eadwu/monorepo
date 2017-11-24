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
pacman -S \
  pulseaudio pulseaudio-alsa pulseaudio-bluetooth \
  yaourt \
  networkmanager network-manager-applet gnome-keyring \
  xf86-input-libinput xorg-server xorg-xinit xorg-xwininfo \
  ntp \
  bluez bluez-libs bluez-utils
# Graphical Environment
pacman -S \
  apg pass pwgen \
  qt5-base qt5-doc \
  lightdm lightdm-gtk-greeter \
  haskell-xmonad haskell-xmonad-contrib xmobar haskell-hlint haskell-stack \
  compton thunar gvfs thunar-volman nitrogen rxvt-unicode cool-retro-term \
  xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
# Additional Packages
pacman -S \
  zsh vim \
  ark p7zip zip unzip unrar \
  broadcom-wl-dkms pepper-flash \
  powertop tlp cups avahi hplip \
  ruby cmake clang xclip openssh \
  adapta-gtk-theme deepin-gtk-theme \
  scrot cmatrix simplescreenrecorder \
  git npm yarn nodejs php glslang texlive-most \
  java-runtime-common java-environment-common jdk8-openjdk jdk9-openjdk \
  libreoffice-fresh hunspell hyphen hunspell-en hyphen-en libmythes mythes-en \
  luarocks lm_sensors python-pip python-pylint python2-pip python2-pylint python-fonttools lsb-release \
  fcron conky docker openvpn redshift rofi ranger feh cmus \
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
