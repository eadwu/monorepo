#!/usr/bin/env bash
perl -0777 -i -pe 's/#\[multilib\]\n#Include = \/etc\/pacman.d\/mirrorlist/\[multilib\]\nInclude = \/etc\/pacman.d\/mirrorlist\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch\n\n\[herecura\]\nServer = https:\/\/repo.herecura.be\/herecura\/x86_64/' /etc/pacman.conf
pacman -Syy
pacman -S pulseaudio alsa-utils
pacman -S yaourt
pacman -S networkmanager network-manager-applet gnome-keyring
pacman -S xf86-input-libinput xorg-server xorg-xinit xorg-xwininfo mesa
pacman -S ntp
pacman -S bluez bluez-utils blueman pulseaudio-bluetooth
cat <<EOF >> /etc/pulse/default.pa

### Automatically switch to newly-connected devices
load-module module-switch-on-connect

### Support for audio in Docker containers
load-module module-native-protocol-unix
EOF
cat <<EOF > /etc/X11/xorg.conf.d/30-touchpad.conf
Section "InputClass"
  Identifier "touchpad"
  Driver "libinput"
  MatchIsTouchpad "on"
  Option "ClickMethod" "buttonareas"
  Option "DisableWhileTyping" "off"
  Option "MiddleEmulation" "on"
EndSection
EOF
pacman -S zsh conky git hub openssh nodejs npm yarn php mysql-workbench vim docker openvpn redshift python-xdg python-fonttools blender opera opera-ffmpeg-codecs compton ark p7zip zip unzip unrar nitrogen rofi lsb-release ranger feh cmake clang processing xclip glslang i3status i3lock luarocks lm_sensors powertop tlp cups avahi hplip thunar-volman gvfs broadcom-wl-dkms adapta-gtk-theme pepper-flash noto-fonts-cjk ttf-liberation
perl -0777 -i -pe 's/hosts: (.+)(resolve \[!UNAVAIL=return\])(.+)\n/hosts: $1mdns_minimal \[NOTFOUND=return\] $2$3\n/' /etc/nsswitch.conf
pacman -S lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings
pacman -S xfce4 xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
systemctl daemon-reload
systemctl enable ntpd
systemctl enable bluetooth
systemctl enable org.cups.cupsd.service
systemctl enable avahi-daemon.service
systemctl enable docker
systemctl enable lightdm.service
systemctl enable NetworkManager
cat <<EOF > /etc/systemd/system/powertop.service
[Unit]
Description=Powertop tunings

[Service]
Type=oneshot
ExecStart=/usr/bin/powertop --auto-tune

[Install]
WantedBy=multi-user.target
EOF
