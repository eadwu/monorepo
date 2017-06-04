#!/usr/bin/env bash
perl -0777 -i -pe 's/#\[multilib\]\n#Include = \/etc\/pacman.d\/mirrorlist/\[multilib\]\nInclude = \/etc\/pacman.d\/mirrorlist\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch/' /etc/pacman.conf
pacman -Syy
pacman -S pulseaudio alsa-utils
pacman -S yaourt
pacman -S networkmanager network-manager-applet gnome-keyring
pacman -S xf86-input-libinput xorg-server xorg-xinit
pacman -S ntp
pacman -S bluez bluez-utils blueman pulseaudio-bluetooth
cat <<EOF >> /etc/pulse/default.pa

### Automatically switch to newly-connected devices
load-module module-switch-on-connect
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
pacman -S conky git openssh nodejs npm php redshift python-xdg blender chromium compton ark p7zip zip unzip unrar nitrogen rofi lsb-release powerline clang processing xclip glslang i3status luarocks lm_sensors powertop tlp cups avahi hplip thunar-volman gvfs noto-fonts-cjk ttf-liberation
perl -0777 -i -pe 's/hosts: (.+)(resolve \[!UNAVAIL=return\])(.+)\n/hosts: $1mdns_minimal \[NOTFOUND=return\] $2$3\n/' /etc/nsswitch.conf
pacman -S lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings
pacman -S xfce4 xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
systemctl daemon-reload
systemctl start org.cups.cupsd.service
systemctl disable netctl
systemctl enable ntpd
systemctl enable bluetooth
systemctl enable org.cups.cupsd.service
systemctl enable avahi-daemon.service
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
