#!/bin/bash
curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/pacman.conf" > /etc/pacman.conf
pacman -Syy
pacman -S pulseaudio alsa-utils
pacman -S yaourt
pacman -S networkmanager network-manager-applet gnome-keyring
pacman -S xf86-input-libinput xorg-server xorg-xinit
pacman -S ntp
pacman -S bluez bluez-utils blueman pulseaudio-bluetooth
curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/default.pa" > /etc/pulse/default.pa
curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/30-touchpad.conf" > /etc/X11/xorg.conf.d/30-touchpad.conf
pacman -S conky git openssh nodejs npm php redshift python-xdg blender chromium compton ark p7zip zip unzip unrar nitrogen rofi lsb-release powerline processing xclip glslang i3status luarocks lm_sensors powertop tlp cups avahi hplip thunar-volman gvfs noto-fonts-cjk ttf-liberation
curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/nsswitch.conf" > /etc/nsswitch.conf
pacman -S lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings
pacman -S xfce4 xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
systemctl daemon-reload
systemctl enable ntpd
systemctl enable bluetooth
systemctl enable org.cups.cupsd.service
systemctl enable avahi-daemon.service
systemctl enable lightdm.service
systemctl disable netctl
systemctl enable NetworkManager
curl "https://raw.githubusercontent.com/eadwu/arch-x-os/master/files/powertop.service" > /etc/systemd/system/powertop.service
