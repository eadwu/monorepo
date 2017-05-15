#!/bin/bash
curl "https://raw.githubusercontent.com/kaketa/arch-x-os/master/files/pacman.conf" > /etc/pacman.conf &&
pacman -Sy pulseaudio alsa-utils &&
pacman -Sy yaourt &&
pacman -Sy networkmanager network-manager-applet gnome-keyring &&
pacman -Sy xf86-input-libinput xorg-server xorg-xinit &&
pacman -Sy ntp &&
pacman -Sy bluez bluez-utils blueman pulseaudio-bluetooth &&
curl "https://raw.githubusercontent.com/kaketa/arch-x-os/master/files/default.pa" > /etc/pulse/default.pa &&
curl "https://raw.githubusercontent.com/kaketa/arch-x-os/master/files/30-touchpad.conf" > /etc/X11/xorg.conf.d/30-touchpad.conf &&
pacman -Sy conky git openssh nodejs npm php redshift python-xdg blender chromium compton ark p7zip zip unzip unrar nitrogen rofi lsb-release luarocks lm_sensors powertop tlp cups avahi hplip noto-fonts-cjk ttf-liberation &&
curl "https://raw.githubusercontent.com/kaketa/arch-x-os/master/files/nsswitch.conf" > /etc/nsswitch.conf &&
pacman -Sy simplescreenrecorder thunar-volman gvfs &&
pacman -Sy lightdm lightdm-gtk-greeter lightdm-gtk-greeter-settings &&
pacman -Sy xfce4 xfce4-notifyd xfce4-screenshooter xfce4-taskmanager &&
curl "https://raw.githubusercontent.com/kaketa/arch-x-os/master/files/powertop.service" > /etc/systemd/system/powertop.service &&
systemctl daemon-reload &&
systemctl enable ntpd &&
systemctl enable bluetooth &&
systemctl enable org.cups.cupsd.service &&
systemctl enable avahi-daemon.service &&
systemctl disable netctl &&
systemctl enable NetworkManager &&
exit 0
