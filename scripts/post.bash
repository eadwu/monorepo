#!/usr/bin/env bash

# Usage
#   bash <(curl https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/post.bash) user

## Variables
user=$1

## Program
# Basic packages
perl -0777 -i -pe 's/#\[multilib\]\n#Include = \/etc\/pacman.d\/mirrorlist/\[multilib\]\nInclude = \/etc\/pacman.d\/mirrorlist\n\n\[archlinuxfr\]\nSigLevel = Never\nServer = http:\/\/repo.archlinux.fr\/\$arch\n\n\[herecura\]\nServer = https:\/\/repo.herecura.be\/herecura\/x86_64/' /etc/pacman.conf
pacman -Syy
pacman -S pulseaudio alsa-utils
pacman -S yaourt
pacman -S networkmanager network-manager-applet gnome-keyring
pacman -S xf86-input-libinput xorg-server xorg-xinit xorg-xwininfo mesa
pacman -S ntp
pacman -S bluez bluez-utils blueman pulseaudio-bluetooth
# Configuration
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
# More packages
pacman -S sddm
sudo -u ${user} yaourt -S i3-gaps-next-git
pacman -S compton i3lock thunar vgfs thunar-volman nitrogen rxvt-unicode
pacman -S acpid xfce4-notifyd xfce4-screenshooter xfce4-taskmanager
pacman -S zsh conky git openssh nodejs npm yarn php mysql-workbench vim docker openvpn redshift python-xdg python-fonttools blender opera opera-ffmpeg-codecs ark p7zip zip unzip unrar rofi lsb-release ranger feh cmake clang processing xclip glslang i3blocks luarocks oblogout lm_sensors powertop tlp cups avahi hplip broadcom-wl-dkms adapta-gtk-theme pepper-flash noto-fonts-cjk ttf-liberation
gpg --recv-keys 11E521D646982372EB577A1F8F0871F202119294
sudo -u ${user} yaourt -S oblogout-blurlock pa-applet monitorix atom-editor-beta-bin discord pamac-aur paper-icon-theme-git angular-cli gtk-theme-arc-git moonscript
sudo -u ${user} yaourt flow-bin
# More configuration
sddm --example-config > /etc/sddm.conf
cat <<EOF > /etc/acpi/events/power-button
event=button/power
action=/etc/acpi/actions/power-button.sh %e
EOF
cat <<EOF > /etc/acpi/events/lid
event=button/lid
action=/etc/acpi/actions/lid.sh %e
EOF
cat <<EOF > /etc/acpi/events/backlightdown
event=video/brightnessdown
action=/etc/acpi/handlers/backlight +
EOF
cat <<EOF > /etc/acpi/events/backlightup
event=video/brightnessdown
action=/etc/acpi/handlers/backlight -
EOF
mkdir -p /etc/acpi/handlers
cat <<EOF > /etc/acpi/handlers/backlight
#!/usr/bin/env sh
bl_dev=/sys/class/backlight/acpi_video0
step=1

case $1 in
  -)
    echo $(($(< $bl_dev/brightness) - $step)) > $bl_dev/brightness ;;
  +)
    echo $(($(< $bl_dev/brightness) + $step)) > $bl_dev/brightness ;;
esac
EOF
mkdir -p /etc/acpi/actions
cat <<EOF > /etc/acpi/actions/power-button.sh
#!/usr/bin/env sh

case "$2" in
  PBTN|PWRF)
    oblogout_blur ;;
  *) logger "ACPI action undefined: $2" ;;
esac
EOF
cat <<EOF > /etc/acpi/actions/lid.sh
#!/usr/bin/env sh

case "$3" in
  open)
    blurlock ;;
  close) logger 'LID closed' ;;
  *) logger "ACPI action undefined: $3" ;;
esac
EOF
chmod +x /etc/acpi/handlers/backlight
chmod +x /etc/acpi/actions/power-button.sh
chmod +x /etc/acpi/actions/lid.sh
perl -0777 -i -pe 's/(enabled = )n/$1y/' /etc/monitorix/monitorix.conf
perl -0777 -i -pe 's/hosts: (.+)(resolve \[!UNAVAIL=return\])(.+)\n/hosts: $1mdns_minimal \[NOTFOUND=return\] $2$3\n/' /etc/nsswitch.conf
# Enable all daemons/services
systemctl daemon-reload
systemctl enable acpid.service
systemctl enable fstrim.timer
systemctl enable ntpd
systemctl enable bluetooth
systemctl enable org.cups.cupsd.service
systemctl enable avahi-daemon.service
systemctl enable docker
systemctl enable sddm.service
systemctl enable NetworkManager
systemctl enable monitorix.service
cat <<EOF > /etc/systemd/system/powertop.service
[Unit]
Description=Powertop tunings

[Service]
Type=oneshot
ExecStart=/usr/bin/powertop --auto-tune

[Install]
WantedBy=multi-user.target
EOF
# Incorporate setup.bash
curl https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/setup.bash | bash -s -- ${user}
