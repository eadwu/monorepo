passwd
pacman -S wpa_supplicant wireless_tools linux-headers
echo bahamut > /etc/hostname
ln -sf /usr/share/zoneinfo/America/New_York /etc/localtime
hwclock --systohc --utc
useradd -m -g users -G wheel -s /bin/bash ead
passwd ead
echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group
nano /etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 > /etc/locale.conf
export LANG=en_US.UTF-8
nano /etc/mkinitcpio.conf
mkinitcpio -p linux
pacman -S grub-efi-x86_64
nano /etc/default/grub
# grub-mkconfig -o boot/grub/grub.cfg
# grub-mkstandalone -o boot.efi -d usr/lib/grub/x86_64-efi -O x86_64-efi --compress=xz boot/grub/grub.cfg
