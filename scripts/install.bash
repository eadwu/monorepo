#!/usr/bin/bash

# Usage
# Get the script then run it
# Example
# curl "https://raw.githubusercontent.com/arch-dual-boot/arch-x-os/master/scripts/install.sh" | bash abc123 US /America/New_York bahamut abc123 derp abc123

# Variables
lvmPassword=$1
country=$2 # From https://www.archlinux.org/mirrorlist
geographicZone=$3 # Format as /:Area/:Region like /America/New_York
hostname=$4
rootPassword=$5
user=$6
password=$7

# Partitioning
cgdisk /dev/sda
(echo "YES"; echo $lvmPassword; echo $lvmPassword) | cryptsetup -c aes-xts-plain64 --use-random luksFormat /dev/sda6
echo $lvmPassword | cryptsetup open --type luks /dev/sda6 lvm
pvcreate --dataalignment 1m /dev/mapper/lvm
vgcreate volgroup0 /dev/mapper/lvm
lvcreate -L 2G volgroup0 -n lv_swap
lvcreate -l 100%FREE volgroup0 -n lv_root
modprobe dm_mod
vgscan
vgchange -ay

# Formatting
mkfs.ext4 /dev/sda5
mkfs.ext4 /dev/volgroup0/lv_root
mkswap /dev/volgroup0/lv_swap
swapon /dev/volgroup0/lv_swap
mount /dev/volgroup0/lv_root /mnt
mkdir /mnt/boot
mount /dev/sda5 /mnt/boot

# Base installation
curl "https://www.archlinux.org/mirrorlist/?country=$country&protocol=http&protocol=https&ip_version=4" > /etc/pacman.d/mirrorlist.source
sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.source
rankmirrors -n 6 /etc/pacman.d/mirrorlist.source > /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel
genfstab -U -p /mnt >> /mnt/etc/fstab
nano /mnt/etc/fstab

# Arch-chroot script
cat <<EOF > /mnt/chroot.sh
(echo $rootPassword; echo $rootPassword) | passwd
pacman -S wpa_supplicant wireless_tools linux-headers
echo $hostname > /etc/hostname
ln -sf /usr/share/zoneinfo$geographicZone /etc/localtime
hwclock --systohc --utc
useradd -m -g users -G wheel -s /bin/bash $user
(echo $password; echo $password) | passwd $user
echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group
nano /etc/locale.gen
locale-gen
echo LANG=en_US.UTF-8 > /etc/locale.conf
export LANG=en_US.UTF-8
nano /etc/mkinitcpio.conf
mkinitcpio -p linux
pacman -S grub-efi-x86_64
nano /etc/default/grub
grub-mkconfig -o boot/grub/grub.cfg
grub-mkstandalone -o boot.efi -d usr/lib/grub/x86_64-efi -O x86_64-efi --compress=xz boot/grub/grub.cfg
mkdir /mnt/usbdisk && mount /dev/sdd1 /mnt/usbdisk
cp boot.efi /mnt/usbdisk
umount /mnt/usbdisk
exit
EOF

arch-chroot /mnt ./chroot.sh
umount /mnt/boot
umount /mnt
