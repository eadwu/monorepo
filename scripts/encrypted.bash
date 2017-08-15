#!/usr/bin/env bash

# Variables
cryptPassword=$1
country=$2 # From https://www.archlinux.org/mirrorlist
geographicZone=$3 # Format as /:Area/:Region like /America/New_York
hostname=$4
rootPassword=$5
user=$6
password=$7
locale=$8

# Partitioning
cgdisk /dev/sda < /dev/tty
printf $cryptPassword | cryptsetup -c aes-xts-plain64 --use-random luksFormat /dev/sda6 -
printf $cryptPassword | cryptsetup open --type luks /dev/sda6 lvm -
pvcreate --dataalignment 1m /dev/mapper/lvm
vgcreate volgroup0 /dev/mapper/lvm
lvcreate -L 4G volgroup0 -n lv_swap
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
nano /mnt/etc/fstab < /dev/tty

# Main Program
cat <<SOF > /mnt/chroot.sh
(echo $rootPassword; echo $rootPassword) | passwd
pacman -S wpa_supplicant wireless_tools linux-headers < /dev/tty
echo $hostname > /etc/hostname
ln -sf /usr/share/zoneinfo$geographicZone /etc/localtime
hwclock --systohc --utc
useradd -m -g users -G wheel -s /bin/bash $user
(echo $password; echo $password) | passwd $user
echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group
sed -i "s/^#$locale UTF-8/$locale UTF-8/" /etc/locale.gen
locale-gen
echo LANG=$locale > /etc/locale.conf
export LANG=$locale
sed -i 's/^HOOKS="base udev autodetect modconf block filesystems keyboard fsck"/HOOKS="base udev autodetect keyboard modconf block encrypt lvm2 filesystems fsck"/' /etc/mkinitcpio.conf
mkinitcpio -p linux
pacman -S grub-efi-x86_64 < /dev/tty
sed -i 's/^GRUB_CMDLINE_LINUX_DEFAULT="quiet"/GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=\/dev\/sda6:volgroup0 quiet rootflags=data=writeback libata.force=1:noncq"/' /etc/default/grub
cat <<EOF >> /etc/default/grub

# Fix broken grub.cfg gen
GRUB_DISABLE_SUBMENU=y
EOF
grub-mkconfig -o boot/grub/grub.cfg
grub-mkstandalone -o boot.efi -d usr/lib/grub/x86_64-efi -O x86_64-efi --compress=xz boot/grub/grub.cfg
mkdir /mnt/usbdisk && mount /dev/sdd1 /mnt/usbdisk
cp boot.efi /mnt/usbdisk
umount /mnt/usbdisk
rm -rf boot.efi
exit
SOF

chmod +x /mnt/chroot.sh
arch-chroot /mnt ./chroot.sh
rm -rf /mnt/chroot.sh
umount /mnt/boot
umount /mnt
