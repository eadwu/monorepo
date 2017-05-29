#!/usr/bin/bash
# cgdisk /dev/sda
# cryptsetup luksFormat /dev/sda6
# cryptsetup open --type luks /dev/sda6 lvm
# pvcreate --dataalignment 1m /dev/mapper/lvm
# vgcreate volgroup0 /dev/mapper/lvm
# lvcreate -L 2GB volgroup0 -n lv_swap
# lvcreate -l 100%FREE volgroup0 -n lv_root
# modprobe dm_mod
# vgscan
# vgchange -ay
mkfs.ext4 /dev/sda5
mkfs.ext4 /dev/volgroup0/lv_root
mkswap /dev/volgroup0/lv_swap
swapon /dev/volgroup0/lv_swap
mount /dev/volgroup0/lv_root /mnt
mkdir /mnt/boot
mount /dev/sda5 /mnt/boot
curl "https://www.archlinux.org/mirrorlist/?country=US&protocol=http&protocol=https&ip_version=4" > /etc/pacman.d/mirrorlist.source
sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.source
rankmirrors -n 6 /etc/pacman.d/mirrorlist.source > /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel
genfstab -U -p /mnt >> /mnt/etc/fstab
nano /mnt/etc/fstab
