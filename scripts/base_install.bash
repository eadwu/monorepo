#!/usr/bin/env bash

# USAGE:
#   curl -sS https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/base_install.bash | \
#     bash -s -- boot_partition crypt_password country geographic_zone hostname root_password user password locale dual_boot

# Variables
boot_partition=${1} # Boot Partition, normally /dev/sda{1|5}
crypt_password=${2}
country=${3} # From https://www.archlinux.org/mirrorlist
geographic_zone=${4} # Format as /:Area/:Region like /America/New_York
hostname=${5}
root_password=${6}
user=${7}
password=${8}
locale=${9} # Normally en_US.UTF-8
dual_boot=${10} # Whether or not this is a dual boot; true|false

root_partition=$[boot_partition+1]

# Partitioning
if [ "${dual_boot}" = true ]; then
  sgdisk -d 4 \
    -n 4:0:+128M \
    -n 5:+128M:+256M \
    -N 6 \
    -t 4:AF00 \
    -t 6:8E00 \
    /dev/sda
else
  sgdisk -o \
    -n 1:0:+512M \
    -N 2 \
    -t 1:EF00 \
    -t 2:8E00 \
    /dev/sda
fi
printf ${crypt_password} | cryptsetup -c aes-xts-plain64 --use-random luksFormat /dev/sda${root_partition} -
printf ${crypt_password} | cryptsetup open --type luks /dev/sda${root_partition} lvm -
pvcreate --dataalignment 1m /dev/mapper/lvm
vgcreate volgroup0 /dev/mapper/lvm
lvcreate -L 4G volgroup0 -n lv_swap
lvcreate -l 100%FREE volgroup0 -n lv_root
modprobe dm_mod
vgscan
vgchange -ay

# Formatting
if [ "${dual_boot}" = true ]; then mkfs.ext4 /dev/sda${boot_partition};
else mkfs.fat -F32 /dev/sda${boot_partition}; fi
mkfs.ext4 /dev/volgroup0/lv_root
mkswap /dev/volgroup0/lv_swap
swapon /dev/volgroup0/lv_swap
mount /dev/volgroup0/lv_root /mnt
mkdir /mnt/boot
mount /dev/sda${boot_partition} /mnt/boot

# Base installation
curl -sS "https://www.archlinux.org/mirrorlist/?country=${country}&protocol=http&protocol=https&ip_version=4&use_mirror_status=on" > /etc/pacman.d/mirrorlist.source
sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.source
rankmirrors -n 6 /etc/pacman.d/mirrorlist.source > /etc/pacman.d/mirrorlist
pacstrap /mnt base base-devel
genfstab -pU /mnt >> /mnt/etc/fstab

# Main Program
cat <<SOF > /mnt/core.sh
(echo ${root_password}; echo ${root_password}) | passwd
pacman -S intel-ucode wpa_supplicant wireless_tools linux-headers < /dev/tty
echo ${hostname} > /etc/hostname
ln -sf /usr/share/zoneinfo${geographic_zone} /etc/localtime
hwclock --systohc --utc
useradd -m -g users -G wheel -s /bin/bash ${user}
(echo ${password}; echo ${password}) | passwd ${user}
echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group
sed -i 's/^#\(${locale} UTF-8\)/\1/' /etc/locale.gen
locale-gen
echo LANG=${locale} > /etc/locale.conf
export LANG=${locale}
curl -sS "https://gitlab.com/arch-dual-boot/arch-x-os/raw/master/scripts/mkinitcpio.conf.patch" | patch -p1 /etc/mkinitcpio.conf
mkinitcpio -p linux
if [ "${dual_boot}" = true ]; then
  pacman -S grub < /dev/tty
  perl -0777 -i -pe "s/(GRUB_CMDLINE_LINUX_DEFAULT=\")quiet\"\n(GRUB_CMDLINE_LINUX=\")\"/\1cryptdevice=UUID=$(blkid | grep /dev/sda${root_partition} | grep -Po '(?<= UUID=\")[^ ]+(?=\")'):volgroup0 libata.force=1:noncq\"\n\2scsi_mod.use_blk_mq=y dm_mod.use_blk_mq=y\"/" /etc/default/grub
  cat <<EOF >> /etc/default/grub

# Fix broken grub.cfg gen
GRUB_DISABLE_SUBMENU=y
EOF
  grub-mkconfig -o boot/grub/grub.cfg
  grub-mkstandalone -o boot.efi -d usr/lib/grub/x86_64-efi -O x86_64-efi --compress=xz boot/grub/grub.cfg
  mkdir /mnt/usbdisk
  mount /dev/sdd1 /mnt/usbdisk
  cp boot.efi /mnt/usbdisk
  umount /mnt/usbdisk
  rm -rf boot.efi
else
  bootctl --path /boot install
  echo "default arch
timeout 5" > /boot/loader/loader.conf
  echo "title Arch Linux
linux /vmlinuz-linux
initrd /intel-ucode.img
initrd /initramfs-linux.img
options cryptdevice=UUID=$(blkid | grep /dev/sda${root_partition} | grep -Po '(?<= UUID=\")[^ ]+(?=\")'):volgroup0:allow-discards root=/dev/mapper/volgroup0-lv_root rw" > /boot/loader/entries/arch.conf
fi
exit
SOF

chmod +x /mnt/core.sh
arch-chroot /mnt ./core.sh
rm -rf /mnt/core.sh
umount -R /mnt
