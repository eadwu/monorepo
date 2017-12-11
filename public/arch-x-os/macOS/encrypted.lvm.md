# **Encrypted LVM Arch Linux and macOS dual boot**
## Sources
Pandeiro's [Arch on Air](https://github.com/pandeiro/arch-on-air)

LearnLinux.tv's [Installing Arch Linux on Encrypted LVM](https://www.youtube.com/watch?v=gB1N00wj3bw)
## Requirements
See [requirements](./standard.md#requirements)
## Prerequisites
See [prerequisites](./standard.md#requirements)
## Procedure
#### Partitioning
The installation process for a LVM Encrypted Arch Linux is pretty similar to the standard installation.

Use `fdisk -l` and find out which of the partitions is your drive. For this, it'll be __/dev/sda__.

Adjust accordingly based on what yours is.

Run `cgdisk /dev/sda`. Controlling the menu here is easy, just use the arrow keys. **Please note: [enter] means just to press enter.**

Delete the partition you made beforehand and the amount of free space should be somewhere around the amount you partitioned for it.

Boot Loader Partition

    New
    [enter]
    +128Mib
    af00
    [enter]

Boot Partition

    New
    +128Mib
    +256Mib
    [enter]
    [enter]

Root Partition

    New
    [enter]
    [enter]
    8e00
    [enter]

The current partition setup for this currently:

    /dev/sda4 - Boot Loader
    /dev/sda5 - Boot Partition
    /dev/sda6 - Root Partition

Adjust the following commands based on your setup.

To set it up for LVM run `cryptsetup luksFormat /dev/sda6`, type __YES__ and then create a password for it.

To create the swap/roots partitions we will need access to the drive so run `cryptsetup open --type luks /dev/sda6 lvm` and enter your password.

__SSD Configuration : Run `pvcreate --dataalignment 1m /dev/mapper/lvm` otherwise run `pvcreate /dev/mapper/lvm`__

Run `vgcreate volgroup0 /dev/mapper/lvm`.

To create the swap partition run `lvcreate -L 2GB volgroup0 -n lv_swap`. Edit the size of this partition accordingly to your needs. It is also possible to skip this partition.

To create the root partition run `lvcreate -l 100%FREE volgroup0 -n lv_root`.

Then run `modprobe dm_mod`, `vgscan`, and `vgchange -ay`.

#### Formatting and Mounting Partitions

    mkfs.ext4 /dev/sda5
    mkfs.ext4 /dev/volgroup0/lv_root
    mkswap /dev/volgroup0/lv_swap
    swapon /dev/volgroup0/lv_swap

    mount /dev/volgroup0/lv_root /mnt
    mkdir /mnt/boot && mount /dev/sda5 /mnt/boot

#### Installation
Due to minimal difference between the steps, this is basically a copy and paste of the standard installation.

We will be setting up wireless here. You can use `wifi-menu` or [netctl](../resources/netctl.md). Verify you have internet access with `ping -c 3 google.com`

To set up the mirrorlist read [here](../resources/mirrorlist.md)

    pacstrap /mnt base base-devel
#### FStab
    genfstab -U -p /mnt >> /mnt/etc/fstab

**This next step is recommended for people with SSDs. See [here](https://wiki.archlinux.org/index.php/Solid_State_Drives#TRIM)**

Verify TRIM support by using `lsblk -D` and checking the DISC-GRAN and DISC-MAX columns for non-zero values.

**Operating in nano: Ctrl+O then enter to save file and Ctrl+X to exit nano**

Edit /mnt/etc/fstab by `nano /mnt/etc/fstab`. Add discard to the root and boot partitions.

    # /dev/mapper/volgroup0-lv_root
    UUID=PARTITION_UUID	/         	ext4      	defaults,noatime,discard,data=writeback		0 1

    # /dev/sda5
    UUID=PARTITION_UUID	/boot     	ext4      	defaults,relatime,stripe=4			0 2

#### System Configuration
The same as the standard installation essentially.

Get into root by `arch-chroot /mnt /bin/bash` and then set the root password by `passwd`.

**Wireless packages for post installation, if you're going to follow the post installation for this you should run `pacman -S wpa_supplicant wireless_tools` otherwise continue.**

OPTIONAL: Install intel-ucode by `pacman -S intel-ucode`

OPTIONAL: Install linux-headers by `pacman -S linux-headers`

Set the hostname by `echo HOST_NAME > /etc/hostname`.

Set your timezone by `ln -s /usr/share/zoneinfo/AREA/CITY /etc/localtime`. If you get an error, do `ln -sf /usr/share/zoneinfo/AREA/CITY /etc/localtime`.

Then do `hwclock --systohc --utc`.

Create your user by `useradd -m -g users -G wheel -s /bin/bash USER_NAME` and set its password by `passwd USER_NAME`.

Install sudo by `pacman -S sudo`. Grant sudo for terminal use by `echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group`.

Set up your locale by `nano /etc/locale.gen` and uncomment the one(s) that fit your criteria. For me this was **en_US.UTF-8**. Run `locale-gen` and `echo LANG=en_US.UTF-8 > /etc/locale.conf` and `export LANG=en_US.UTF-8`
#### Mkinitcpio
Run `nano /etc/mkinitcpio.conf` and make sure __HOOKS__ is like this

    HOOKS="base udev autodetect keyboard modconf block encrypt lvm2 filesystems fsck"

and then run `mkinitcpio -p linux`.

#### GRUB/EFI
**Explanations for this part can be found [here](https://github.com/pandeiro/arch-on-air#11-set-up-grubefi).**

Install GRUB-EFI by `pacman -S grub-efi-x86_64`.

Configure it by `nano /etc/default/grub` and edit GRUB_CMDLINE_LINUX_DEFAULT to be

    GRUB_CMDLINE_LINUX_DEFAULT="cryptdevice=/dev/sda6:volgroup0 quiet rootflags=data=writeback libata.force=1:noncq"

Add this snippet to file also.

    # fix broken grub.cfg gen
    GRUB_DISABLE_SUBMENU=y

Run the following command to build "boot.efi". __You may experience warnings here. I just ignored them and it still works fine.__

    grub-mkconfig -o boot/grub/grub.cfg
    grub-mkstandalone -o boot.efi -d usr/lib/grub/x86_64-efi -O x86_64-efi --compress=xz boot/grub/grub.cfg

#### Copying boot.efi
This is where you need your second USB. Plug it into a port and then run `fdisk -l` to find the partition. Here it would be /dev/sdd. Find the partition that is the USB's filesystem. It would most likely be in a FAT or FAT32 format. Here it will be /dev/sdd1.

Make a directory for the USB and mount it by `mkdir /mnt/usbdisk && mount /dev/sdd1 /mnt/usbdisk`.

Copy boot.efi by running `cp boot.efi /mnt/usbdisk`
#### Umounting and shutdown

    umount /mnt/usbdisk
    exit
    umount /mnt/boot
    umount /mnt
    shutdown now

#### Making boot.efi bootable
Format /dev/sda4 (the Boot Loader Partition) with the macOS Journaled filesystem. The name should be disk0s4.

Open up terminal and run the following.

    cd /Volumes/disk0s4
    mkdir System mach_kernel
    cd System
    mkdir Library
    cd Library
    mkdir CoreServices
    cd CoreServices
    touch SystemVersion.plist
    nano SystemVersion.plist

Put the following inside SystemVersion.plist.

    <xml version="1.0" encoding="utf-8"?>
    <plist version="1.0">
    <dict>
        <key>ProductBuildVersion</key>
        <string></string>
        <key>ProductName</key>
        <string>Linux</string>
        <key>ProductVersion</key>
        <string>Arch Linux</string>
    </dict>
    </plist>

Plug in your USB and then find out the device name. Here it will be Untitled.

Run `cp /Volumes/Untitled/boot.efi .` and assuming you didn't change the directory it should copy to /Volumes/disk0s4/System/Library/CoreServices.

The final structure should look like

    |___mach_kernel
    |___System
           |
           |___Library
                  |
                  |___CoreServices
                          |
                          |___SystemVersion.plist
                          |___boot.efi


To make the partition bootable you need to run `sudo bless --device /dev/disk0s4 --setBoot`.

This may not work and if it doesn't disable System Integrity Protection on macOS. Shutdown and boot into recovery mode by Command-R and then in the menu bar bring up Terminal (Utilities > Terminal). Run `csrutil disable` and restart.

Open up terminal and run `sudo bless --device /dev/disk0s4 --setBoot` and it should work now.

To reenable System Integrity Protection go back to recovery mode and its Terminal and run `csrutil enable`.

## Post Install
This is more like a reference for what I do after I finish this part. But if you want to see it look [here](../resources/post_install.md).
