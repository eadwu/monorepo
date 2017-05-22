# **Standard macOS and Arch Linux dual boot**
## Sources
Pandeiro's [Arch on Air](https://github.com/pandeiro/arch-on-air)

Mark H. Nichols's [Arch Linux on a MacBook Pro Part 3: Base Installation](http://zanshin.net/2015/02/05/arch-linux-on-a-macbook-pro-part-3-base-installation/)

LearnLinux.tv's [Installing Arch Linux (Standard Procedure)](https://www.youtube.com/watch?v=lizdpoZj_vU)
## **Requirements**
Wifi access

2 USB Drives
## **Prerequisites**
Partition the hard drive using DiskUtility.

Download the [Arch Linux ISO](https://www.archlinux.org/download/) and burn it into a USB/CD.

Boot into the USB/CD.

## **Procedure**
#### **Partitioning**
Here we will be creating 4 partitions for Arch Linux. Adjust accordingly to your preferences.

Use `fdisk -l` to display the hard drive partitions. Find out which one is your hard drive. Here it will be /dev/sda

Run `cgdisk /dev/sda`

Use the arrow keys to control the menu.

Delete the partition you created beforehand. And make your partition target the Free Space which should be at the bottom of the partitions.

**[enter] basically means to just press enter**

Boot Loader Partition

    New
    [enter]
    +128Mib
    af00
    [enter]

Boot partition

**There is suppose to be free space. Read more about Apple's partitioning policy [here](https://developer.apple.com/library/content/technotes/tn2166/_index.html#//apple_ref/doc/uid/DTS10003927-CH1-SUBSECTION5).**

    New
    +128Mib
    +256Mib
    [enter]
    [enter]

Swap partition

***The size of this partition is opinion based. Some say it isn't needed but I usually just make a 2GB partition when I have 4GB RAM built-in with no problems so far.***

**If you don't want to create a swap partition but a swap file follow the instructions [here](https://github.com/pandeiro/arch-on-air/blob/master/README.org#4-format-and-mount-partitions)**

    New
    [enter]
    +2Gib
    8200
    [enter]

Root partition

    New
    [enter]
    [enter]
    [enter]
    [enter]

#### Formatting and Mounting Partitions

If you followed the above directions and assuming you only have macOS then

/dev/sda4 should be the Boot Loader partition

/dev/sda5 should be the Boot partition

/dev/sda6 should be the Swap partition

/dev/sda7 should be the Root partition

    mkfs.ext4 /dev/sda5
    mkfs.ext4 /dev/sda7
    mkswap /dev/sda6
    swapon /dev/sda6

    mount /dev/sda7 /mnt
    mkdir /mnt/boot && mount /dev/sda5 /mnt/boot

#### Installation
We will be setting up wireless here. You can use `wifi-menu` or [netctl](https://github.com/eadwu/arch-x-os/blob/master/resources/netctl.md). Verify you have internet access with `ping -c 3 google.com`

To set up the mirrorlist read [here](https://github.com/eadwu/arch-x-os/blob/master/resources/mirrorlist.md)

    pacstrap /mnt base base-devel   
#### FStab
    genfstab -U -p /mnt >> /mnt/etc/fstab   

**This next step is recommended for people with SSDs. See [here](https://wiki.archlinux.org/index.php/Solid_State_Drives#TRIM)**

Verify TRIM support by using `lsblk -D` and checking the DISC-GRAN and DISC-MAX columns for non-zero values.

**Operating in nano: Ctrl+O then enter to save file and Ctrl+X to exit nano**

Edit /mnt/etc/fstab by `nano /mnt/etc/fstab`. Add discard to the root and boot partitions.

    /dev/sda7 /         ext4 defaults,noatime,discard,data=writeback      0 1
    /dev/sda5 /boot     ext4 defaults,relatime,stripe=4                   0 2

#### System Configuration
Get into root by `arch-chroot /mnt /bin/bash` and then set the root password by `passwd`.

**Wireless packages for post installation, if you're going to follow the post installation for this you should run `pacman -S wpa_supplicant wireless_tools` otherwise continue.**

OPTIONAL: Install linux-headers by `pacman -S linux-headers`

Set the hostname by `echo HOST_NAME > /etc/hostname`.

Set your timezone by `ln -s /usr/share/zoneinfo/AREA/CITY /etc/localtime`. If you get an error, do `ln -sf /usr/share/zoneinfo/AREA/CITY /etc/localtime`.

Then do `hwclock --systohc --utc`.

Create your user by `useradd -m -g users -G wheel -s /bin/bash USER_NAME` and set its password by `passwd USER_NAME`.

Install sudo by `pacman -S sudo`. Grant sudo for terminal use by `echo "%wheel ALL=(ALL) ALL" > /etc/sudoers.d/10-grant-wheel-group`.

Set up your locale by `nano /etc/locale.gen` and uncomment the one(s) that fit your criteria. For me this was **en_US.UTF-8**. Run `locale-gen` and `echo LANG=en_US.UTF-8 > /etc/locale.conf` and `export LANG=en_US.UTF-8`
#### Mkinitcpio
Add "keyboard" after "autodetect" if it doesn't exist by `nano /etc/mkinitcpio.conf` and then running it with `mkinitcpio -p linux`
#### GRUB/EFI
**Explanations for this part can be found [here](https://github.com/pandeiro/arch-on-air#11-set-up-grubefi).**

Install GRUB-EFI by `pacman -S grub-efi-x86_64`.

Configure it by `nano /etc/default/grub` and edit GRUB_CMDLINE_LINUX_DEFAULT to be `GRUB_CMDLINE_LINUX_DEFAULT="quiet rootflags=data=writeback libata.force=1:noncq"`.

Add this snippet to file also.

    # fix broken grub.cfg gen
    GRUB_DISABLE_SUBMENU=y

Run the following command to build "boot.efi".

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
This is more like a reference for what I do after I finish this part. But if you want to see it look [here](https://github.com/eadwu/arch-x-os/blob/master/resources/post_install.md).
