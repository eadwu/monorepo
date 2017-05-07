# Setting up Pacman's mirrolist
Getting the right mirrors for fastest download speeds.

Go to the Arch Linux [mirrorlist generator](https://www.archlinux.org/mirrorlist/) and then use the options given to generate your regional mirrorlist and record the url/link. For this, the link will be `https://www.archlinux.org/mirrorlist/?country=US&protocol=http&protocol=https&ip_version=4`

Get the mirrorlist onto the local filesystem by running `curl "https://www.archlinux.org/mirrorlist/?country=US&protocol=http&protocol=https&ip_version=4" > /etc/pacman.d/mirrorlist.source`

Uncomment the Servers with `sed -i 's/^#Server/Server/' /etc/pacman.d/mirrorlist.source`

Then get the top 6 mirrors by running `rankmirrors -n 6 /etc/pacman.d/mirrorlist.source > /etc/pacman.d/mirrorlist`

Now cross reference with the [mirror status](https://www.archlinux.org/mirrors/status/) and comment out the mirrors that are out of sync if any exist.
