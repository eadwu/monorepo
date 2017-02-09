# Setting up netctl
Mainly just for the setting up, later this won't really be needed.

This is assuming your wifi password is WPA/WPA2 encrypted.

Copy the configuration file ```cp /etc/netctl/examples/wireless-wpa /etc/netctl/temp-wifi```.

Get your wifi card device by ```ip a```. It should start with w. For this it will be **wlp2s0**

Edit the copied configuration file by ```nano /etc/netctl/temp-wifi```.

Set "Interface" to the device, ```Interface=wlp2s0```.

Set "ESSID" to the network hostname.

Set "Key" to the network password.

Save and exit.

Run it by ```netctl start temp-wifi``` and then ```ping -c 3 google.com``` to verify you have an internet connection.

## Source
https://youtu.be/lizdpoZj_vU?t=12m30s
