# Bluetooth
Setting up Bluetooth

## Sources
LearnLinux.tv, see [here](https://www.youtube.com/watch?v=TNDisr6z7Rc)

wiki.archlinux.org, see [here](https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting) and [here](https://wiki.archlinux.org/index.php/Bluetooth_headset)

https://wiki.archlinux.org/index.php/PulseAudio/Troubleshooting

## Installation
Run ```pacman -Sy bluez bluez-utils blueman```

Then run ```systemctl daemon-reload```

Run ```systemctl enable bluetooth``` to enable bluetooth

#### Headset Autoconnection
Edit /etc/pulse/default.pa by running ```nano /etc/pulse/default.pa``` and add the following snippet to it.
```
### automatically switch to newly-connected devices
load-module module-switch-on-connect
```

#### Headset connected but no sound
Find your headset from running ```pacmd ls``` and then get the index value of your headset and run ```pacmd set-card-profile INDEX_HERE a2dp_sink```.
