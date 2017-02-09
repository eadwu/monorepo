# NTP or Network Time Protocol
Basically checks your time with internet time servers to verify it's accurate. See [here](https://wiki.archlinux.org/index.php/Network_Time_Protocol_daemon) for more information.

## Sources
Information here is mainly from LearnLinux.tv. See [here](https://www.youtube.com/watch?v=TNDisr6z7Rc).

## Installation
Run ```pacman -Sy ntp``` to install NTP.

Run ```systemctl daemon-reload``` and then ```systemctl enable ntpd```

If you are in a desktop environment and don't want to restart the computer then run ```systemctl start ntpd```
