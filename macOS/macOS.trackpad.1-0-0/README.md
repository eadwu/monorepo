# Trackpad configuration
The default Apple trackpad movements is clickfinger based (see [here](https://wayland.freedesktop.org/libinput/doc/latest/clickpad_softbuttons.html#clickfinger)). This changes it to a button area based format (see [here](https://wayland.freedesktop.org/libinput/doc/latest/clickpad_softbuttons.html#software_buttons)). This also changes the Middle Mouse to be clicking the left and right buttons at the same time inside of the center of the trackpad.

#### Configuring Left/Right/Middle click
Generate ```30-touchpad.conf``` by running ```sudo nano /etc/X11/xorg.conf.d/30-touchpad.conf``` and put the following snippet inside it.
```
Section "InputClass"
  Identifier "touchpad"
  Driver "libinput"
  MatchIsTouchpad "on"
  Option "ClickMethod" "buttonareas"
  Option "MiddleEmulation" "on"
EndSection
```
