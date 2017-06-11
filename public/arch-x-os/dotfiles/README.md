### Other Stuff
Install awesome from [here](http://fontawesome.io/) into `~/.local/share/fonts`.

File manager __ranger__: `pacman -Sy ranger`. Run by running `ranger`.

Image Viewer __feh__: `pacman -Sy feh`

CLI Music: `pacman -Sy cmus` and visualizer `yaourt cli-visualizer` see [here](https://github.com/dpayne/cli-visualizer)
### Installation
The __.bashrc__, __.vimrc__, __.Xresources__ files goes to your home directory `/home/$USER` or `~`.

The __compton.conf__ file and __.vim__, __i3__, __i3status__, __vis__, and __conky__ folders go to the config directory `/home/$USER/.config` or `~/.config`.

The __i3exit__ and __i3lock__ files goes to `/usr/bin`.

The __i3exit.service__ file goes to `/etc/systemd/system` and then run `sudo systemctl daemon-reload`, `sudo systemctl enable i3exit.service`, and `sudo systemctl start i3exit.service`

The __i3exit__, __i3lock__, and __init-conky__ files need to executable which can be done by running `sudo chmod +x PATH_TO_FILE`
