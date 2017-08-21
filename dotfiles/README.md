### Other Stuff
Install awesome from [here](http://fontawesome.io/) into `~/.local/share/fonts`.

File manager __ranger__: `pacman -S ranger`. Run by running `ranger`.

Image Viewer __feh__: `pacman -S feh`

CLI Music: `pacman -Sy cmus` and visualizer `yaourt cli-visualizer` see [here](https://github.com/dpayne/cli-visualizer)

Statusline Plugin __powerline__: `pacman -S powerline`
### Installation
The __.zshrc__, __.vimrc__, and __.Xresources__ files and __.vim__ folder goes to your home directory `/home/$USER` or `~`.

The __compton.conf__ file and __i3__, __i3status__, and __conky__ folders go to the config directory `/home/$USER/.config` or `~/.config`.

The __xflock4__ file goes to `/usr/local/bin`.

The __i3lock.service__ file goes to `/etc/systemd/system` and then run `sudo systemctl daemon-reload`, `sudo systemctl enable i3lock.service`, and `sudo systemctl start i3lock.service`

The __xflock4__ and __init-conky__ files need to executable which can be done by running `sudo chmod +x PATH_TO_FILE`
