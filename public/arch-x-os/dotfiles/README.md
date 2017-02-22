### Dependencies
Install awesome from [here](http://fontawesome.io/) into `~/.local/share/fonts`.

To change wallpapers in i3 you need __nitrogen__: `pacman -Sy nitrogen`.

i3 Compositor __compton__: `pacman -Sy compton`.

D-Menu alternative __rofi__: `pacman -Sy rofi`.

i3lock blurlock function __oblogout-blurlock__: `yaourt oblogout-blurlock`

D-Menu alternative 2 __morc_menu__: Build manually from [here](https://github.com/Boruch-Baum/morc_menu#setup).
### Installation
The __.Xresources__ file goes to your home directory `/home/$USER` or `~`.

The __compton.conf__ file goes to the config directory `/home/$USER/.config` or `~/.config`.

The __i3exit__ and __i3lock__ files goto `/usr/bin`. This can be done by `sudo cp PATH_TO_i3exit /usr/bin` and `sudo cp PATH_TO_i3lock /usr/bin`. You made need to make __i3exit__ executable and this can be done via `sudo chmod +x /usr/bin/i3exit`

The __i3__, __i3status__, and __conky__ folders goto the config directory also. `home/$USER/.config` or `~/.config`.
