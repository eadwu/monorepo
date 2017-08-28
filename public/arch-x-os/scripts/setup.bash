#!/usr/bin/env bash

### Variables
CONFIG=$HOME/.config
DOWNLOADS=$HOME/Downloads

### Zsh
## Set Zsh as default shell
chsh -s /bin/zsh
## Use Pure Prompt for the console
PURE_VERSION=$(curl https://api.github.com/repos/sindresorhus/pure/releases/latest | grep -e '\"tag_name\".*,' | cut -c 16- | rev | cut -c 3- | rev)
curl -Lo $DOWNLOADS/pure.zip https://github.com/sindresorhus/pure/archive/$PURE_VERSION.zip
unzip $DOWNLOADS/pure.zip
PURE=$DOWNLOADS/pure-$PURE_VERSION
mv $PURE/pure.zsh /usr/share/zsh/site-functions/prompt_pure_setup
mv $PURE/async.zsh /usr/share/zsh/site-functions/async

### Dotfiles
ZIP_COMMIT=$(curl https://gitlab.com/api/v4/projects/arch-dual-boot%2Farch-x-os/repository/commits | grep -o '\"id\":\"[^,]*' | head -1 | cut -c 7- | rev | cut -c 2- | rev)
curl -Lo $DOWNLOADS/arch-x-os.zip https://gitlab.com/arch-dual-boot/arch-x-os/repository/master/archive.zip
unzip $DOWNLOADS/arch-x-os.zip

DOTFILES=$DOWNLOADS/arch-x-os-master-$ZIP_COMMIT/dotfiles
mv $DOTFILES/.zshrc $HOME
mv $DOTFILES/.vimrc $HOME
mv $DOTFILES/.Xresources $HOME
mv $DOTFILES/.vim $HOME
mv $DOTFILES/compton.conf $CONFIG
mv $DOTFILES/i3 $CONFIG
mv $DOTFILES/i3status $CONFIG
mv $DOTFILES/conky $CONFIG
mv $DOTFILES/xflock4 /usr/local/bin
mv $DOTFILES/i3lock.service /etc/systemd/system
chmod +x /usr/local/bin/xflock4
chmod +x $CONFIG/conky/init-conky

systemctl daemon-reload
systemctl enable i3lock.service
systemctl start i3lock.service

### Fonts
## Operator and Input require a manual installation
curl -o $DOWNLOADS/awesome-font.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
curl -Lo $DOWNLOADS/fira-code-font.zip https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip
unzip $DOWNLOADS/awesome-font.zip
unzip $DOWNLOADS/fira-code-font.zip -d $DOWNLOADS/fira-code

mkdir -p $HOME/.local/share/fonts
FONTS=$HOME/.local/share/fonts

mv $DOWNLOADS/font-awesome-4.7.0/fonts/FontAwesome.otf $FONTS
mv $DOWNLOADS/fira-code/otf/FiraCode-Bold.otf $FONTS
mv $DOWNLOADS/fira-code/otf/FiraCode-Light.otf $FONTS
mv $DOWNLOADS/fira-code/otf/FiraCode-Medium.otf $FONTS
mv $DOWNLOADS/fira-code/otf/FiraCode-Regular.otf $FONTS
mv $DOWNLOADS/fira-code/otf/FiraCode-Retina.otf $FONTS
