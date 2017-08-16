#!/usr/bin/env bash

## Variables
CONFIG=$HOME/.config
DIRECTORY=$HOME/Downloads

## Dotfiles
curl -o $DIRECTORY/arch-x-os-master.zip https://codeload.github.com/arch-dual-boot/arch-x-os/zip/master
# curl -Lo $DIRECTORY/atom-config.zip https://gist.github.com/eadwu/3cf21fa5a80c6902213a3e1960cd9d3b/archive/33307b1eed4b80bbff458fd8ae5155ba7dfbeb7d.zip
unzip $DIRECTORY/arch-x-os-master.zip
# unzip $DIRECTORY/atom-config.zip
DOTFILES=$DIRECTORY/arch-x-os-master/dotfiles
mv $DOTFILES/.bashrc $HOME
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

## Fonts
# curl -o $DIRECTORY/input-font.zip http://input.fontbureau.com/build/?fontSelection=fourStyleFamily&regular=InputMonoNarrow-Light&italic=InputMonoNarrow-LightItalic&bold=InputMonoNarrow-Medium&boldItalic=InputMonoNarrow-MediumItalic&a=0&g=ss&i=serif&l=serifs_round&zero=slash&asterisk=height&braces=straight&preset=dejavu&line-height=1.2&accept=I+do&email=
curl -o $DIRECTORY/awesome-font.zip http://fontawesome.io/assets/font-awesome-4.7.0.zip
curl -o $DIRECTORY/flottflott-font.zip http://dl.dafont.com/dl/?f=flottflott
curl -Lo $DIRECTORY/fira-code-font.zip https://github.com/tonsky/FiraCode/releases/download/1.204/FiraCode_1.204.zip
# unzip $DIRECTORY/input-font.zip -d $DIRECTORY/Input
unzip $DIRECTORY/awesome-font.zip
unzip $DIRECTORY/flottflott-font.zip -d $DIRECTORY/flottflott
unzip $DIRECTORY/fira-code-font.zip -d $DIRECTORY/fira-code
mkdir -p $HOME/.local/share/fonts
FONT=$HOME/.local/share/fonts
mv $DIRECTORY/font-awesome-4.7.0/fonts/fontawesome-webfont.ttf $FONT/fontawesome-webfont.ttf
mv $DIRECTORY/flottflott/Flottflott.ttf $FONT/Flottflott.ttf
mv $DIRECTORY/fira-code/ttf/FiraCode-Bold.ttf $FONT/FiraCode-Bold.ttf
mv $DIRECTORY/fira-code/ttf/FiraCode-Light.ttf $FONT/FiraCode-Light.ttf
mv $DIRECTORY/fira-code/ttf/FiraCode-Medium.ttf $FONT/FiraCode-Medium.ttf
mv $DIRECTORY/fira-code/ttf/FiraCode-Regular.ttf $FONT/FiraCode-Regular.ttf
mv $DIRECTORY/fira-code/ttf/FiraCode-Retina.ttf $FONT/FiraCode-Retina.ttf
