#!/usr/bin/env bash

# cleaning
rm -rf dotfiles/.*
# backup
mkdir dotfiles/.config

cp ${HOME}/.config/compton.conf dotfiles/.config
cp -r ${HOME}/.config/conky dotfiles/conky
cp -r ${HOME}/.config/gtk-3.0 dotfiles/gtk-3.0

cp ${HOME}/.vimrc dotfiles
cp ${HOME}/.gtkrc-2.0 dotfiles

cp -r ${HOME}/.xmonad dotfiles/.xmonad

cp ${HOME}/.Xresources dotfiles

mkdir dotfiles/.vim
mkdir dotfiles/.vim/colors
mkdir dotfiles/.vim/autoload
mkdir dotfiles/.vim/autoload/airline
mkdir dotfiles/.vim/autoload/airline/themes

curl https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim > dotfiles/.vim/autoload/plug.vim
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/master/colors/nord.vim > dotfiles/.vim/colors/nord.vim
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/develop/autoload/airline/themes/nord.vim > dotfiles/.vim/autoload/airline/themes/nord.vim

cp ${HOME}/.zshrc dotfiles
# packages
pacman -Qqttn > pkg_lists/pacman.txt
pacman -Qqttm > pkg_lists/yaourt.txt
