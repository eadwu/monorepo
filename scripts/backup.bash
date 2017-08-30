#!/usr/bin/env bash

## dotfiles
# cleaning
rm -rf dotfiles/.*
rm -rf dotfiles/conky
rm -rf dotfiles/i3
rm -rf dotfiles/compton.conf
# backup
cp $HOME/.zshrc dotfiles/.zshrc
cp $HOME/.vimrc dotfiles/.vimrc
cp $HOME/.Xresources dotfiles/.Xresources
cp $HOME/.config/compton.conf dotfiles/compton.conf
cp -r $HOME/.config/i3 dotfiles/i3
cp -r $HOME/.config/conky dotfiles/conky

mkdir dotfiles/.vim
mkdir dotfiles/.vim/autoload
mkdir dotfiles/.vim/colors
curl https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim > dotfiles/.vim/autoload/plug.vim
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/master/colors/nord.vim > dotfiles/.vim/colors/nord.vim

## pkg lists
pacman -Qqttn > pkg_lists/pacman.txt
pacman -Qqttm > pkg_lists/yaourt.txt
