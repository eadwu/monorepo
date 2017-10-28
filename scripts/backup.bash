#!/usr/bin/env bash

# clean directory
rm -rf dotfiles/.config
rm -rf dotfiles/.vim
rm -rf dotfiles/etc
rm -rf dotfiles/usr/local
# directories
mkdir dotfiles/.config
mkdir dotfiles/.vim
mkdir dotfiles/etc
# variables
CONFIG=dotfiles/.config
VIM=dotfiles/.vim
ETC=dotfiles/etc
USR=dotfiles/usr

# home
cp ${HOME}/.gtkrc-2.0 dotfiles
cp ${HOME}/.vimrc dotfiles
cp ${HOME}/.Xresources dotfiles
cp ${HOME}/.zshrc dotfiles

##  .config
cp ${HOME}/.config/compton.conf ${CONFIG}
###   gtk-3.0
mkdir ${CONFIG}/gtk-3.0
cp ${HOME}/.config/gtk-3.0/settings.ini ${CONFIG}/gtk-3.0
###   vis
mkdir ${CONFIG}/vis
mkdir ${CONFIG}/vis/colors
cp ${HOME}/.config/vis/config ${CONFIG}/vis
cp ${HOME}/.config/vis/colors/colors ${CONFIG}/vis/colors

##  .vim
mkdir ${VIM}/autoload
###   autoload
mkdir ${VIM}/autoload/airline
curl https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim > ${VIM}/autoload/plug.vim
####    airline
mkdir ${VIM}/autoload/airline/themes
#####     themes
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/develop/autoload/airline/themes/nord.vim > ${VIM}/autoload/airline/themes/nord.vim
###   colors
mkdir ${VIM}/colors
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/master/colors/nord.vim > ${VIM}/colors/nord.vim

##  etc
mkdir ${ETC}/lightdm
mkdir ${ETC}/sysctl.d
mkdir ${ETC}/systemd
cp /etc/oblogout.conf ${ETC}
###   lightdm
cp /etc/lightdm/lightdm.conf ${ETC}/lightdm
cp /etc/lightdm/pantheon-greeter.conf ${ETC}/lightdm
###   sysctl.d
cp /etc/sysctl.d/30-max-user-instances.conf ${ETC}/sysctl.d
cp /etc/sysctl.d/40-max-user-watches.conf ${ETC}/sysctl.d
###   systemd
mkdir ${ETC}/systemd/system
cp /etc/systemd/logind.conf ${ETC}/systemd
####    system
cp /etc/systemd/system/i3color.service ${ETC}/systemd/system

##  usr
mkdir ${USR}/local
###   local
mkdir ${USR}/local/bin
####    bin
cp /usr/local/bin/i3color ${USR}/local/bin

# package lists
pacman -Qqttn > external/pacman.txt
pacman -Qqttm > external/yaourt.txt
code-insiders --list-extensions > external/code.txt
code-insiders --list-extensions > external/code-minimal.txt
