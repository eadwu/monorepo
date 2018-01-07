#!/usr/bin/env bash

# clean directory
rm -rf dotfiles/.config
rm -rf dotfiles/.ncmpcpp
rm -rf dotfiles/.vim
rm -rf dotfiles/etc
# directories
mkdir dotfiles/.config
mkdir dotfiles/.ncmpcpp
mkdir dotfiles/.vim
mkdir dotfiles/etc
# variables
CONFIG=dotfiles/.config
VIM=dotfiles/.vim
ETC=dotfiles/etc

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
###   mpd
mkdir ${CONFIG}/mpd
cp ${HOME}/.config/mpd/mpd.conf ${CONFIG}/mpd
####    playlists
mkdir ${CONFIG}/mpd/playlists

##  .ncmpcpp
cp ${HOME}/.ncmpcpp/config dotfiles/.ncmpcpp

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
curl https://raw.githubusercontent.com/w0ng/vim-hybrid/master/colors/hybrid.vim > ${VIM}/colors/hybrid.vim
curl https://raw.githubusercontent.com/arcticicestudio/nord-vim/master/colors/nord.vim > ${VIM}/colors/nord.vim

##  etc
mkdir ${ETC}/lightdm
mkdir ${ETC}/pulse
mkdir ${ETC}/sysctl.d
mkdir ${ETC}/systemd
cp /etc/oblogout.conf ${ETC}
###   lightdm
cp /etc/lightdm/lightdm.conf ${ETC}/lightdm
cp /etc/lightdm/pantheon-greeter.conf ${ETC}/lightdm
###   pulse
cp /etc/pulse/client.conf ${ETC}/pulse
cp /etc/pulse/default.pa ${ETC}/pulse
###   sysctl.d
cp /etc/sysctl.d/30-max-user-instances.conf ${ETC}/sysctl.d
cp /etc/sysctl.d/40-max-user-watches.conf ${ETC}/sysctl.d
###   systemd
cp /etc/systemd/logind.conf ${ETC}/systemd
####    system
mkdir ${ETC}/systemd/system
cp /etc/systemd/system/i3color.service ${ETC}/systemd/system

# package lists
pacman -Qen | awk '{print $1}' > external/pacman.txt
pacman -Qem | awk '{print $1}' > external/yaourt.txt
code-insiders --list-extensions > external/code.txt
code-insiders --list-extensions > external/code-minimal.txt
