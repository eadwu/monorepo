#!/usr/bin/env sh
XOFFSET=882
XPOS=$((XOFFSET + 250))

arch=$(uname -m)
kernel=$(uname -r)
packages=$(pacman -Q | wc -l)
uptime=$(uptime | awk '{gsub(/,/,""); print $3}' | sed 's/\([0-9]\+\):\([0-9]\+\)/\1h \2m/')
shell=$(which ${SHELL})
n=3

(
  # title
  echo ""
  # system information
  echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/arch_10x10.xbm)^fg() System ${shell}"
  echo " ^fg(#fff176)^i(${HOME}/.xmonad/dzen2/pacman.xbm)^fg() Kernel: ${kernel} ${arch}"
  echo " ^fg(#81c784)^i(${HOME}/.xmonad/dzen2/net_up_01.xbm)^fg() Packages: ${packages}  Uptime: ${uptime}"
  # sleep 10
) | dzen2 -p \
  -x ${XPOS} -w 308 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exit;key_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
