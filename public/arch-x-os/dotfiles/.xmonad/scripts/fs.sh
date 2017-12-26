#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 120))

drives=$(df -h / /tmp)
total=$(awk -F " " '{print $2}' <<< ${drives})
used=$(awk -F " " '{print $3}' <<< ${drives})
free=$(awk -F " " '{print $4}' <<< ${drives})
n=8

(
  # title
  echo ""
  echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/diskette.xbm)^fg() /"
  echo "   used: $(sed -n "2p" <<< ${used})"
  echo "   free: $(sed -n "2p" <<< ${free})"
  echo "   total: $(sed -n "2p" <<< ${total})"
  echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/diskette.xbm)^fg() /tmp"
  echo "   used: $(sed -n "3p" <<< ${used})"
  echo "   free: $(sed -n "3p" <<< ${free})"
  echo "   total: $(sed -n "3p" <<< ${total})"
  # sleep 10
) | dzen2 -p \
  -x ${XPOS} -w 250 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
