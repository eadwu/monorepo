#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 226))

n=3

# (
#   # title
#   echo ""
#   # sleep 10
# )
conky -qc ~/.xmonad/scripts/.net.conkyrc | dzen2 -p \
  -x ${XPOS} -w 200 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
