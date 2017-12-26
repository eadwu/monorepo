#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 15))

n=1

(
  # title
  echo ""
  # sleep 10
) | dzen2 -p \
  -x ${XPOS} -w 450 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
