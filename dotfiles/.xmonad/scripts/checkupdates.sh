#!/usr/bin/env sh
XOFFSET=882
XPOS=${XOFFSET}

orphans=$(pacman -Qtdq)
updates=$(checkupdates)
on=$(wc -l <<< ${orphans})
un=$(wc -l <<< ${updates})
tn=$(( on + un ))
n=$([ ${tn} -gt 45 ] && echo 45 || echo ${tn})

(
  # title
  echo ""
  # `orphans` output
  if [ -n "${orphans}" ]; then
    echo " ^fg(#ff8a65)^fn(Font Awesome 5 Free Solid:pixelsize=13)^fn()^fg() ${on}"
    sed 's/^/    /g' <<< ${orphans}
  fi
  # `checkupdates` output
  if [ -n "${updates}" ]; then
    echo " ^fg(#81c784)^fn(Font Awesome 5 Free Solid:pixelsize=13)^fn()^fg() ${un}"
    sed 's/^/    /g' <<< ${updates}
  fi
  # clean?
  if [ -z "${orphans}" ] && [ -z "${updates}" ]; then
    echo "    N/A"
  fi
  # sleep 10
) | dzen2 -p \
  -x ${XPOS} -w 558 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=scrollhome,grabkeys;leaveslave=ungrabkeys,exit;key_Up=scrollup;key_Right=scrollend;key_Down=scrolldown;key_Left=scrollhome;key_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
