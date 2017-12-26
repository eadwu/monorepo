#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 355))

info=$(acpi -b)
batstatus=$(cut -d',' -f1 <<< $(awk -F " " '{print $3}' <<< ${info}))
battimeleft=$(awk -F " " '{print $5}' <<< ${info})
n=$([ ! ${batstatus} '==' "Full" ] && echo 3 || echo 2)

(
  # title
  echo ""
  echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/ac_01.xbm)^fg() ${batstatus}"
  if [ ! ${batstatus} '==' "Full" ]; then
    echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/bat_full_02.xbm)^fg() ${battimeleft}"
  fi
  echo " ^fg(#64b5f6)^i(${HOME}/.xmonad/dzen2/scorpio.xbm)^fg() $(cat /sys/class/power_supply/BAT0/cycle_count)"
  # sleep 10
) | dzen2 -p \
  -x ${XPOS} -w 200 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
