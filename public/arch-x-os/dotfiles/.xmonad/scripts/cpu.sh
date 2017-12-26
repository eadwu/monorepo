#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 8))

n=12

# (
#   # title
#   echo ""
#   echo " ^i(${HOME}/.xmonad/dzen2/cpu.xbm)"
#   echo "   $(sensors | grep Core | cut -d'+' -f2 | head -c8)"
#   echo "     00%"
#   echo "       $(cat /sys/devices/system/cpu/cpu0/cpufreq/scaling_cur_freq) Hz"
#   echo "     00%"
#   echo "       $(cat /sys/devices/system/cpu/cpu1/cpufreq/scaling_cur_freq) Hz"
#   echo " ^i(${HOME}/.xmonad/dzen2/cpu.xbm)"
#   echo "   $(sensors | grep Core | sed -n "2p" | cut -d'+' -f2 | head -c8)"
#   echo "     00%"
#   echo "       $(cat /sys/devices/system/cpu/cpu2/cpufreq/scaling_cur_freq) Hz"
#   echo "     00%"
#   echo "       $(cat /sys/devices/system/cpu/cpu3/cpufreq/scaling_cur_freq) Hz"
#   # sleep 10
# )
conky -qc ~/.xmonad/scripts/.cpu.conkyrc | dzen2 -p \
  -x ${XPOS} -w 250 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
