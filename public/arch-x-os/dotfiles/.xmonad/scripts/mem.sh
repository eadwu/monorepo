#!/usr/bin/env sh
XOFFSET=0
XPOS=$((XOFFSET + 64))

# memstat=$(free -m)
# usedmem=$(awk -F " " '{print $3}' <<< ${memstat})
# freemem=$(awk -F " " '{print $4}' <<< ${memstat})
# cache=$(awk -F " " '{print $6}' <<< ${memstat})
n=7

# (
#   # title
#   echo ""
#   echo " ^i(${HOME}/.xmonad/dzen2/mem.xbm) RAM"
#   echo "   used: $(sed -n "2p" <<< ${usedmem})M"
#   echo "   free: $(sed -n "2p" <<< ${freemem})M"
#   echo "   cache: $(sed -n "2p" <<< ${cache})M"
#   echo " ^i(${HOME}/.xmonad/dzen2/mem.xbm) SWAP"
#   echo "   used: $(sed -n "3p" <<< ${usedmem})M"
#   echo "   free: $(sed -n "3p" <<< ${freemem})M"
#   # sleep 10s
# )
conky -qc ~/.xmonad/scripts/.mem.conkyrc | dzen2 -p \
  -x ${XPOS} -w 250 \
  -y 0 \
  -sa 'l' -ta 'c' \
  -l ${n} -e 'onstart=hide,uncollapse;enterslave=grabkeys;leaveslave=ungrabkeys,exitkey_Escape=ungrabkeys,exit' \
  -bg '#252526' -fg '#ca5' -fn 'Operator Mono:pixelsize=13'
