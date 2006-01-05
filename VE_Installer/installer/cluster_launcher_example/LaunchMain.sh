#!/bin/sh
echo Parameter file to be used $1
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchVEServers &
sleep 5
# pass in parameter file name to be used
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchCommand $1 & 
sleep 5
# pass in present working dir and parameter file name to be used on remote machines
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode1 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode2 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode3 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode4 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode5 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode6 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode7 $PWD $1 &
sleep 5
/isv/vr/VE_Suite/Launch-TC/Launcher/LaunchNode8 $PWD $1 &
