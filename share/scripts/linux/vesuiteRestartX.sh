#!/bin/sh
echo "Rebooting rockarwkviz1"
ssh rockarwkviz1 'sudo /sbin/init 3'
echo "Rebooting rockarwkviz2"
ssh rockarwkviz2 'sudo /sbin/init 3'
echo "Rebooting rockarwkviz3"
ssh rockarwkviz3 'sudo /sbin/init 3'
echo "Rebooting rockarwkviz4"
ssh rockarwkviz4 'sudo /sbin/init 3'
echo "Rebooting rockarwkviz1"
ssh rockarwkviz1 'sudo /sbin/init 5'
echo "Rebooting rockarwkviz2"
ssh rockarwkviz2 'sudo /sbin/init 5'
echo "Rebooting rockarwkviz3"
ssh rockarwkviz3 'sudo /sbin/init 5'
echo "Rebooting rockarwkviz4"
ssh rockarwkviz4 'sudo /sbin/init 5'
sleep 3
