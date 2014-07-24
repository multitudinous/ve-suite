#!/bin/sh

function usage {
    echo "Usage: $0 [ --on | --off ]"
}

function setupFrameLockGroup() {
    # I got these values by first manually setting up the framelock
    # group in the nvidia-settings GUI tool, and then in a shell
    # running the commands:
    #
    #   nvidia-settings -q :0.0[gpu:N]/FrameLockMaster
    #   nvidia-settings -q :0.0[gpu:N]/FrameLockSlaves
    #
    # where N = 1, 2, ... , n

    # DFP-0 on GPU 1 is the master device
    nvidia-settings -a :0.0[gpu:1]/FrameLockMaster=0x00010000
    # DFP-3 on GPU 1 is a slave
    nvidia-settings -a :0.0[gpu:1]/FrameLockSlaves=0x00080000
    # both DFP-0 and DFP-3 on GPU 2 are slaves
    nvidia-settings -a :0.0[gpu:2]/FrameLockSlaves=0x00090000
}

function disableFrameLock() {
    nvidia-settings -a :0.0[gpu:0]/FrameLockEnable=0
    nvidia-settings -a :0.0[gpu:1]/FrameLockEnable=0
    nvidia-settings -a :0.0[gpu:2]/FrameLockEnable=0
}

function enableFrameLock() {
    setupFrameLockGroup
    nvidia-settings -a :0.0[gpu:1]/FrameLockEnable=1
    nvidia-settings -a :0.0[gpu:2]/FrameLockEnable=1
    nvidia-settings -a :0.0[framelock:0]/FrameLockUseHouseSync=0
    nvidia-settings -a :0.0[gpu:1]/FrameLockTestSignal=1
    nvidia-settings -a :0.0[gpu:1]/FrameLockTestSignal=0
}

case $1 in
    --on|on)
        disableFrameLock
        enableFrameLock
        exit 0
        ;;
    --off|off)
        disableFrameLock
        exit 0
        ;;
    *)
        usage
        exit 1
        ;;
esac
