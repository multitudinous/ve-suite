#!/bin/sh
#--ctrl-display localhost:0.0
ssh rockarwkviz1.ria.army.mil 'export DISPLAY=:0.0 && nvidia-settings -l --config=/ves/VE_Software/nvidia-settings-rc-rendernode'
ssh rockarwkviz2.ria.army.mil 'export DISPLAY=:0.0 && nvidia-settings -l --config=/ves/VE_Software/nvidia-settings-rc-rendernode'
ssh rockarwkviz3.ria.army.mil 'export DISPLAY=:0.0 && nvidia-settings -l --config=/ves/VE_Software/nvidia-settings-rc-rendernode'
ssh rockarwkviz4.ria.army.mil 'export DISPLAY=:0.0 && nvidia-settings -l --config=/ves/VE_Software/nvidia-settings-rc-rendernode'
