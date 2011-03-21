#!/bin/bash

for b in   osg-2.8.3.build\
            osgephemeris.build\
            osgworks-1.1.0.build\
            bullet.build\
            osgbullet-1.1.0.build\
            ACE_TAO.build\
            cppdom-1.0.3.build

do
    echo "./build.sh -kpbj 2 $b"
    ./build.sh -kpbj 2 $b
done

