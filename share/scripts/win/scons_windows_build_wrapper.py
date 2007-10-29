#!/usr/bin/python

import  os

#os.system('build_scons.bat')

build_type = raw_input('Input build type: DEBUG|RELEASE:')

os.system(("build_scons.bat %s") % build_type);
