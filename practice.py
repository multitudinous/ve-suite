# python

# sets up environment to build and/or run VE-Xplorer

#OMNI_HOME : comment out definitions to make a non-corba version - that's all you have to do

#this is typically defined in your .cshrc file
#setenv VE_SUITE_HOME /home/vr/Applications/TSVEG/VE_Suite


import glob
import os
import re
import shutil
import sys
import time
import traceback


if "IRIX64" in os.uname():
   uname_LIB = "IRIX64"
   os.environ['CFD_LIBS'] = uname_LIB
   print "CFD_LIBS = " , os.getenv('CFD_LIBS','')
elif "Linux" in os.uname():
   uname_LIB = "Linux"
   os.environ['CFD_LIBS'] = uname_LIB
   print "CFD_LIBS = " , os.getenv('CFD_LIBS','')
else:
   print "ERROR"
   
#print "uname is" , os.uname()
#print "uname -n is" , os.uname() -n
#os.environ['CFD_LIBS'] = 'uname()_LIB'
#print "CFD_LIBS = " , os.getenv('CFD_LIBS','')
#os.environ['VE_LIBS'] = 'os.uname()'
#print "VE_LIBS = " , os.getenv('VE_LIBS','')
#print "CFD_LIBS = " , os.getenv(CFD_LIBS)
os.environ['CFDHOSTTYPE'] = os.getenv('HOSTTYPE','')
print "CFDHOSTTYPE = " , os.getenv('CFDHOSTTYPE','')

#os.environ['TAO_BUILD'] = 'TRUE'
#print "TAO_BUILD = " , os.getenv('TAO_BUILD','')
os.environ['TAO_BUILD'] = 'FALSE'
print "TAO_BUILD = " , os.getenv('TAO_BUILD','')

os.environ['PFNFYLEVEL'] = '2'
print "PFNFYLEVEL = " , os.getenv('PFNFYLEVEL','')
os.environ['VPR_DEBUG_NFY_LEVEL'] = '0'
print "VPR_DEBUG_NFY_LEVEL = " , os.getenv('VPR_DEBUG_NFY_LEVEL', '')
os.environ['VPR_DEBUG_ENABLE'] = '1'
print "VPR_DEBUG_ENABLE = " , os.getenv('VPR_DEBUG_ENABLE', '')
os.environ['PFSHAREDSIZE'] = '534773700'
print "PFSHAREDSIZE = " , os.getenv('PFSHAREDSIZE', '')
os.environ['WX_HOME_DIR'] = '/home/users/mccdo/wxWindows/wxGTK'
print "WX_HOME_DIR = " , os.getenv('WX_HOME_DIR', '')
os.environ['OMNIORB_CONFIG'] = os.path.join(os.getenv('VE_SUITE_HOME','') , 'VE_Installer/omniORB4.cfg')
print "OMNIORB_CONFIG = " , os.getenv('OMNIORB_CONFIG','')
os.environ['OMNINAMES_LOGDIR'] = os.path.join(os.getenv('VE_SUITE_HOME','') , 'VE_Installer')
print "OMNINAMES_LOGDIR = " , os.getenv('OMNINAMES_LOGDIR','')
VTK_HOME_DIR = '/home/users/mccdo/vtk-builds'

if "IRIX64" in os.uname():
   #print "specifying libraries for irix"
   os.environ['JDK_HOME'] = '/usr/java2'
   print "JDK_HOME = " , os.getenv('JDK_HOME','')
   os.environ['VTK_BASE_DIR'] = VTK_HOME_DIR + '/IRIX32'
   print "VTK_BASE_DIR = " , os.getenv('VTK_BASE_DIR','')
   os.environ['VJ_BASE_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread'
   print "VJ_BASE_DIR = " , os.getenv('VJ_BASE_DIR','')
   os.environ['VJ_DEPS_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps'
   print "VJ_DEPS_DIR = " , os.getenv('VJ_DEPS_DIR','')
   #os.envrion['LD_LIBRARYN32_PATH'] = '/home/users/mccdo/VE_Suite/VE_Installer/arenasize/libs'
   #print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
   os.environ['LD_LIBRARYN32_PATH'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'lib32') \
                                        + os.path.join(os.getenv('VTK_BASE_DIR','') , 'lib/vtk') \
                                        + os.path.join(os.getenv('VJ_DEPS_DIR','') , 'lib32')
   print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','') 
   os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') + '/home/users/jhynek/Pigs/Oinks/OpenAL/openal/linux/bin/lib'
   print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
   os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') + ':/home/users/mccdo/software/IRIX32/lib'
   print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
   os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'irix-65')
   print "WX_HOME = " , os.getenv('WX_HOME','')

   if os.getenv('TAO_BUILD') == "TRUE":
      os.environ['ACE_ROOT'] = '/home/users/mccdo/ACE_TAO/Irix-vrac/ACE_wrappers'
      print "ACE_ROOT = " , os.getenv('ACE_ROOT','')
      os.environ['TAO_ROOT'] = os.path.join(os.getenv('ACE_ROOT','') , 'TAO')
      print "TAO_ROOT = " , os.getenv('TAO_ROOT','')
      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
                                          + os.path.join(os.getenv('ACE_ROOT','') , 'ace') \
                                          + os.path.join(os.getenv('ACE_ROOT','') , 'lib')
      print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
                                          + os.path.join(os.getenv('TAO_ROOT','') , 'TAO_IDL') \
                                          + os.path.join(os.getenv('WX_HOME','') , 'lib')
      print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','') 

   else:
      #os.environ['OMNI_HOME'] = '/home/vr/Juggler/irix/mipspro-omniORB-4.0.1'
      #print "OMNI_HOME = " , os.getenv('OMNI_HOME'.'')
      #os.envrion['OMNI_HOME'] = '/home/users/mccdo/software/IRIX32'
      #print "OMNI_HOME = " , os.getenv('OMNI_HOME','')
      os.environ['OMNI_HOME'] = os.getenv('VJ_DEPS_DIR','')
      print "OMNI_HOME = " , os.getenv('OMNI_HOME','')
#/jhome/vr/Juggler/irix/mipspro
      os.environ['PYTHONPATH'] = os.getenv('OMNI_HOME,','') + '/lib32/python2.3/site-packages'
      print "PYTHONPATH = " , os.getenv('PYTHONPATH','')
      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
                                          + os.path.join(os.getenv('OMNI_HOME','') , 'lib') \
                                          + os.path.join(os.getenv('WX_HOME','') , 'lib')
      print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
      
elif "Linux" in os.uname():
   #print "specifying libraries for linux" 
   os.environ['VTK_BASE_DIR'] = VTK_HOME_DIR + '/Linux-rh8'
   print "VTK_BASE_DIR = " , os.getenv('VTK_BASE_DIR','')

   if os.getenv('HOSTTYPE','') == "i386-linux":
      os.environ['JDK_HOME'] = '/usr/java/j2sdk1.4.2_03'
      print "JDK_HOME = " , os.getenv('JDK_HOME','')
      os.environ['VJ_BASE_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80'
      print "VJ_BASE_DIR = " , os.getenv('VJ_BASE_DIR','')
      os.environ['VJ_DEPS_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80-deps'
      print "VJ_DEPS_DIR = " , os.getenv('VJ_DEPS_DIR','')
   else:
      os.environ['JDK_HOME'] = '/usr/lib/java2'
      print "JDK_HOME = " , os.getenv('JDK_HOME','')
      os.environ['VJ_BASE_DIR'] = '/home/users/mccdo/vrjuggler-builds/Suse-9.1-alpha4'
      print "VJ_BASE_DIR = " , os.getenv('VJ_BASE_DIR','')
      os.environ['VJ_DEPS_DIR'] = '/home/users/mccdo/cppdom-0.3.2/Suse-9.1'
      print "VJ_DEPS_DIR = " , os.getenv('VJ_DEPS_DIR','')

   os.environ['BOOST_INCLUDES'] = os.path.join(os.getenv('VJ_DEPS_DIR','') , 'include/boost-1_31')
   print "BOOST_INCLUDES = " , os.getenv('BOOST_INCLUDES','')
   os.environ['LD_LIBRARY_PATH'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'lib') \
                                    + os.path.join(os.getenv('VTK_BASE_DIR','') , 'lib/vtk') \
                                    + os.path.join(os.getenv('VJ_DEPS_DIR','') , '/lib')
   print "LD_LIBRARY_PATH = " , os.getenv('LD_LIBRARY_PATH','')
   os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'linux-rh80')
   print "WX_HOME = " , os.getenv('WX_HOME','')

   if os.getenv('TAO_BUILD') == "TRUE":
      os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'linux-rh80')
      print "WX_HOME = " , os.getenv('WX_HOME','')
      os.environ['ACE_ROOT'] = '/home/users/mccdo/ACE_TAO/Suse-9-vrac/ACE_wrappers'
      print "ACE_ROOT = " , os.getenv('ACE_ROOT','')
      #os.environ['ACE_ROOT'] = '/home/users/mccdo/ACE_TAO/Linux-rh80-vrac/ACE_wrappers'
      #print "ACE_ROOT = " , os.getenv('ACE_ROOT','')

      os.environ['TAO_ROOT'] = os.path.join(os.getenv('ACE_ROOT','') , 'TAO')
      print "TAO_ROOT = " , os.getenv('TAO_ROOT','')
      os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                       + os.path.join(os.getenv('ACE_ROOT','') , 'ace') \
                                       + os.path.join(os.getenv('ACE_ROOT','') , 'lib')
      print "LD_LIBRARY_PATH = " , os.getenv('LD_LIBRARY_PATH','')
      os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                       + os.path.join(os.getenv('TAO_ROOT','') , 'TAO_IDL') \
                                       + os.path.join(os.getenv('WX_HOME','') , 'lib')
      print "LD_LIBRARY_PATH = " , os.getenv('LD_LIBRARY_PATH','')
   else:
      os.environ['OMNI_HOME'] = '/home/vr/Juggler/linux-rh80'
      print "OMNI_HOME = " , os.getenv('OMNI_HOME','')
      #os.environ['OMNI_HOME'] = '/home/vr/Juggler/linux-fc1'
      #print "OMNI_HOME = " , os.getenv('OMNI_HOME','')
      os.environ['PYTHONPATH'] = os.path.join(os.getenv('OMNI_HOME','') , 'lib/python2.2/site-packages')
      print "PYTHONPATH = " , os.getenv('PYTHONPATH','')
      os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                       + os.path.join(os.getenv('OMNI_HOME','') , 'lib') \
                                       + os.path.join(os.getenv('WX_HOME','') , 'lib')
      print "LD_LIBRARY_PATH = " , os.getenv('LD_LIBRARY_PATH','')

else:
   print "ERROR: Unsupported operating system"
   print "       CFD_LIBS and OMNI_HOME are undefined"

os.environ['TWEEK_BASE_DIR'] = os.getenv('VJ_BASE_DIR','')
print "TWEEK_BASE_DIR = " , os.getenv('TWEEK_BASE_DIR','')
os.environ['DZR_BASE_DIR'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'share/Doozer')
print "DZR_BASE_DIR = " , os.getenv('DZR_BASE_DIR','')
os.environ['SNX_BASE_DIR'] = os.getenv('VJ_BASE_DIR','')
print "SNX_BASE_DIR = " , os.getenv('SNX_BASE_DIR','')
os.environ['PATH'] = os.getenv('PATH','') \
                     + os.path.join(os.getenv('VJ_BASE_DIR','') , 'bin') \
                     + os.path.join(os.getenv('VE_SUITE_HOME','') , 'bin') \
                     + os.path.join(os.getenv('VE_SUITE_HOME','') , 'bin/' + os.getenv('VE_LIBS','')) \
                     + os.path.join(os.getenv('VJ_DEPS_DIR','') , 'bin')
print "PATH = " , os.getenv('PATH','')
os.environ['PATH'] = os.path.join(os.getenv('WX_HOME','') , '/bin') \
                     + os.getenv('PATH','')
print "PATH = " , os.getenv('PATH','')
os.environ['PATH'] = os.path.join(os.getenv('JDK_HOME','') , '/bin') \
                     + os.getenv('PATH','')
print "PATH = " , os.getenv('PATH','')
if os.getenv('TAO_BUILD','') == "TRUE":
   os.environ['PATH'] = os.path.join(os.getenv('ACE_ROOT','') , '/bin') \
                        + os.getenv('PATH','')
   print "PATH = " , os.getenv('PATH','')
else:
   os.environ['PATH'] = os.path.join(os.getenv('OMNI_HOME','') , '/bin') + os.getenv('PATH','')
   print "PATH = " , os.getenv('PATH','')

#print ""
#print "Now you may type 'gmake' to build the application"
#print "              or 'gmake clean'
#print "              or 'gmake cleandepend'
#print "              or 'run' to start the application in sim mode"
#print "              or 'runc6' to start the application on the c6"
#print "              or 'runc4.closed' or 'runc4.open' to start the application on the c4"
#print ""

#import menu_options
