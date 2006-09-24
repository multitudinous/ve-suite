# python

# sets up environment to build and/or run VE-Xplorer

#OMNI_HOME : comment out definitions to make a non-corba version - that's all you have to do

#this is typically defined in your .cshrc file
#os.environ['VE_SUITE_HOME'] = '/home/users/jaredabo/VE_Suite-SCons/VE_Suite'
#print "VE_SUITE_HOME = " , os.getenv('VE_SUITE_HOME','')

import glob
import os
import re
import shutil
import sys
import time
import traceback

def PrintVar(var):
    print "%s = %s" % (var, os.getenv(var, ''))
    return

##if "IRIX64" in os.uname():
##   uname_LIB = "IRIX64"
##   os.environ['CFD_LIBS'] = uname_LIB
##   PrintVar('CFD_LIBS')
##elif "Linux" in os.uname():
if "Linux" in os.uname():
   uname_LIB = "Linux"
   os.environ['CFD_LIBS'] = uname_LIB
   PrintVar('CFD_LIBS')
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
PrintVar('CFDHOSTTYPE')

if os.getenv("TAO_BUILD") == None:
    os.environ['TAO_BUILD'] = 'TRUE'
PrintVar('TAO_BUILD')

os.environ['PFNFYLEVEL'] = '2'
PrintVar('PFNFYLEVEL')
os.environ['VPR_DEBUG_NFY_LEVEL'] = '0'
PrintVar('VPR_DEBUG_NFY_LEVEL')
os.environ['VPR_DEBUG_ENABLE'] = '1'
PrintVar('VPR_DEBUG_ENABLE')
os.environ['PFSHAREDSIZE'] = '534773700'
PrintVar('PFSHAREDSIZE')
#os.environ['WX_HOME_DIR'] = '/home/users/mccdo/wxWindows/wxGTK-2.6.2/'
os.environ['WX_HOME_DIR'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/wxGTK-2.6.2/'
PrintVar('WX_HOME_DIR')
##Conversion ends here.
os.environ['OMNIORB_CONFIG'] = os.path.join(os.getenv('VE_SUITE_HOME','') , 'VE_Installer/omniORB4.cfg')
PrintVar('OMNIORB_CONFIG')
os.environ['OMNINAMES_LOGDIR'] = os.path.join(os.getenv('VE_SUITE_HOME','') , 'VE_Installer')
PrintVar('OMNINAMES_LOGDIR')
VTK_HOME_DIR = '/home/vr/Applications/TSVEG/Libraries/Release/Debug/VTK/'

##if "IRIX64" in os.uname():
##   #print "specifying libraries for irix"
##   os.environ['JDK_HOME'] = '/usr/java2'
##   PrintVar('JDK_HOME')
##   os.environ['VTK_BASE_DIR'] = VTK_HOME_DIR + '/IRIX32'
##   PrintVar('VTK_BASE_DIR')
##   os.environ['VJ_BASE_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread'
##   PrintVar('VJ_BASE_DIR')
##   os.environ['VJ_DEPS_DIR'] = '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps'
##   PrintVar('VJ_DEPS_DIR')
##   #os.envrion['LD_LIBRARYN32_PATH'] = '/home/users/mccdo/VE_Suite/VE_Installer/arenasize/libs'
##   #print "LD_LIBRARYN32_PATH = " , os.getenv('LD_LIBRARYN32_PATH','')
##   os.environ['LD_LIBRARYN32_PATH'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'lib32') \
##                                        + os.path.join(os.getenv('VTK_BASE_DIR','') , 'lib/vtk') \
##                                        + os.path.join(os.getenv('VJ_DEPS_DIR','') , 'lib32')
##   PrintVar('LD_LIBRARYN32_PATH') 
##   os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') + '%s/home/users/jhynek/Pigs/Oinks/OpenAL/openal/linux/bin/lib' % (os.pathsep)
##   PrintVar('LD_LIBRARYN32_PATH')
##   os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') + '%s/home/users/mccdo/software/IRIX32/lib' % (os.pathsep)
##   PrintVar('LD_LIBRARYN32_PATH')
##   os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'irix-65')
##   PrintVar('WX_HOME')

##   if os.getenv('TAO_BUILD') == "TRUE":
##      os.environ['ACE_ROOT'] = '/home/users/mccdo/ACE_TAO/Irix-vrac/ACE_wrappers'
##      PrintVar('ACE_ROOT')
##      os.environ['TAO_ROOT'] = os.path.join(os.getenv('ACE_ROOT','') , 'TAO')
##      PrintVar('TAO_ROOT')
##      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
##                                          + os.pathsep + os.path.join(os.getenv('ACE_ROOT','') , 'ace') \
##                                          + os.pathsep + os.path.join(os.getenv('ACE_ROOT','') , 'lib')
##      PrintVar('LD_LIBRARYN32_PATH')
##      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
##                                          + os.pathsep + os.path.join(os.getenv('TAO_ROOT','') , 'TAO_IDL') \
##                                          + os.pathsep + os.path.join(os.getenv('WX_HOME','') , 'lib')
##      PrintVar('LD_LIBRARYN32_PATH') 

##   else:
##      #os.environ['OMNI_HOME'] = '/home/vr/Juggler/irix/mipspro-omniORB-4.0.1'
##      #print "OMNI_HOME = " , os.getenv('OMNI_HOME'.'')
##      #os.envrion['OMNI_HOME'] = '/home/users/mccdo/software/IRIX32'
##      #print "OMNI_HOME = " , os.getenv('OMNI_HOME','')
##      os.environ['OMNI_HOME'] = os.getenv('VJ_DEPS_DIR','')
##      PrintVar('OMNI_HOME')
#/jhome/vr/Juggler/irix/mipspro
##      os.environ['PYTHONPATH'] = os.getenv('OMNI_HOME,','') + '/lib32/python2.3/site-packages'
##      PrintVar('PYTHONPATH')
##      os.environ['LD_LIBRARYN32_PATH'] = os.getenv('LD_LIBRARYN32_PATH','') \
##                                          + os.pathsep + os.path.join(os.getenv('OMNI_HOME','') , 'lib') \
##                                          + os.pathsep + os.path.join(os.getenv('WX_HOME','') , 'lib')
##      PrintVar('LD_LIBRARYN32_PATH')
      
if "Linux" in os.uname():
   #print "specifying libraries for linux" 

   os.environ['JDK_HOME'] = '/usr'
   PrintVar('JDK_HOME')
   os.environ['VTK_BASE_DIR'] = VTK_HOME_DIR + 'Linux-RHEL_4'
   PrintVar('VTK_BASE_DIR')
#   os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'install-rhel_4')
   os.environ['WX_HOME'] = os.path.join(os.getenv('WX_HOME_DIR','') , 'Linux-RHEL_4')
   PrintVar('WX_HOME')
   os.environ['VJ_BASE_DIR'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1/vrjuggler-2.0.1-linux-rhel4-i686'
   PrintVar('VJ_BASE_DIR')
   os.environ['VJ_DEPS_DIR'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1-deps/vrjuggler-2.0.1-linux-rhel4-i686-deps'
   PrintVar('VJ_DEPS_DIR')
   ##os.environ['OSG_HOME'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG/Linux-RHEL_4'
   os.environ['OSG_HOME'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG-1.0/Linux-RHEL_4'
   PrintVar('OSG_HOME')
   
   os.environ['BOOST_INCLUDES'] = os.path.join(os.getenv('VJ_DEPS_DIR','') , 'include/boost')
   PrintVar('BOOST_INCLUDES')
   os.environ['LD_LIBRARY_PATH'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'lib') \
                                    + os.pathsep + os.path.join(os.getenv('VTK_BASE_DIR','') , 'lib/vtk') \
                                    + os.pathsep + os.path.join(os.getenv('VJ_DEPS_DIR','') , 'lib')
   PrintVar('LD_LIBRARY_PATH')
   os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                    + os.pathsep + os.path.join(os.getenv('WX_HOME','') , 'lib') 
   PrintVar('LD_LIBRARY_PATH')
   os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                    + os.pathsep + os.path.join(os.getenv('OSG_HOME','') , 'lib') \
                                    + os.pathsep + os.path.join(os.getenv('OSG_HOME','') , 'lib/osgPlugins') \
                                    + os.pathsep + os.path.join(os.getenv('OSG_HOME','') , '/share/OpenSceneGraph/bin')
   PrintVar('LD_LIBRARY_PATH')
   os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                    + os.pathsep + os.path.join(os.getenv('VE_SUITE_HOME',''), 'lib', os.getenv('CFDHOSTTYPE',''))
   PrintVar('LD_LIBRARY_PATH')

   if os.getenv('TAO_BUILD') == "TRUE":

      os.environ['XERCESCROOT'] = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/xercexc-c-2.7.0/Linux-RHEL_4'
      PrintVar('XERCESCROOT')  
      ##os.environ['TAO_HOME'] = "/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE-TAO-5.4.10/Linux-RHEL_4"
      ##os.environ['TAO_HOME'] = "/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE_TAO/Linux-RHEL_4"
      os.environ['TAO_HOME'] = "/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE-TAO-5.5/Linux-RHEL_4"
      PrintVar('TAO_HOME')
      os.environ['PATH'] = os.path.join(os.getenv('TAO_HOME',''), 'bin') \
                                       + os.pathsep + os.path.join(os.getenv('PATH',''))
      PrintVar('PATH')
      os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                       + os.pathsep + os.path.join(os.getenv('TAO_HOME','') , 'lib')
      PrintVar('LD_LIBRARY_PATH')

      os.environ['LD_LIBRARY_PATH'] = os.getenv('LD_LIBRARY_PATH','') \
                                       + os.pathsep + os.path.join(os.getenv('XERCESCROOT',''), 'lib')
      PrintVar('LD_LIBRARY_PATH')    
      
   else:
      os.environ['PYTHONPATH'] = os.path.join(os.getenv('VJ_DEPS_DIR','') , 'lib/python2.2/site-packages')
      PrintVar('PYTHONPATH')

else:
   print "ERROR: Unsupported operating system"
   print "       CFD_LIBS and OMNI_HOME are undefined"

##Append(CPPPATH = pj(os.getenv(VJ_BASE_DIR, 'bin'))) ##TESTER

os.environ['TWEEK_BASE_DIR'] = os.getenv('VJ_BASE_DIR','')
PrintVar('TWEEK_BASE_DIR')
os.environ['DZR_BASE_DIR'] = os.path.join(os.getenv('VJ_BASE_DIR','') , 'share/Doozer')
PrintVar('DZR_BASE_DIR')
os.environ['SNX_BASE_DIR'] = os.getenv('VJ_BASE_DIR','')
PrintVar('SNX_BASE_DIR')

os.environ['PATH'] = os.getenv('PATH','') + os.pathsep + os.path.join(os.getenv('VJ_BASE_DIR',''), 'bin')
os.environ['PATH'] = os.getenv('PATH','') + os.pathsep + os.path.join(os.getenv('TAO_HOME'), 'bin')
PrintVar('PATH')

#print ""
#print "Now you may type 'gmake' to build the application"
#print "              or 'gmake clean'
#print "              or 'gmake cleandepend'
#print "              or 'run' to start the application in sim mode"
#print "              or 'runc6' to start the application on the c6"
#print "              or 'runc4.closed' or 'runc4.open' to start the application on the c4"
#print ""

#import menu_options
