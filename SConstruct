#!python
EnsureSConsVersion(0,96)
SConsignFile()

import os, sys, string, smtplib

from subprocess import *

import SConsAddons.Util as sca_util
import SConsAddons.Options as asc_opt
##import SConsAddons.Options.CppUnit
##import SConsAddons.Options.Boost
import SConsAddons.AutoDist as sca_auto_dist
from SConsAddons.EnvironmentBuilder import EnvironmentBuilder

GetPlatform = sca_util.GetPlatform
Export('GetPlatform')
GetArch = sca_util.GetArch
Export('GetArch')

WX_HOME_DIR = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/wxGTK-2.6.2/'

pj = os.path.join
opts = Options()
opts.Add(BoolOption('VE_PATENTED', 'Set for Patent', 1))
opts.Add(BoolOption('TAO_BUILD', 'Set for Tao', 1))
opts.Add(BoolOption('CLUSTER_APP', 'Set for Cluster', 0))
##NOTE: Put in path to flagpoll & flagpoll files.

def GetCFDHostType():
    """Determines which hosttype this build is being run on."""
    if windows:
        return "WIN32"
    elif unix:
        if (os.path.exists("/etc/redhat-release")):
            piped = os.popen("""cat /etc/redhat-release """ +
                             """| awk -F" " '{print $1}'""", 'r')
            firstWord = piped.read()[:-1]
            ##NOTE: [:-1] is to remove the line break from the read()
            piped.close()
            if firstWord == "Red":
                piped = os.popen("""cat /etc/redhat-release """ +
                                 """| awk -F" " '{print $3}'""", 'r')
                thirdWord = piped.read()[:-1]
                piped.close()
                if thirdWord == "Enterprise":
                    ##Extract words from file to create similar to RHEL_3
                    piped= os.popen("""cat /etc/redhat-release """ +
                                    """| awk -F" " '{print "RHEL_" $7}'""",
                                    'r')
                    cfdHostType = piped.read()[:-1]
                    piped.close()
                else:
                    ##Extract words from file to create
                    ##something like RedHat_8.0
                    piped = os.popen("""cat /etc/redhat-release """ +
                                     """| awk -F" " '""" +
                                     """{print $1 $2 "_" $5}'""",
                                     'r')
                    cfdHostType = piped.read()[:-1]
                    piped.close()
            elif firstWord == "Fedora":
                ##Extract words from file to create something like Fedora_1
                piped= os.popen("""cat /etc/redhat-release """ +
                                """| awk -F" " '{print $1 "_" $4}'""", 'r')
                cfdHostType = piped.read()[:-1]
                piped.close()
            else:
                ##NOTE: If the program couldn't identify this type of
                ##Redhat, just use uname.
                piped = os.popen("uname")
                cfdHostType = piped.read()[:-1]
                piped.close()
        elif os.path.exists("/etc/SuSE-release"):
            ##Extract words from file to create
            ##something like SuSE_9.2_x86-64
            piped = os.popen("""head -1 /etc/SuSE-release """ +
                             """| awk -F" " '{print $1 "_" $3 "_" $4}'""",
                             'r')
            cfdHostType = piped.read()[:-1]
            piped.close()
        else:
            piped = os.popen("uname")
            cfdHostType = piped.read()[:-1]
            piped.close()
        ##If CFDHOSTTYPE has parentheses, remove them.
        piped = os.popen("""echo \"%s\" """ %cfdHostType +
                         """| sed -e 's/(//g' | sed -e 's/)//g' """ + 
                         """| sed -e 's/"//g'""", 'r')
        cfdHostType = piped.read()[:-1]
        piped.close()
    return cfdHostType

def GetTag(execTag = False, osgTag = False,
           patentedTag = False, clusterTag = False):
    """Creates a combined tag for libraries and programs."""
    ##Combine the tags.
    finalTag = ''
    if execTag:
        if os.getenv('TAO_BUILD') == 'TRUE':
            exec_tag = '_tao'
        else:
            exec_tag = ''
        finalTag += exec_tag
    if osgTag:
        ##if os.getenv('SCENE_GRAPH') == 'OSG':
        osg_tag = "_osg"
        finalTag += osg_tag
    if patentedTag:
        if os.getenv('VE_PATENTED') == 'TRUE':
            patented_tag = "_vep"
        else:
            patented_tag = ''
        finalTag += patented_tag
    if os.getenv('CLUSTER_APP') == 'TRUE':
        cluster_tag = '_cluster'
    else:
        cluster_tag = ''
    return finalTag

##execTag = GetTag(True)
##execOsgTag = GetTag(True, True)
##Export('execTag')
##Export('execOsgTag')
cfdHostType = GetPlatform() + '.' + GetArch()
libPath = pj('#', 'lib', cfdHostType)
binDir = pj('#', 'bin', cfdHostType)
execOsgPatTag = GetTag(True, True, True)
execOsgPatClusterTag = GetTag(True, True, True, True)
Export('cfdHostType')
Export('libPath')
Export('execOsgPatTag')
Export('execOsgPatClusterTag')

def BuildLinuxEnvironment():
    """Builds a base environment for other modules to build on set up for Linux"""
    global optimize, profile, builders
    profile = 'no'
    optimize = 'no'
    env = Environment(ENV = os.environ,
                      options = opts,
                      CXX = 'g++',
                      LINK = 'g++')
    CXXFLAGS = ['-g']
    if env['VE_PATENTED'] == 'true':
        CXXFLAGS.extend(['-DVE_PATENTED'])
    CPPDEFINES = ['HAVE_CONFIG_H', '_LINUX', 'VTK_STREAMS_FWD_ONLY', '_TAO',
                  '_REENTRANT', 'ACE_HAS_AIO_CALLS', '_GNU_SOURCE',
                  'ACE_HAS_EXCEPTIONS', '__ACE_INLINE__', '_OSG', 'VTK44',
                  'WXUSINGDLL', '__WXGTK__']
    LINKFLAGS = CXXFLAGS
    LIBS = []
    CPPPATH = ['#']
    # Enable profiling?
    if profile != 'no':
        CXXFLAGS.extend([])   ##Does nothing.
        LINKFLAGS.extend([])  ##Ditto.
    # Debug or optimize build?
    if optimize != 'no': ##Debug mode
        CPPDEFINES.extend(['NDEBUG'])
        CXXFLAGS.extend(['-O2'])
    else: ##Optimize mode
        #CPPDEFINES.append('_DEBUG')
        CXXFLAGS.extend(['-g'])
    env.Append(CXXFLAGS = CXXFLAGS,
               CPPDEFINES = CPPDEFINES,
               LINKFLAGS = LINKFLAGS,
               CPPPATH = CPPPATH,
               LIBPATH = libPath,
               LIBS = LIBS)
    ##Build VE-env from setup.tsh
    env['VE_SUITE_HOME'] = sys.path[0]
    env['CFDHOSTTYPE'] = GetCFDHostType()
##    uname_LIB = "Linux"
##    env['CFD_LIBS'] = uname_LIB
    env['PFNFYLEVEL'] = '0'
    env['PFSHAREDSIZE'] = '534773700'
    env['VPR_DEBUG_NFY_LEVEL'] = '0'
    env['VPR_DEBUG_ENABLE'] = '1'
    env['NO_RTRC_PLUGIN'] = 'TRUE'
    env['NO_PERF_PLUGIN'] = 'TRUE'
    env['OSGTHREAD_SAFE_REF_UNREF'] = '1'
    env['OSGNOTIFYLEVEL'] = 'DEBUG_INFO'
    env['OMNINAMES_LOGDIR'] = '%s/VE_Installer' %env['VE_SUITE_HOME']
    env['OMNIORB_CONFIG'] = '%s/omniORB4.cfd' %env['OMNINAMES_LOGDIR']
    ##CFDHostType specific settings
    changes = {}
    appends = {}
    appends['PATH'] = []
    cfdHostType = env['CFDHOSTTYPE']
    if cfdHostType.split('_')[0] in ["RedHat", "Fedora", "RHEL"]:
        vtkBase = Popen(['flagpoll', 'vtk', '--variable=prefix'], stdout=PIPE).communicate[0]
        osgHome = Popen(['flagpoll', 'osg', '--variable=prefix'], stdout=PIPE).communicate[0]
        xercesRoot = Popen(['flagpoll', 'xerces', '--variable=prefix'], stdout=PIPE).communicate[0]
        changes = {"JDK_HOME": "/usr/java",
                   "VTK_BASE_DIR": vtkBase,
                   "WX_HOME": "/home/users/mccdo/wxWindows/wxGTK-2.6.2/install-rhel_4",
                   "VJ_BASE_DIR": "/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1/vrjuggler-2.0.1-linux-rhel4-i686",
                   "VJ_DEPS_DIR": "/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1-deps/vrjuggler-2.0.1-linux-rhel4-i686-deps",
                   "CORONA_HOME": "/home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92",
                   "OSG_HOME": osgHome,
                   "COIN_HOME": "/home/vr/Applications/TSVEG/Libraries/Release/Opt/Coin-2.4.5/Linux-RHEL_4"}
        if env['TAO_BUILD'] == 'TRUE':
            taoHome = Popen(['flagpoll', 'tao', '--variable=prefix'], stdout=PIPE).communicate[0]
            changes['TAO_HOME'] = taoHome
            changes['XERCESROOT'] = xercesRoot
            appends['LD_LIBRARY_PATH'] = ["%s/lib" %taoHome, "%s/lib" %xercesRoot]
            appends['PATH'] = ["%s/bin" %taoHome]
    elif cfdHostType[0] == 'S' and cfdHostType[2:3] == 'SE':
        if cfdHostType[-2:] == '64': ##S*SE*64
            pass
        else:
            pass
    elif cfdHostType == "SunOS":
        pass
    elif cfdHostType == "Darwin":
        changes = {"JDK_HOME": "/usr/java/jdk1.5.0_04",
                   "VTK_BASE_DIR": "/home/users/mccdo/VTK/vtk-5.0/install-rhel4_release",
                   "WX_HOME": "/home/vr/Applications/TSVEG/Libraries/Release/Opt/wxMac-2.6.2",
                   "VJ_BASE_DIR": "/usr/local",
                   "VJ_DEPS_DIR": "/usr/local/vrjuggler-deps",
                   "OSG_HOME": "/home/users/mccdo/OSG/install-powermac"}
    else:
        print "ERROR!"
    ##Change vars in changes
    for var in changes:
        env[var] = changes[var]
    env['TWEEK_BASE_DIR'] = env['VJ_BASE_DIR']
    env['DZR_BASE_DIR'] = "%s/share/Doozer" %env['VJ_BASE_DIR']
    env['SNX_BASE_DIR'] = env['VJ_BASE_DIR']
    appends['PATH'].append("%s/bin" %env['VJ_BASE_DIR'])
    appends['PATH'].append("%s/bin" %env['VE_SUITE_HOME'])
    appends['PATH'].append("%s/bin/%s" %(env['VE_SUITE_HOME'], env['CFDHOSTTYPE']))
    appends['PATH'].append("%s/bin" %env['VJ_DEPS_DIR'])
    appends['PATH'].append("%s/bin" %env['WX_HOME'])
    pathSep = ':'
    ##Append paths in appends
    for var in appends:
        for entry in appends[var]:
            env[var] = "%s%s%s" %(entry, pathSep, env[var])
    if env['OSG_HOME'] == 'TRUE':
        osgHome = Popen(['flagpoll', 'osg', '--variable=prefix'], stdout=PIPE).communicate[0]
        appends['PATH'].append("%s/share/OpenSceneGraph/bin" %osgHome)
        env['OSG_FILE_PATH'] = "%s/share/OpenSceneGraph-Data" %osgHome
    return env

base_bldr = EnvironmentBuilder()
baseEnv = base_bldr.buildEnvironment()
## load environment of the shell that scons is launched from
baseEnv[ 'ENV' ] = os.environ

baseEnv.Append(CPPPATH = ['#'])
##taoHome = Popen(['flagpoll', 'TAO', '--get-prefix'], stdout=PIPE).communicate[0]
##piped = os.popen("flagpoll TAO --get-prefix")
##taoHome = piped.read()[:-1]
##piped.close()
##baseEnv.Append(PATH = ["%s/bin" %taoHome])
##baseEnv = BuildBaseEnvironment()
Platform = GetPlatform() ##Temporary setup
buildDir = 'build.' + GetPlatform() + '.' + GetArch()
baseEnv.BuildDir(buildDir, '.', duplicate = 0)

##See scons users guide section 15 on variant builds
##include = "#export/$PLATFORM/include"
lib = pj( '#' + buildDir, 'lib' )
bin = pj( '#' + buildDir, 'bin' )
baseEnv.Append( BINDIR = bin, 
            LIBDIR = lib, 
            LIBPATH = [lib],
            CPPDEFINES = ['_TAO', 'VE_PATENTED'] )

## make the temp bin and lib dirs in the build dir
if not os.path.exists( pj( buildDir, 'lib' ) ):
   baseEnv.Execute(Mkdir( pj( buildDir, 'lib' ) ))

if not os.path.exists( pj( buildDir, 'lib', 'flagpoll' ) ):
   baseEnv.Execute(Mkdir( pj( buildDir, 'lib', 'flagpoll' ) ))

if not os.path.exists( pj( buildDir, 'bin' ) ):
   baseEnv.Execute(Mkdir( pj( buildDir, 'bin' ) ))

Export('baseEnv')
Export('Platform')
Export('buildDir')

env = baseEnv.Copy()

##Set the Sconscript files to build.
openSubdirs = Split("""
    VE_Open
    VE_Open/XML
    VE_Open/XML/Shader
    VE_Open/XML/CAD
    VE_Open/XML/Model
    VE_Open/skel
""")

ceSubdirs = Split("""
    VE_CE/Utilities
    VE_CE
""")

conductorSubdirs = Split("""
    Network
    GUIPlugin
    Utilities
    Framework
    DefaultPlugin
""")

xplorerSubdirs = Split("""
    Utilities  
    SceneGraph
    SceneGraph/NURBS
    SceneGraph/NURBS/Utilities
    SceneGraph/Utilities
    TextureBased
    XplorerHandlers
    GraphicalPlugin
    XplorerNetwork
    GE
    DefaultGraphicalPlugin
""")

builderSubdirs = Split("""
    Translator/cfdTranslatorToVTK
    Translator/DataLoader
    Translator/DataLoader/TestLoader
    Preprocessor
    vtkTo3DTexture/tcGUI
    Utilities
""") 

##Tack on path prefixes to subdirs specified above.
builderSubdirs = map(lambda s: pj(buildDir, 'VE_Builder', s), builderSubdirs)
openSubdirs = map(lambda s: pj(buildDir, s), openSubdirs)
conductorSubdirs = map(lambda s: pj(buildDir, 'VE_Conductor', s), conductorSubdirs)
xplorerSubdirs = map(lambda s: pj(buildDir, 'VE_Xplorer', s), xplorerSubdirs)
ceSubdirs = map(lambda s: pj(buildDir, s), ceSubdirs)

##Run SConscript files in all of those folders.
SConscript(dirs = openSubdirs)
SConscript(dirs = ceSubdirs)
SConscript(dirs = conductorSubdirs)
SConscript(dirs = xplorerSubdirs)
SConscript(dirs = builderSubdirs)

Default('.')
