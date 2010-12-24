#!python
EnsureSConsVersion(1,3,0)
# See this page for more information about these options
# http://scons.org/wiki/GoFastButton
SetOption('max_drift', 1)
#SetOption('implicit_cache', 1)

###
from SCons.Defaults import SharedCheck, ProgScan
from SCons.Script.SConscript import SConsEnvironment


import os, sys, string, copy, re
import SCons.Environment
import SCons.Platform
import SCons
import SCons.dblite
import fnmatch
###

import os, sys, re,string, smtplib,platform
import distutils.util
import commands
from time import sleep
pj = os.path.join

# Pull in SConsAddons from the source directory if necessary.
sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'src'))
sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'templates'))
import SConsAddons

import SConsAddons.Util as sca_util
import SConsAddons.Options as asc_opt
import SConsAddons.Options.Options 
import SConsAddons.Options.VTK
import SConsAddons.Options.OSG
import SConsAddons.Options.VRJuggler.VRJ
import SConsAddons.Options.Boost
import SConsAddons.Options.Xerces
import SConsAddons.Options.WxWidgets
import SConsAddons.AutoDist as sca_auto_dist
import SConsAddons.Options.FlagPollBasedOption as fp_option
from SConsAddons.EnvironmentBuilder import EnvironmentBuilder

RootDir = os.getcwd()
Export('RootDir')
GetPlatform = sca_util.GetPlatform
Export('GetPlatform')
GetArch = sca_util.GetArch
Export('GetArch')

sys.path.append(pj(os.getcwd(), 'dist', 'build', 'tools'))
import doxygen

# Pull in extra scons helpers
sys.path.append(pj(os.getcwd(), 'share', 'python'))
import HDF5
import HDF4
import InnoSetup
import qt46

# Pull in scolorizer files
#sys.path.append(pj(os.getcwd(), 'share', 'python','scolorizer-0.2'))
#from colorizer import colorizer

# Add svnversion and upx source directory to the end of the path so it is found
# LAST.
if GetPlatform() == 'win32':
    local_fp_dir = pj(os.getcwd(), 'external', 'svn-win32-1.6.0','bin') 
    os.environ['PATH'] = '%s%s%s' %(local_fp_dir, os.path.pathsep, os.environ['PATH'])
    local_fp_dir = pj(os.getcwd(), 'external', 'FreezePython','upx305w') 
    os.environ['PATH'] = '%s%s%s' %(local_fp_dir, os.path.pathsep, os.environ['PATH'])

# Find flagpoll
sys.stdout.write("Searching for flagpoll...\n")
flagpoll_cmd = WhereIs('flagpoll')

if None == flagpoll_cmd:
    sys.stdout.write("Could not find flagpoll. Please make sure flagpoll is in your PATH.\n")
    sys.stdout.write("Flagpoll can be found here: https://realityforge.vrsource.org/view/FlagPoll\n")
    sys.exit(0)
else:
    sys.stdout.write("Found flagpoll %s\n" % flagpoll_cmd )

def GetSVNVersion( dir = ''):
    cwd = os.getcwd()
    os.chdir( dir )
    cmd_call = os.popen('svnversion')
    svn_str = cmd_call.read().strip()
    if None != cmd_call.close():
        svn_str = 999999
        print "Unable to determine local subversion revision number %s"%svn_str
    else:
        print "%s is at subversion revision number %s"%(dir,svn_str)
    vesSVNRevision = 'SVN_VES_REVISION=\"\\\"%s\\\"\"'%svn_str
    os.chdir( cwd )
    return vesSVNRevision

Export('GetSVNVersion')

################################################################################
########### Setup build dir and name
buildPlatform = distutils.util.get_platform()
##setup platform specific information
pt = platform
machineType = pt.machine()
if machineType == None:
     machineType = 'none'

if GetPlatform() == 'linux':
   kernelVersion = pt.libc_ver()[1]+'-'+pt.dist()[0]+'-'+pt.dist()[1]
else:
   kernelVersion = pt.release()

## setup the uuid for the build directory
buildUUID = GetPlatform()+'.'+kernelVersion+'.'+machineType+'.'+GetArch()
buildUUID = buildUUID.replace('/', '-')
print "Build identifier %s" %buildUUID

if ARGUMENTS.has_key("build_dir"):
   buildDir = ARGUMENTS["build_dir"]
elif GetPlatform() == 'win32':
   buildDir = 'buildwin'
else:
   buildDir = 'build.'+buildUUID

################################################################################
########### Some utility functions

def CreateConfig(target, source, env):
   "Creates the xxx-config file users use to compile against this library"

   targets = map(lambda x: str(x), target)
   sources = map(lambda x: str(x), source)

   submap = env['submap']

   # Build each target from its source
   for i in range(len(targets)):
      print "Generating config file " + targets[i]
      contents = open(sources[i], 'r').read()

      # Go through the substitution dictionary and modify the contents read in
      # from the source file
      for key, value in submap.items():
         #contents = re.sub(re.escape(key), re.escape(value), contents)
         contents = re.sub(key, value, contents)

      # Write out the target file with the new contents
      open(targets[0], 'w').write(contents)
      os.chmod(targets[0], 0755)
   return 0
################################################################################
## Bullet vars
bulletVersion = (int(2), int(76))
bulletBaseVar = 'bullet-2.76'
osgbulletBaseVar = 'osgBullet-1.1'

## Apply Bullet vars
def ApplyBulletVars(env, bulletOnly = False ):
    "apply bullet library info"

    # Bullet defines
    env.AppendUnique(CPPPATH = [pj(RootDir,'external', bulletBaseVar,'src')])
    env.AppendUnique(LIBS = ['bullet'])
    env.AppendUnique(LIBPATH = [ pj('#', buildDir, 'external', bulletBaseVar)])

    if env[ 'ARCH' ] == 'x64':
        env.AppendUnique( CPPDEFINES = ['USE_ADDR64=1'] )

    if bulletOnly:
        return 0
    #env.AppendUnique( CPPDEFINES = ['BULLET_MAJOR_VERSION=%i' %bulletVersion[ 0 ],
    #              'BULLET_MINOR_VERSION=%i' %bulletVersion[ 1 ] ] )  
    env.AppendUnique(CPPPATH = [pj(RootDir,'external', osgbulletBaseVar,'include')])
    env.AppendUnique(CPPPATH = [pj(RootDir,'external','osgBulletPlus','include')])
    env.AppendUnique(LIBS = ['osgBulletPlus','osgBullet'])
    env.AppendUnique(LIBPATH = [pj('#', buildDir, 'external', 'osgBulletPlus'),
                          pj('#', buildDir, 'external', osgbulletBaseVar)])

Export('ApplyBulletVars','osgbulletBaseVar')

##See scons users guide section 15 on variant builds
##Setup some project defaults
################################################################################
################################################################################
################################################################################
################################################################################
# Figure out what version of VE-Suite we're building
def GetVESVersion():
   "Gets the VE-Suite version from the src/ves/VEConfig.h header"

   contents = open( pj('src','ves','VEConfig.h'), 'r').read()
   major = re.compile('.*(#define *VES_MAJOR_VERSION *(\d+)).*', re.DOTALL).sub(r'\2', contents)
   minor = re.compile('.*(#define *VES_MINOR_VERSION *(\d+)).*', re.DOTALL).sub(r'\2', contents)
   patch = re.compile('.*(#define *VES_PATCH_VERSION *(\d+)).*', re.DOTALL).sub(r'\2', contents)
   return (int(major), int(minor), int(patch))

VE_SUITE_VERSION = GetVESVersion()
print 'Building VE-Suite Version: %i.%i.%i' % VE_SUITE_VERSION
help_text = "\n---- VE-Suite Build System ----\n"

# Create the extra builders
# Define a builder for fpc files
builders = {
   'ConfigBuilder'   : Builder(action = CreateConfig)
}

################################################################################
options_cache = 'options.cache.' + buildUUID
##check and see if user wants to use custom options file
if ARGUMENTS.has_key("options_file"):
   opt_file = ARGUMENTS["options_file"]
   if os.path.exists(opt_file):
      print "Reading options from: %s" % str(opt_file)
      options_cache = opt_file
   else:
      print "Options file '%s' not found.. will continue with default '%s'" % \
      (opt_file, options_cache)

opts = SConsAddons.Options.Options(files = [options_cache, 'options.custom'],
                                   args= ARGUMENTS)

options_db_cache = options_cache + SCons.dblite.dblite_suffix
SConsignFile(options_db_cache)

if GetPlatform() == 'win32':
    vtk_options = fp_option.FlagPollBasedOption("VTK",
         "VTK", "5.2", True, True, compileTest=True )
else:
    vtk_options = SConsAddons.Options.StandardPackageOption("vtk",
        "VTK library options, default : vtk_incdir=<vtk>/include/vtk-5.2 vtk_libdir=<vtk>/lib(64)/vtk-5.2", 
        pj('vtkConfigure.h'), library=['vtkImaging','vtkGraphics','vtkCommon','vtkHybrid',
                         'vtkIO','vtkexpat','vtkFiltering','vtkRendering', 
                         'vtkParallel','vtkpng','vtktiff','vtksys','vtkjpeg', 
                         'vtkexoIIc','vtkftgl','vtkfreetype','vtkDICOMParser', 
                         'vtkzlib','vtkNetCDF','vtkverdict',
                         'vtkmetaio','vtksqlite'], 
        symbol="main", required=True)
    if ARGUMENTS.has_key("vtk"):
        vtkBaseDir = ARGUMENTS["vtk"]
        vtk_options.setInitial( {'vtk_incdir' : pj(vtkBaseDir,'include','vtk-5.2'),
                'vtk_libdir': pj(vtkBaseDir,'lib','vtk-5.2')} )
opts.AddOption( vtk_options )

#opts.Add('VtkVersion', 'Set the VTK version so that the VTK version specific include dir can be found', '5.2')
hdf5_options = HDF5.HDF5("hdf5","1.6.5", False, True, ['hdf5','hdf5_cpp','hdf5_hl','sz'])
opts.AddOption(hdf5_options)
hdf4_options = HDF4.HDF4("hdf4","4.2.1", False, True, ['mfhdf','df','jpeg'])
opts.AddOption(hdf4_options)

osg_options = None
#if GetPlatform() == 'win32':
osg_options = fp_option.FlagPollBasedOption("OpenSceneGraph",
                                               "openscenegraph", "1.2", True, True, compileTest=True)
#else:
#   osg_options = SConsAddons.Options.OSG.OSG("osg","1.2", True, True, 
#                        ['osgText', 'osgText',
#                         'osgGA', 'osgDB', 'osgUtil', 'osg', 'OpenThreads',
#                         'osgSim', 'osgFX','osgViewer'])
opts.AddOption( osg_options )
if GetPlatform() == 'win32':
   xerces_options = fp_option.FlagPollBasedOption("xerces",
         "xerces-c", "2.7", True, True, compileTest=True)
else:
   xerces_options = SConsAddons.Options.StandardPackageOption("xercesc",
      "Xerces-C library options, default : xercesc_incdir=<xercesc>/include xercesc_libdir=<xercesc>/lib(64)", 
      pj('xercesc','util','XercesVersion.hpp'), library=['xerces-c'], symbol="main", required=True)
   
opts.AddOption( xerces_options )
wxwidgets_options = None

if GetPlatform() == 'win32':
   wxwidgets_options = fp_option.FlagPollBasedOption("wxWidgets",
         "wxWidgets", "2.8", False, True, compileTest=True)
else:
   wxwidgets_options = SConsAddons.Options.WxWidgets.WxWidgets("wxwidgets","2.8", False, True)
opts.AddOption( wxwidgets_options )

#opts.Add('AprVersion', 'Set the APR version so that the proper apr pkg-config files can be found', '1.0')
opts.Add('VRJugglerVersion', 'Set the VRJuggler version so that the proper flagpoll files can be found', '2.0.2')
opts.Add('VPRVersion', 'Set the VPR version so that the proper VPR flagpoll files can be found', '1.0.2')
opts.Add('VPRProfile', 'If "yes", build applications with VPR profiling enabled', 'no')
opts.Add('prefix', 'Installation prefix', '/usr/local')
##opts.Add('build_test', 'Build the test programs', 'yes')
opts.Add('StaticLibs', 'If yes then build static libraries too', 'no')
opts.Add('MakeDist', 'If "yes", make the distribution packages as part of the build', 'no')
opts.Add('UseMPI', 'If "yes", make 3D texture creator with MPI support', 'no')
opts.Add('validate', 'If "no", do not validate flagpoll packages. Should help speed up the build', 'yes')
opts.Add('buildTests', 'If "yes", Build tests applications', 'no')
opts.Add('buildLog', 'Provide a file name for the build log if you would like a log', '')
opts.Add('options_file', 'Provide a file name for the options caches', '')
opts.Add('build_dir', 'Provide an alternate build directory for variants', buildDir)
opts.Add('SVN_Previous_Date', 'Previous Date to create a change log from. Should be of the form yyyy-mm-dd','')
opts.Add('MakeQtSupport', 'If "yes", add Qt UI controls to Xplorer', 'no')
opts.Add('MakeAspenSupport', 'If "yes", make aspen support', 'no')
opts.Add('MakeDynSimSupport', 'If "yes", make dynsim support', 'no')
opts.Add('MakePowersimSupport', 'If "yes", make powersim support', 'no')
#opts.Add('MakeMinervaSupport', 'If "yes", add GIS support with minerva', 'no')
opts.Add('tecplot_sdk', 'Provide the directory to the root of the Tecplot SDK.', '')

opts.Add('ARCH', 'CPU architecture (ia32, x64)', GetArch() )
if GetPlatform() == 'win32':
    opts.Add('MSVS_ARCH', 'CPU architecture (x86, amd64)', 'x86')
    opts.Add('MSVC_VERSION', 'MSVS version (9.0,8.0)', '8.0')

#opts.Add( 'CharacterController', 'If "yes", then integrate CharacterController into the build', 'no' )
#opts.Add( 'TransformManipulator', 'If "yes", then integrate TransformManipulator into the build', 'no' )

#apr_options = fp_option.FlagPollBasedOption("Apache Portable Runtime",
#                                            "apr-1", "1.0", True, True, helpText=None, compileTest=True, headerToCheck="apr.h")

#apu_options = fp_option.FlagPollBasedOption("Apache Portable Runtime Utils",
#                                             "apr-util-1", "1.0", True, True, helpText=None, compileTest=True, headerToCheck="apu.h")

bullet_options = fp_option.FlagPollBasedOption("Bullet Physics SDK",
                                               "bullet", "2.77", True, True, helpText=None, compileTest=True,
                                               headerToCheck="btBulletCollisionCommon.h")

#need to do a flagpoll check to see if TAO pc or fpc files are available
tao_options = fp_option.FlagPollBasedOption("ACE TAO libraries",
                     "ACE TAO_Valuetype TAO_CosNaming TAO_Svc_Utils TAO_IORTable TAO_Messaging TAO_PortableServer TAO_BiDirGIOP TAO_AnyTypeCode TAO",
                                               "1.5", True, True, helpText=None, compileTest=True,
                                               headerToCheck="ace/ACE.h")

boost_header_options = fp_option.FlagPollBasedOption("Boost Headers",
        "Boost", "1.32.0", True, True, helpText=None, compileTest=True)

boost_options = fp_option.FlagPollBasedOption("Boost Libraries",
        "Boost.Filesystem", "1.32.0", True, True, helpText=None, compileTest=True)

boost_program_options = fp_option.FlagPollBasedOption("Boost Program Options Libraries",
        "Boost.program_options", "1.32.0", True, True, helpText=None, compileTest=True)

boost_signals_options = fp_option.FlagPollBasedOption("Boost Signals Libraries",
        "Boost.Signals", "1.32.0", True, True, helpText=None, compileTest=True)

gmtl_options = fp_option.FlagPollBasedOption("Generic Math Template Library",
                                             "gmtl", "0.5", True, True,
                                             None,
                                             compileTest=True, headerToCheck="gmtl/gmtl.h")

vpr_options = fp_option.FlagPollBasedOption("Vapor",
                                            "vpr", "2.0", True, True,
                                            None,
                                            compileTest=True, headerToCheck="vpr/vpr.h")

gadgeteer_options = fp_option.FlagPollBasedOption("Gadgeteer",
                                                  "gadgeteer", "1.0", True, True,
                                                  None,
                                                  compileTest=True, headerToCheck="gadget/gadgetConfig.h")

vrjuggler_options = SConsAddons.Options.VRJuggler.VRJ.VRJ("VR Juggler", "2.0.2")

osgal_options = fp_option.FlagPollBasedOption("osgAL", "osgAL", "0.6.1", False, True, 
                                              None, 
                                              compileTest=True, headerToCheck="osgAudio/SoundNode.h")

# Setup POCO library
if GetPlatform() == 'win32':
    poco_options = fp_option.FlagPollBasedOption( "POCO", "POCO", "1.3.4", False, True, None,
		                              compileTest = True, headerToCheck = "Poco/Poco.h" )
else:
    poco_options = SConsAddons.Options.StandardPackageOption("POCO",
      "POCO library options, default : POCO_incdir=<POCO>/include POCO_libdir=<POCO>/lib(64)", 
      pj('Poco','Data','SQLite','SQLite.h'), library=['PocoFoundation','PocoData',
      'PocoNet','PocoSQLite','PocoUtil','PocoXML','PocoZip'], symbol="main", required=False)

# Setup osgWorks library
if GetPlatform() == 'win32':
    osgworks_options = fp_option.FlagPollBasedOption( "osgWorks", "osgWorks", "1.0.0", True, True, None,
		                              compileTest = True, headerToCheck = "osgwTools/Version.h" )
else:
    osgworks_options = SConsAddons.Options.StandardPackageOption("osgWorks",
      "osgWorks utility library, default : osgWorks_incdir=<osgWorks>/include osgWorks_libdir=<osgWorks>/lib(64)", 
      pj('osgwTools','Export.h'), library=['osgwTools'], symbol="main", required=True)

#Setup minerva library
minerva_options = fp_option.FlagPollBasedOption( "Minerva", "Minerva", "1.0", False, True, None, 
									  compileTest = False, headerToCheck = "Minerva/Common/Data/Object.h" )

osgbullet_options = fp_option.FlagPollBasedOption("osgBullet",
                                                  "osgbullet", "1.0", True, True,
                                                  None,
                                                  compileTest=True, headerToCheck="osgbBullet/PhysicsState.h")

osgbulletplus_options = fp_option.FlagPollBasedOption("osgBulletPlus",
                                                  "osgbulletplus", "1.0", True, True,
                                                  None,
                                                  compileTest=True, headerToCheck="osgbBulletPlus/DataLoader.h")

bdfx_options = fp_option.FlagPollBasedOption("backdropFX",
                                                  "backdropfx", "0.1.0", True, True,
                                                  None,
                                                  compileTest=True, headerToCheck="backdropFX/Effect.h")

#Setup qt on linux
if GetPlatform() != 'darwin':
   qt_options = fp_option.FlagPollBasedOption("Qt libraries",
      "QtGui QtCore QtOpenGL",
      "4.0", False, True, helpText=None, compileTest=True,
      headerToCheck="QtCore/qglobal.h")
   opts.AddOption( qt_options )
   Export('qt_options')			
						  
#opts.AddOption( apr_options )
#opts.AddOption( apu_options )
opts.AddOption( bullet_options )
opts.AddOption( tao_options )
opts.AddOption( boost_header_options )
opts.AddOption( boost_options )
opts.AddOption( boost_signals_options )
opts.AddOption( boost_program_options )
opts.AddOption( gmtl_options )
opts.AddOption( vpr_options )
opts.AddOption( gadgeteer_options )
opts.AddOption( vrjuggler_options )
opts.AddOption( osgal_options )
opts.AddOption( poco_options )
opts.AddOption( minerva_options )
opts.AddOption( osgworks_options )
opts.AddOption( osgbullet_options )
opts.AddOption( osgbulletplus_options )
opts.AddOption( bdfx_options )

Export( 'opts', 'vtk_options', 'osg_options', 
        'xerces_options','wxwidgets_options',
        'hdf5_options',
        'hdf4_options',
        'VE_SUITE_VERSION',
        'boost_signals_options', 'boost_program_options',
        'bullet_options', 'boost_header_options', 
        'bulletVersion',
        'tao_options',
        'vrjuggler_options', 'boost_options',
        'gmtl_options', 'vpr_options',
        'gadgeteer_options', 'osgal_options',
        'poco_options', 'osgworks_options',
	    'minerva_options',
        'osgbullet_options', 'osgbulletplus_options',
        'bdfx_options' )

##Display some help
help_text = """--- VE-Suite Build system ---
Targets:
   To build VE-Suite:
   > scons
   
   To install VE-Suite:
      install - Install VE-Suite
      > scons install prefix=<build/test> to install in a test build directory
 
   To build a specific component of VE-Suite:
      Build VE-Builder
      > cd VE_Builder
      > scons -u

      Build VE-Open
      > cd VE_Open
      > scons -u

      Build VE-Conductor
      > cd VE_Condutor
      > scons -u

      Build VE-Xplorer
      > cd VE_Xplorer
      > scons -u

      Build VE-CE
      > cd VE_CE
      > scons -u

      Build velauncher.exe:
      > scons freeze
      NOTE (Windows Users): You will need to have Python for Windows Extensions installed:
         http://pywin32.sourceforge.net/

   To create a ChageLog file:
      changelog - Create ChangeLog and ChangeLog.xml
      > scons changelog SVN_Previous_Date=2007-06-01

   To generate documentation:
      > scons docs

   Make sure that:
      vrjuggler, 
      gmtl, 
      cppdom, and
      ACE/TAO 
   are in your FLAGPOLL_PATH or PKGCONFIG_PATH or (DY)LD_LIBRARY_PATH. 
   This is necessaary to auto-detect the dependencies.

   NOTE: An example environment configuration file (ves.setup.csh > csh shell
         & ves.setup.sh > sh shell) is in the VE_Installer directory. This
         file shows how to setup the VE-Suite build environment.
"""

help_text += """
You can store configuration options in the file: options.custom
This file will be loaded each time.  Note: Options are cached in the file
%s
""" % options_cache
# create an initial dictionary to store variables that scons needs 
# to setup the proper build tool. This should be done BEFORE
# the buildEnvironment call is made.
tempEnv = dict(ENV=os.environ)

# Manually read the options file so that we can determine some options
# that are needed to setup the build environment
tempArchWinEnv = dict(ENV=os.environ)
if os.path.exists(options_cache):
    execfile(options_cache, tempArchWinEnv)

# setup common windows specific variables for the build
if GetPlatform() == 'win32':
    if ARGUMENTS.has_key("MSVC_VERSION"):
        tempEnv[ 'MSVC_VERSION' ] = ARGUMENTS[ 'MSVC_VERSION' ]
    elif "MSVS_VERSION" in tempArchWinEnv:
        tempEnv[ 'MSVC_VERSION' ] = tempArchWinEnv[ "MSVS_VERSION" ]
    elif "MSVC_VERSION" in tempArchWinEnv:
        tempEnv[ 'MSVC_VERSION' ] = tempArchWinEnv[ "MSVC_VERSION" ]
    else:
        tempEnv[ 'MSVC_VERSION' ] = "8.0"

    if ARGUMENTS.has_key("MSVS_ARCH"):
        tempEnv[ 'MSVS_ARCH' ] = ARGUMENTS[ 'MSVS_ARCH' ]
    elif "MSVS_ARCH" in tempArchWinEnv:
        tempEnv[ 'MSVS_ARCH' ] = tempArchWinEnv[ "MSVS_ARCH" ]
    else:
        tempEnv[ 'MSVS_ARCH' ] = "x86"

    tempEnv['TARGET_ARCH'] = tempEnv[ 'MSVS_ARCH' ]
    #tempEnv['MSVC_BATCH'] = "1"
    #tempEnv[ 'MSVS_USE_MFC_DIRS' ] = "1"
    #tempEnv[ 'MSVC_USE_SCRIPT' ] = True
    tempEnv[ 'WINDOWS_INSERT_MANIFEST' ] = "1"
    #tempEnv['ENV'][ 'SCONS_MSCOMMON_DEBUG' ] = "test.log"

#tempEnv['CCCOMSTR'] = "Compiling static object $TARGET"
#tempEnv['SHCCCOMSTR'] = "Compiling static object $TARGET"
#tempEnv['SHCXXCOMSTR'] = "Compiling static object $TARGET"
#tempEnv['SHLINKCOMSTR'] = "Linking $TARGET"
#tempEnv['CXXCOMSTR'] = "Compiling static object $TARGET"
#tempEnv['LDMODULECOMSTR'] = "Linking $TARGET"
#tempEnv['LINKCOMSTR'] = "Linking $TARGET"
# Setup a configure build directory for each unique build type
tempEnv['CONFIGUREDIR'] = pj( RootDir, options_cache + "_cache_dir" )

## Create Environment builder from scons addons
## At this point the scons tool is initialized (e.g msvc, g++,...)
base_bldr = EnvironmentBuilder()
## Add options for environment creation
base_bldr.addOptions( opts )

## Setup the cpu architecture
if ARGUMENTS.has_key("ARCH"):
    base_bldr.setCpuArch( ARGUMENTS[ 'ARCH' ] )
elif tempArchWinEnv.has_key("ARCH"):
    base_bldr.setCpuArch( tempArchWinEnv[ 'ARCH' ] )
else:
    base_bldr.setCpuArch()

##Setup windows compiler flags
opts.GetOption('default_opt_level').setInitial(tempArchWinEnv)
opts.GetOption('default_debug_level').setInitial(tempArchWinEnv)
opts.GetOption('default_opt_level').apply(tempArchWinEnv)
opts.GetOption('default_debug_level').apply(tempArchWinEnv)

if tempArchWinEnv['default_debug_level'] != EnvironmentBuilder.NONE:
    base_bldr.enableDebug(EnvironmentBuilder.STANDARD)
    base_bldr.setMsvcRuntime(EnvironmentBuilder.MSVC_MT_DLL_RT)
else:
    base_bldr.setMsvcRuntime(EnvironmentBuilder.MSVC_MT_DLL_RT)

# setup compiler optimizations
if GetPlatform() != 'win32':
    base_bldr.enableOpt( EnvironmentBuilder.MAXIMUM, ["fast_math"] )
else:
    if tempArchWinEnv['default_opt_level'] != EnvironmentBuilder.NONE:
        base_bldr.enableOpt( tempArchWinEnv['default_opt_level'], ["fast_math"] )

## Finally get the environment
baseEnv = base_bldr.buildEnvironment(None,None,**tempEnv)
if GetPlatform() == 'win32':
    print "Using MSVS version %s and for CPU architecture %s." %(baseEnv[ 'MSVC_VERSION' ],baseEnv[ 'MSVS_ARCH' ])

## Now build commands will be colored
#col = colorizer()
#col.colorize( baseEnv )
## Help speed up build times
baseEnv.Decider('MD5-timestamp')
# more info about the option below can be found here:
# http://scons.org/wiki/GoFastButton
baseEnv.SourceCode('.', None)

# Add doxygen builder to the base environment
doxygen.generate(baseEnv)
# Add InnoSetup builder to the base environment
InnoSetup.generate(baseEnv)
# Add qt Tools
qt46.generate(baseEnv)

# Setup help text
help_text += opts.GenerateHelpText(baseEnv)
baseEnv.Help(help_text)

if not SConsAddons.Util.hasHelpFlag():
    # setup initial windows build environment before the options are processed
    if GetPlatform() == 'win32':
        #print "Visual Studio Versions Available %s" %baseEnv[ 'MSVS' ]['VERSIONS']
        # This flag is needed because some packages still use win32 even on win64 systems
        baseEnv.AppendUnique( CPPDEFINES = ['WIN32'] )
        if baseEnv[ 'MSVS_ARCH' ] == "x86":
            baseEnv.AppendUnique( ARFLAGS = ['/MACHINE:X86'], LINKFLAGS = ['/MACHINE:X86','/INCREMENTAL:NO'] )
        else:
            baseEnv.AppendUnique( ARFLAGS = ['/MACHINE:X64'], LINKFLAGS = ['/MACHINE:X64','/INCREMENTAL:NO'] )

    # now lets process everything
    opts.Process(baseEnv)                   # Update the options

    ## Try to save the options if possible and if the user did
    ## not specify an options file
    try:                                   
        if not ARGUMENTS.has_key("options_file"):
            opts.Save(options_cache, baseEnv)
    except LookupError, le:
        pass

    ## see if the options file has the build dir
    if baseEnv['build_dir'] != '':
        buildDir = baseEnv['build_dir']

    # setup compiler warnings
    if GetPlatform() != 'win32':
        base_bldr.enableWarnings( EnvironmentBuilder.MAXIMUM )

    # VTK defines
    baseEnv.AppendUnique( CPPDEFINES = ['VTK_STREAMS_FWD_ONLY'] )
    # basic ves include info
    baseEnv.AppendUnique( CPPPATH = [pj(RootDir,'src'),pj(RootDir,buildDir,'src')] )

    if GetPlatform() == 'darwin':
        baseEnv.AppendUnique( CPPDEFINES = ['_DARWIN'] )
        #baseEnv.AppendUnique( CXXFLAGS = ['-fno-objc-call-cxx-cdtors'] )
        baseEnv.AppendUnique( LINKFLAGS = ['-Wl,-bind_at_load'] )
        #Needed for mac osx 10.6.2 and wxWidgets-2.9.0 w/ cocoa support
        #baseEnv.AppendUnique( FRAMEWORKS = ['CoreFoundation'] )
        baseEnv['LDMODULESUFFIX'] = '.bundle'
        #baseEnv['LDMODULEFLAGS'] = '$LDMODULEFLAGS -bundle -flat_namespace -undefined suppress'

    if GetPlatform() == 'win32':
        # for more information on WIN32_LEAN_AND_MEAN see:
        # http://support.microsoft.com/kb/166474
        # For more info on STRICT
        # http://msdn.microsoft.com/en-us/library/aa383681(VS.85).aspx
        baseEnv.AppendUnique( CPPDEFINES = ['WIN32_LEAN_AND_MEAN','STRICT'] ) 
        # As noted below WINVER will be defined as 0x0502
        # http://msdn.microsoft.com/en-us/library/aa383745(VS.85).aspx
        baseEnv.AppendUnique( CPPDEFINES = ['WINVER=0x0502','_WIN32_WINNT=0x0502'] )
        # setup the windows compilers to compile in the manifest files
        # Embed .manifest into .DLL and .EXE
        baseEnv['LINKCOM'] = [baseEnv['LINKCOM'], 'mt -nologo -manifest ${TARGET}.manifest -outputresource:$TARGET;1']
        baseEnv['SHLINKCOM'] = [baseEnv['SHLINKCOM'], 'mt -nologo -manifest ${TARGET}.manifest -outputresource:$TARGET;2']
        # linker optimizations
        # http://msdn.microsoft.com/en-us/library/fsk896zz.aspx
        baseEnv.AppendUnique( LINKFLAGS = ['/OPT:REF','/OPT:ICF'] )
        #http://msdn.microsoft.com/en-us/library/k1ack8f1%28v=VS.90%29.aspx
        baseEnv.AppendUnique( CXXFLAGS = ['/Oi'] )


    baseEnv = base_bldr.applyToEnvironment( baseEnv.Clone() )

    # Apply boost include path to whole build
    tmpBoostEnv = base_bldr.buildEnvironment()
    boost_options.apply( tmpBoostEnv )
    if tmpBoostEnv.has_key('CXXFLAGS'):
        baseEnv.AppendUnique( CXXFLAGS = tmpBoostEnv['CXXFLAGS'])
    if tmpBoostEnv.has_key('CPPPATH'):
        baseEnv.AppendUnique( CPPPATH = tmpBoostEnv['CPPPATH'] )
    if tmpBoostEnv.has_key('CPPDEFINES'):
        baseEnv.AppendUnique( CPPDEFINES = tmpBoostEnv['CPPDEFINES']  )
    baseEnv.AppendUnique( CPPDEFINES = ['BOOST_ALL_DYN_LINK'] )

    if GetPlatform() != 'win32':
        baseEnv.AppendUnique( LINKFLAGS = ['-g'] )
        baseEnv.AppendUnique( CXXFLAGS = ['-g','-funroll-loops'] )
        #baseEnv.AppendUnique( CXXFLAGS = ['-Wall', '-Wold-style-cast', '-Wundef', '-Wsign-compare', '-Wconversion', '-Wpointer-arith', '-pedantic'] )

    baseEnv.Append(BUILDERS = builders)
    #setup the build dir
    baseEnv.VariantDir(buildDir, '.', duplicate = 0)
    lokiBaseVar = 'loki-0.1.7'
    Export('baseEnv buildDir bulletBaseVar lokiBaseVar')

    # Setup file paths
    PREFIX = os.path.abspath(baseEnv['prefix'])

    if baseEnv.has_key('libdir'):
        LIBDIR = baseEnv['libdir']
    else:
        if baseEnv['ARCH'] == 'x64':
            LIBDIR = 'lib64'
        else:
            LIBDIR = 'lib'
        baseEnv['libdir'] = LIBDIR

    distDir = pj(buildDir, 'dist')
    Export('PREFIX', 'LIBDIR', 'distDir')

    # Create the VE-Suite package
    ves_pkg = sca_auto_dist.Package( name="VE-Suite-%s"%(buildUUID), 
                                    version = "%i.%i.%i"%VE_SUITE_VERSION,
                                    prefix=PREFIX, baseEnv=baseEnv)
 
    Export('ves_pkg')
      
    ## setup build log to aid in debugging remote builds
    if baseEnv[ 'buildLog' ] != '':
        sys.stdout = os.popen("tee "+ baseEnv[ 'buildLog' ], "w")
        sys.stderr = sys.stdout

    ##Tack on path prefixes to subdirs specified above.
    vesSubdirs=pj(buildDir, 'src' )
    distSubdirs = pj(buildDir,'dist','installerImages')
    shareSubdirs = pj(buildDir,'share')
    lokiSubdirs = pj( buildDir, 'external', lokiBaseVar )
    minervaDataSubdirs = pj( buildDir, 'external', 'gdal_data')
    #osgPPUSubdirs = pj( buildDir, 'external', 'osgPPU')
    osgEphemerisSubdirs = pj( buildDir, 'external', 'osgEphemeris')
    osgBulletSubdirs = pj( buildDir, 'external', osgbulletBaseVar)
    osgBulletPlusSubdirs = pj( buildDir, 'external', 'osgBulletPlus')
    bullet = pj( buildDir, 'external', bulletBaseVar)
    test = pj( buildDir, 'test', 'osg')
    issBuilder = pj(buildDir,'dist','win','iss')
    qtTestBuilder = pj(buildDir,'test','qt','BasicQtOffscreenRender')
    qtpropertybrowserSubdirs = pj( buildDir, 'external', 'qtpropertybrowser-2.5_1-opensource' )

    ves_dirs = [vesSubdirs, distSubdirs, osgEphemerisSubdirs,
               shareSubdirs, lokiSubdirs, osgBulletPlusSubdirs,
               osgBulletSubdirs, bullet, minervaDataSubdirs]

    if baseEnv[ 'MakeQtSupport' ] == 'yes':
        ves_dirs.append( qtpropertybrowserSubdirs )
        ves_dirs.append(pj(buildDir, 'external','osgQtTree'))
        if GetPlatform() != 'win32':
            ves_dirs.append( qtTestBuilder )
    
    #build applications in test/ directory
    if baseEnv[ 'buildTests' ] == 'yes':
        testDir = [test,  pj(buildDir,'test','vtk')] #[ pj(buildDir,'test','testNURBS') ]
        ves_dirs.append( testDir )

    # freeze the python code
    #if 'freeze' in  COMMAND_LINE_TARGETS or GetPlatform() == 'win32':
    #    veiFreezeSubdirs = pj(buildDir,'dist', 'build', 'freeze')
    #    ves_dirs.append( veiFreezeSubdirs )
    #    baseEnv.Alias('freeze', veiFreezeSubdirs) 
    
    if 'issBuild' in  COMMAND_LINE_TARGETS:
        if GetPlatform() == 'win32':
            ves_dirs.append( issBuilder )
        else:
            print "The issBuild build option is only available on Windows."
            sys.exit(0)

    # Build the test suite if asked.
    if 'testsuite' in COMMAND_LINE_TARGETS:
        ves_dirs.append(pj('#', 'test'))
        baseEnv.Alias('testsuite', pj('#', 'test'))
   
    ##Run SConscript files in all of those folders.
    for d in ves_dirs:
        SConscript( dirs = d )

    ## create a tar ball of VE-Suite
    if baseEnv['MakeDist'] != 'no':
        ves_pkg.setDistDir( distDir )
        if GetPlatform() == 'linux':
            ves_pkg.addPackager( SConsAddons.AutoDist.TarGzPackager() )
        elif GetPlatform() == 'win32':
            pass
        else:
            ves_pkg.addPackager( SConsAddons.AutoDist.TarGzPackager() )

    # Requires one build on command line to verify options are correct.
    if GetPlatform() == 'win32':
        SConsAddons.AutoDist.GenerateVisualStudioSolution(ves_pkg, 'VisualStudio')

    ##Setup the install flag to install VE-Suite
    if 'install' in COMMAND_LINE_TARGETS:
        ves_pkg.build( install=True )
    else:
        ves_pkg.build( install=False )
         
    baseEnv.Alias('install', PREFIX)
    Default('.')
