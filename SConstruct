#!python
EnsureSConsVersion(0,96)
SConsignFile()

###
from SCons.Defaults import SharedCheck, ProgScan
from SCons.Script.SConscript import SConsEnvironment
import fnmatch
###

import os, sys, re,string, smtplib,platform
import distutils.util
import commands
from time import sleep
pj = os.path.join

# Pull in SConsAddons from the source directory if necessary.
try:
   import SConsAddons
except:
   sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'src'))
   sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'templates'))

# Pull in HDF options files
try:
   import HDF5
   import HDF4
except:
   sys.path.append(pj(os.getcwd(), 'share', 'python'))
   import HDF5
   import HDF4

# Add flagpoll from the source directory to the end of the path so it is found
# LAST.
#local_fp_dir = pj(os.getcwd(), 'Tools', 'flagpoll') 
#os.environ['PATH'] = '%s%s%s' %(local_fp_dir, os.path.pathsep, os.environ['PATH'])
sys.stdout.write("Searching for flagpoll...\n")
flagpoll_cmd = WhereIs('flagpoll')

if None == flagpoll_cmd:
    sys.stdout.write("Could not find flagpoll. Please make sure flagpoll is in your PATH.\n")
    sys.stdout.write("Flagpoll can be found here: https://realityforge.vrsource.org/view/FlagPoll\n")
    sys.exit(0)
else:
    sys.stdout.write("Found flagpoll %s\n" % flagpoll_cmd )

# Set FLAGPOLL_PATH to the local flagpoll if necessary.
#if not os.environ.has_key('FLAGPOLL_PATH'):
#   os.environ['FLAGPOLL_PATH'] = pj(os.getcwd(), 'Tools', 'flagpoll')
# determine the svn version of the local vesuite build
# pass in the real src directory not the scons build dir
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

import SConsAddons.Util as sca_util
import SConsAddons.Options as asc_opt
import SConsAddons.Options.Options 
import SConsAddons.Options.VTK
import SConsAddons.Options.OSG
import SConsAddons.Options.VRJuggler.VRJ
import SConsAddons.Options.Boost
#import SConsAddons.Options.OPAL
#import SConsAddons.Options.ODE
import SConsAddons.Options.Xerces
import SConsAddons.Options.WxWidgets
import SConsAddons.AutoDist as sca_auto_dist
import SConsAddons.Options.FlagPollBasedOption as fp_option
from SConsAddons.EnvironmentBuilder import EnvironmentBuilder

################################################################################
########### Setup build dir and name
RootDir = os.getcwd()
Export('RootDir')
GetPlatform = sca_util.GetPlatform
Export('GetPlatform')
GetArch = sca_util.GetArch
Export('GetArch')
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

##See scons users guide section 15 on variant builds
##Setup some project defaults
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


vtk_options = SConsAddons.Options.VTK.VTK("vtk","5.0", True, True,
                        ['vtkImaging','vtkGraphics','vtkCommon','vtkHybrid',
                         'vtkIO','vtkexpat','vtkFiltering','vtkRendering', 
                         'vtkParallel','vtkpng','vtktiff','vtksys','vtkjpeg', 
                         'vtkexoIIc','vtkftgl','vtkfreetype','vtkDICOMParser', 
                         'vtkzlib','vtkNetCDF','verdict'])
opts.AddOption( vtk_options )
opts.Add('VtkVersion', 'Set the VTK version so that the VTK version specific include dir can be found', '5.0')
hdf5_options = HDF5.HDF5("hdf5","1.6.5", False, True, ['hdf5','hdf5_cpp','hdf5_hl','sz'])
opts.AddOption(hdf5_options)
hdf4_options = HDF4.HDF4("hdf4","4.2.1", False, True, ['mfhdf','df','jpeg'])
opts.AddOption(hdf4_options)

osg_options = None
if GetPlatform() == 'win32':
   osg_options = fp_option.FlagPollBasedOption("OSG",
                                               "OpenSceneGraph", "1.2", True, True)
else:
   osg_options = SConsAddons.Options.OSG.OSG("osg","1.2", True, True, 
                        ['osgText', 'osgText',
                         'osgGA', 'osgDB', 'osgUtil', 'osg', 'OpenThreads',
                         'osgSim', 'osgFX','osgViewer'])
opts.AddOption( osg_options )
if GetPlatform() == 'win32':
   xerces_options = fp_option.FlagPollBasedOption("xerces",
         "xerces-c", "2.7", True, True)
else:
   xerces_options = SConsAddons.Options.Xerces.Xerces("xerces","1.0", True, True)
   
opts.AddOption( xerces_options )
wxwidgets_options = None

if GetPlatform() == 'win32':
   wxwidgets_options = fp_option.FlagPollBasedOption("wxWidgets",
         "wxWidgets", "2.8", True, True)
else:
   wxwidgets_options = SConsAddons.Options.WxWidgets.WxWidgets("wxwidgets","2.8", True, True)
opts.AddOption( wxwidgets_options )

opts.Add('AprVersion', 'Set the APR version so that the proper apr pkg-config files can be found', '1.0')
opts.Add('VRJugglerVersion', 'Set the VRJuggler version so that the proper flagpoll files can be found', '2.0.2')
opts.Add('BoostVersion', 'Set the Boost version so that the proper Boost flagpoll files can be found', '1.31')
opts.Add('VPRVersion', 'Set the VPR version so that the proper VPR flagpoll files can be found', '1.0.2')
opts.Add('VPRProfile', 'If "yes", build applications with VPR profiling enabled', 'no')
opts.Add('prefix', 'Installation prefix', '/usr/local')
##opts.Add('build_test', 'Build the test programs', 'yes')
opts.Add('StaticLibs', 'If yes then build static libraries too', 'no')
opts.Add('MakeDist', 'If "yes", make the distribution packages as part of the build', 'no')
opts.Add('Patented', 'If "yes", make the patented version of VE-Suite', 'no')
opts.Add('UseMPI', 'If "yes", make 3D texture creator with MPI support', 'no')
opts.Add('validate', 'If "no", do not validate flagpoll packages. Should help speed up the build', 'yes')
opts.Add('buildTests', 'If "yes", Build tests applications', 'no')
##Added options for velauncher build.
##opts.Add('LauncherExe', 'If true, builds velauncher.py as an executable', 'yes')
##opts.Add('CxPath', "Set CXPATH to find 
##opts.Add('PythonHome', "Set PYTHONHOME to find python's executable and libs", '')
##End added velauncher build options.
opts.Add('buildLog', 'Provide a file name for the build log if you would like a log', '')
opts.Add('options_file', 'Provide a file name for the options caches', '')
opts.Add('build_dir', 'Provide an alternate build directory for variants', buildDir)
opts.Add('SVN_Previous_Date', 'Previous Date to create a change log from. Should be of the form yyyy-mm-dd','')
##opts.Add('arch', 'CPU architecture (ia32, x86_64, or ppc)',
##         cpu_arch_default)

apr_options = fp_option.FlagPollBasedOption("Apache Portable Runtime",
                                            "apr-1", "1.0", True, True, compileTest=True, headerToCheck="apr.h")

apu_options = fp_option.FlagPollBasedOption("Apache Portable Runtime Utils",
                                             "apr-util-1", "1.0", True, True, compileTest=True, headerToCheck="apu.h")

bullet_options = fp_option.FlagPollBasedOption("Bullet Physics SDK",
                                               "bullet", "0.1", True, True, compileTest=True,
                                               headerToCheck="btBulletCollisionCommon.h")

tao_options = fp_option.FlagPollBasedOption("ACE TAO libraries",
                     "ACE TAO_Valuetype TAO_CosNaming TAO_Svc_Utils TAO_IORTable TAO_Messaging TAO_PortableServer TAO_BiDirGIOP TAO_AnyTypeCode TAO",
                                               "1.5", True, True, compileTest=True,
                                               headerToCheck="ace/ACE.h")
if GetPlatform() != 'win32':
    boost_options = SConsAddons.Options.Boost.Boost("boost", "1.32.0",
                                                libs = ['filesystem'],
                                                required = True,
                                                useVersion = True,
                                                autoLink = True)
else:
    boost_options = fp_option.FlagPollBasedOption("Boost Libraries",
        "Boost.Filesystem", "1.33.1", True, True)

vrjuggler_options = SConsAddons.Options.VRJuggler.VRJ.VRJ("VR Juggler", "2.0.2")

gmtl_options = fp_option.FlagPollBasedOption("Generic Math Template Library",
                                               "gmtl", "0.5", True, True)

vpr_options = fp_option.FlagPollBasedOption("Vapor",
                                               "vpr", "2.0", True, True)

gadgeteer_options = fp_option.FlagPollBasedOption("Gadgeteer",
                                               "gadgeteer", "1.0", True, True)

osgal_options = fp_option.FlagPollBasedOption("osgAL", "osgAL", "0.6.1", False, True, None, True, headerToCheck=None)

opts.AddOption( apr_options )
opts.AddOption( apu_options )
opts.AddOption( bullet_options )
opts.AddOption( tao_options )
opts.AddOption( vrjuggler_options )
opts.AddOption( boost_options )
opts.AddOption( gmtl_options )
opts.AddOption( vpr_options )
opts.AddOption( gadgeteer_options )
opts.AddOption( osgal_options )

Export('opts', 'vtk_options', 'osg_options', 
         'xerces_options','wxwidgets_options',
         'hdf5_options',
         'hdf4_options',
         'VE_SUITE_VERSION',
         'apr_options', 'apu_options',
         'bullet_options', 'tao_options',
         'vrjuggler_options', 'boost_options',
         'gmtl_options', 'vpr_options',
         'gadgeteer_options', 'osgal_options')

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

   To create a ChageLog file:
      changelog - Create ChangeLog and ChangeLog.xml
      > scons changelog SVN_Previous_Date=2007-06-01' you can change a start date

   To generate the doxygen documents:
      doxygen - Generate Doxygen Document
      > scons doxygen

   Make sure that:
      vrjuggler, 
      gmtl, 
      cppdom, 
      ACE/TAO, 
      bullet, and 
      apr/apr-util 
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

## Create Environment builder from scons addons
base_bldr = EnvironmentBuilder()
## Add debug options in for vesuite from SConsAddons 
base_bldr.addOptions( opts )

baseEnv = base_bldr.buildEnvironment()
baseEnv = base_bldr.buildEnvironment(ENV = os.environ)
if GetPlatform() == 'darwin':
   # Load some default tools
   baseEnv.AppendUnique(tools=('default', TOOL_SUBST))
   #baseEnv.AppendUnique(tools=('default', TOOL_BUNDLE))
   TOOL_BUNDLE( baseEnv )

help_text += opts.GenerateHelpText(baseEnv)
baseEnv.Help(help_text)

if not SConsAddons.Util.hasHelpFlag():
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

   if baseEnv['Patented'] == 'yes':
      baseEnv.Append( CPPDEFINES = ['VE_PATENTED'] )
      buildDir += '.patented'

   if baseEnv['default_debug_level'] != EnvironmentBuilder.NONE:
      base_bldr.enableDebug()
      base_bldr.setMsvcRuntime(EnvironmentBuilder.MSVC_MT_DBG_DLL_RT)
   else:
      base_bldr.enableOpt()
      base_bldr.setMsvcRuntime(EnvironmentBuilder.MSVC_MT_DLL_RT)

   if osgal_options.isAvailable():
      baseEnv.Append( CPPDEFINES = ['VE_SOUND'] )  

   baseEnv = base_bldr.applyToEnvironment( baseEnv.Copy() )
   ## load environment of the shell that scons is launched from   
   ##possible additional flags
   baseEnv.Append( CPPPATH = [pj(RootDir,'src'),pj(RootDir,buildDir,'src')] )
   baseEnv.Append( CPPDEFINES = ['_OSG','VTK44','LOKI_OBJECT_LEVEL_THREADING'] )
   if GetPlatform() == 'win32':
        baseEnv.Append( CPPDEFINES = ['BOOST_ALL_DYN_LINK','LOKI_FUNCTOR_IS_NOT_A_SMALLOBJECT','LOKI_OBJECT_LEVEL_THREADING'] )
   
   if GetPlatform() == 'darwin':
      baseEnv.Append( LINKFLAGS = ['-Wl,-bind_at_load'] )

   #setup default libraries and defines
   baseEnv.Append( CPPPATH = [pj(RootDir,'external', 'loki-0.1.6', 'include')] )
   baseEnv.Append( LIBS = ['loki.0.1.6'] )
   baseEnv.Append( LIBPATH = [pj(RootDir, buildDir,'external', 'loki-0.1.6')] )
   #baseEnv.Append( CXXFLAGS = ['-Wall', '-Wold-style-cast', '-Wundef', '-Wsign-compare', '-Wconversion', '-Wpointer-arith', '-pedantic'] )

   #baseEnv.Append( LINKFLAGS = ['-g'] )
   #baseEnv.Append( CXXFLAGS = ['-g'] )

   if GetPlatform() == 'win32':
      baseEnv.Append( WINDOWS_INSERT_MANIFEST = True )

   baseEnv.Append(BUILDERS = builders)
   #setup the build dir
   baseEnv.BuildDir(buildDir, '.', duplicate = 0)
   Export('baseEnv')
   Export('buildDir')


   # Setup file paths
   PREFIX = os.path.abspath(baseEnv['prefix'])

   if baseEnv.has_key('libdir'):
      LIBDIR = baseEnv['libdir']
   else:
      if GetArch() == 'x86_64':
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
   #veiDistSubdirs = pj(buildDir,'VE_Installer','installer', 'dist')
   #fpcSubdirs = pj(buildDir,'VE_Installer','fpc')
   #installerSubdirs = pj(buildDir,'VE_Installer' )
   shareSubdirs = pj(buildDir,'share')
   lokiSubdirs = pj( buildDir, 'external', 'loki-0.1.6')
   osgOQSubdirs = pj( buildDir, 'external', 'osgOQ')
   osgEphemerisSubdirs = pj( buildDir, 'external', 'osgEphemeris')
   
   ves_dirs = [vesSubdirs, distSubdirs, osgEphemerisSubdirs,
               shareSubdirs, lokiSubdirs, osgOQSubdirs]

   #build applications in test/ directory
   if baseEnv[ 'buildTests' ] == 'yes':
      testDir = pj(buildDir,'test','testNURBS')
      ves_dirs.append( testDir )

   # freeze the python code
   if 'freeze' in  COMMAND_LINE_TARGETS:
      veiFreezeSubdirs = pj(buildDir,'VE_Installer','installer', 'freezeCode')	
      ves_dirs.append( veiFreezeSubdirs )
      baseEnv.Alias('freeze', veiFreezeSubdirs) 
     
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

