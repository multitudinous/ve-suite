#!python
EnsureSConsVersion(0,96)
SConsignFile()

import os, sys, string, smtplib, platform
import distutils.util
from subprocess import *
pj = os.path.join

# Pull in SConsAddons from the source directory if necessary.
try:
   import SConsAddons
except:
   sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'src'))
   sys.path.append(pj(os.getcwd(), 'Tools', 'scons-addons', 'templates'))

# Add flagpoll from the source directory to the end of the path so it is found
# LAST.
local_fp_dir = pj(os.getcwd(), 'Tools', 'flagpoll') 
os.environ['PATH'] += '%s%s' % (os.path.pathsep, local_fp_dir)
sys.path.append(local_fp_dir)

# Set FLAGPOLL_PATH to the local flagpoll if necessary.
if not os.environ.has_key('FLAGPOLL_PATH'):
   os.environ['FLAGPOLL_PATH'] = pj(os.getcwd(), 'Tools', 'flagpoll')

import SConsAddons.Util as sca_util
import SConsAddons.Options as asc_opt
import SConsAddons.Options.Options 
import SConsAddons.Options.VTK
import SConsAddons.Options.OSG
import SConsAddons.Options.Xerces
import SConsAddons.Options.WxWidgets
import SConsAddons.AutoDist as sca_auto_dist
import SConsAddons.Options.FlagPollBasedOption as fp_opt
from SConsAddons.EnvironmentBuilder import EnvironmentBuilder

########### Setup build dir and name
GetPlatform = sca_util.GetPlatform
Export('GetPlatform')
GetArch = sca_util.GetArch
Export('GetArch')
buildPlatform = distutils.util.get_platform()
##setup platform specific information
pt = platform
machineType = pt.machine()
if GetPlatform() == 'linux':
   kernelVersion = pt.libc_ver()[1]+'-'+pt.dist()[0]+'-'+pt.dist()[1]
else:
   kernelVersion = pt.release()

buildUUID = GetPlatform()+'.'+kernelVersion+'.'+machineType+'.'+GetArch()

########### Some utility functions
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

execOsgPatTag = GetTag(True, True, True)
execOsgPatClusterTag = GetTag(True, True, True, True)
Export('execOsgPatTag')
Export('execOsgPatClusterTag')

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
         contents = re.sub(re.escape(key), re.escape(value), contents)

      # Write out the target file with the new contents
      open(targets[0], 'w').write(contents)
      os.chmod(targets[0], 0755)
   return 0

def CheckPackageVersion(context, name, version):
   context.Message( 'Checking for '+name+' >= '+version+'... ')
   fp = 'flagpoll '+name+' --atleast-version='+version
   fp = 'flagpoll '+name+' --modversion'
   ret = context.TryAction(fp)[0]
   context.Result( ret )
   return ret

##See scons users guide section 15 on variant builds
##Setup some project defaults

## make the temp bin and lib dirs in the build dir
##if not os.path.exists( pj( buildDir, 'lib' ) ):
   ##baseEnv.Execute(Mkdir( pj( buildDir, 'lib' ) ))
   ##os.makedirs( pj( buildDir, 'lib' ) )

##if not os.path.exists( pj( buildDir, 'lib', 'flagpoll' ) ):
   ##baseEnv.Execute(Mkdir( pj( buildDir, 'lib', 'flagpoll' ) ))
   ##os.makedirs( pj( buildDir, 'lib', 'flagpoll' ) )

##if not os.path.exists( pj( buildDir, 'bin' ) ):
   ##baseEnv.Execute(Mkdir( pj( buildDir, 'bin' ) ))
   ##os.makedirs( pj( buildDir, 'bin' ) )

################################################################################
################################################################################
################################################################################
# Figure out what version of VE-Suite we're building
VE_SUITE_VERSION = ( int('1'), int('0'), int('2') )
print 'Building VE-Suite Version: %i.%i.%i' % VE_SUITE_VERSION

help_text = "\n---- VE-Suite Build System ----\n"

# Get command-line arguments
##optimize = ARGUMENTS.get('optimize', 'no')
##profile = ARGUMENTS.get('profile', 'no')
##PREFIX = ARGUMENTS.get('prefix', '/usr/local')
##Prefix(PREFIX)
##Export('PREFIX')

##Export('optimize')
##print "Install prefix: ", Prefix()

# Create the extra builders
# Define a builder for the gmtl-config script
builders = {
   'ConfigBuilder'   : Builder(action = CreateConfig)
}
   

################################################################################
options_cache = 'options.cache.' + buildUUID
opts = SConsAddons.Options.Options(files = [options_cache, 'options.custom'],
                                   args= ARGUMENTS)

opts.Add('VtkVersion', 'Set the VTK version so that the VTK version specific include dir can be found', '5.0')
vtk_options = SConsAddons.Options.VTK.VTK("vtk","5.0", True, True,
                        ['vtkImaging','vtkGraphics','vtkCommon','vtkHybrid',
                         'vtkIO','vtkexpat','vtkFiltering','vtkRendering', 
                         'vtkParallel','vtkpng','vtktiff','vtksys','vtkjpeg', 
                         'vtkexoIIc','vtkftgl','vtkfreetype','vtkDICOMParser', 
                         'vtkzlib','vtkMPEG2Encode','vtkNetCDF'])
opts.AddOption( vtk_options )
osg_options = SConsAddons.Options.OSG.OSG("osg","1.2", True, True, 
                        ['osgText', 'osgProducer', 'Producer', 'osgText',
                         'osgGA', 'osgDB', 'osgUtil', 'osg', 'OpenThreads',
                         'osgSim', 'osgFX'])
opts.AddOption( osg_options )
xerces_options = SConsAddons.Options.Xerces.Xerces("xerces","1.0", True, True)
opts.AddOption( xerces_options )
wxwidgets_options = SConsAddons.Options.WxWidgets.WxWidgets("wxwidgets","2.6", True, True)
opts.AddOption( wxwidgets_options )
opts.Add('prefix', 'Installation prefix', '/usr/local')
##opts.Add('libdir', 'Library installation directory under <prefix>')
##opts.Add('build_test', 'Build the test programs', 'yes')
opts.Add('StaticOnly', 'If not "no" then build only static library', 'no')
opts.Add('MakeDist', 'If true, make the distribution packages as part of the build', 'no')
opts.Add('Patented', 'If true, make the patented version of VE-Suite', 'yes')
opts.Add('tao', 'If true, use TAO in the build', 'yes')
## do not need this one since we build cluster and non cluster by default now
##opts.Add('cluster', 'If true, build the cluster version of VE-Xplorer', 'no')
opts.Add('AprVersion', 'Set the APR version so that the proper apr pkg-config files can be found', '1.0')
opts.Add('buildLog', 'Provide a file name for the build log if you would like a log', '')
##opts.Add('arch', 'CPU architecture (ia32, x86_64, or ppc)',
##         cpu_arch_default)
Export('opts', 'vtk_options', 'osg_options','xerces_options','wxwidgets_options')

base_bldr = EnvironmentBuilder()
##base_bldr.addOptions( opts )
baseEnv = base_bldr.buildEnvironment()
baseEnv[ 'ENV' ] = os.environ

##check for flagpoll packages
conf = Configure(baseEnv, custom_tests = {'CheckPackageVersion' : CheckPackageVersion })

if not conf.CheckPackageVersion('flagpoll','0.8.1'):
   print 'flagpoll >= 0.8.1 not found.'
   Exit(1)

if not conf.CheckPackageVersion('TAO','1.5'):
   print 'TAO >= 1.5 not found.'
   Exit(1)

if not conf.CheckPackageVersion('vrjuggler','2.0.1'):
   print 'vrjuggler >= 2.0.1 not found.'
   Exit(1)
baseEnv = conf.Finish()

##Display some help
help_text = """--- VE-Suite Build system ---
Targets:
   To build VE-Suite:
   > scons
   
   To install VE-Suite:
      install - Install VE-Suite
      > scons install prefix=build/test' to install in a test build directory
 
   To build a specific component of VE-Suite:
      builder - Build VE-Builder
      > scons builder

      open - Build VE-Open
      > scons open

      conductor - Build VE-Conductor
      > scons conductor

      xplorer - Build VE-Xplorer
      > scons xplorer

      ce - Build VE-CE
      > scons ce

   Make sure that vrjuggler, Boost.Filesystem, ACE/TAO, and apr/apr-util are
   in your FLAGPOLL_PATH or PKGCONFIG_PATH or (DY)LD_LIBRARY_PATH. This is
   necessaary to auto-detect the dependencies.
"""

help_text += """
You can store configuration options in the file: options.custom
This file will be loaded each time.  Note: Options are cached in the file
%s
""" % options_cache

help_text += opts.GenerateHelpText(baseEnv)

baseEnv.Help(help_text)

if not SConsAddons.Util.hasHelpFlag():
   opts.Process(baseEnv, None, True)                   # Update the options

   try:                                   # Try to save the options if possible
      opts.Save(options_cache, baseEnv)
   except LookupError, le:
      pass
   
   ## read the builder options after they have been added to the env
   ##base_bldr.readOptions( baseEnv )
   ##base_bldr = base_bldr.clone()
   baseEnv = base_bldr.applyToEnvironment( baseEnv.Copy() )
   ## load environment of the shell that scons is launched from   
   baseEnv.Append(CPPPATH = ['#'])
   baseEnv.Append( CPPDEFINES = ['_TAO','VE_PATENTED','_OSG','VTK44'] )
   #setup the build dir
   buildDir = 'build.'+buildUUID
   baseEnv.BuildDir(buildDir, '.', duplicate = 0)
   baseEnv[ 'cluster' ] = 'no'
   Export('buildDir')
   Export('baseEnv')

   # Setup file paths
   PREFIX = os.path.abspath(baseEnv['prefix'])

   # Create the GMTL package
   ves_pkg = sca_auto_dist.Package(name="VE_Suite", version = "%i.%i.%i"%VE_SUITE_VERSION,
                                 prefix=PREFIX, baseEnv=baseEnv)
   ##ves_pkg.addExtraDist(Split("""
   ##      AUTHORS
   ##      ChangeLog
   ##      COPYING
   ##      README
   ##   """))
   Export('ves_pkg')
   
   # Process subdirectories
   ##subdirs = []
   ##SConscript(dirs = subdirs)

   # Setup the builder for gmtl-config
   ##env = baseEnv.Copy(BUILDERS = builders)
   ##gmtl_pc_submap = {
   ##      '@prefix@'                    : PREFIX,
   ##      '@exec_prefix@'               : '${prefix}',
   ##      '@gmtl_cxxflags@'             : '',
   ##      '@includedir@'                : pj(PREFIX, 'include'),
   ##      '@gmtl_extra_cxxflags@'       : '',
   ##      '@gmtl_extra_include_dirs@'   : '',
   ##      '@version_major@'             : str(GMTL_VERSION[0]),
   ##      '@version_minor@'             : str(GMTL_VERSION[1]),
   ##      '@version_patch@'             : str(GMTL_VERSION[2]),
   ##   }
   ##env.ConfigBuilder('gmtl.pc','gmtl.pc.in',submap = gmtl_pc_submap)
   ##installed_targets += env.Install(pj(PREFIX, 'share', 'pkgconfig'), 'gmtl.pc')

   if baseEnv.has_key('libdir'):
      LIBDIR = baseEnv['libdir']
   else:
      if GetArch() == 'x86_64':
         LIBDIR = 'lib64'
      else:
         LIBDIR = 'lib'

   distDir = pj(buildDir, 'dist')
   Export('buildDir', 'PREFIX', 'LIBDIR', 'distDir')
   
   # Setup package
   ##CPPDOM_VERSION
   ##cppdom_pkg = AutoDist.Package(name="cppdom", version = "%s.%s.%s"%CPPDOM_VERSION,
   ##                              prefix=PREFIX, baseEnv=baseEnv)
   ##cppdom_pkg.addExtraDist(Split("""
   ##      AUTHORS
   ##      ChangeLog
   ##      COPYING
   ##      README
   ##   """))
   ##Export('cppdom_pkg')
   
   if baseEnv['MakeDist'] != 'no':
      cppdom_pkg.setDistDir( distDir )
      if GetPlatform() == 'linux':
         cppdom_pkg.addPackager( SConsAddons.AutoDist.TarGzPackager() )
         #cppdom_pkg.addPackager( SConsAddons.AutoDist.RpmPackager('cppdom.spec'))
      elif GetPlatform() == 'win32':
         pass
      else:
         cppdom_pkg.addPackager( SConsAddons.AutoDist.TarGzPackager() )
   
   ## setup build log to aid in debugging remote builds
   if baseEnv[ 'buildLog' ] != '':
      sys.stdout = os.popen("tee "+ baseEnv[ 'buildLog' ], "w")
      sys.stderr = sys.stdout
   
   ##Tack on path prefixes to subdirs specified above.
   builderSubdirs=pj(buildDir, 'VE_Builder')
   ##builderSubdirs = map(lambda s: pj(buildDir, 'VE_Builder', s), builderSubdirs)
   openSubdirs = pj(buildDir,'VE_Open')
   ##openSubdirs = map(lambda s: pj(buildDir, s), openSubdirs)
   conductorSubdirs = pj(buildDir, 'VE_Conductor')
   ##conductorSubdirs = map(lambda s: pj(buildDir, 'VE_Conductor', s), conductorSubdirs)
   xplorerSubdirs = pj(buildDir, 'VE_Xplorer')
   ##xplorerSubdirs = map(lambda s: pj(buildDir, 'VE_Xplorer', s), xplorerSubdirs)
   ceSubdirs = pj(buildDir,'VE_CE')
   ##ceSubdirs = map(lambda s: pj(buildDir, s), ceSubdirs)
   veiSubdirs = pj(buildDir,'VE_Installer','installer')

   ##Set the Sconscript files to build.
   if 'xplorer' in COMMAND_LINE_TARGETS:
      ves_dirs = [ xplorerSubdirs ]
      baseEnv.Alias('xplorer', xplorerSubdirs)
   elif 'builder' in COMMAND_LINE_TARGETS:
      ves_dirs = [ builderSubdirs ]
      baseEnv.Alias('builder', builderSubdirs)
   elif 'ce' in COMMAND_LINE_TARGETS:
      ves_dirs = [ ceSubdirs ]
      baseEnv.Alias('ce', ceSubdirs)
   elif 'open' in COMMAND_LINE_TARGETS:
      ves_dirs = [ openSubdirs ]
      baseEnv.Alias('open', openSubdirs)
   elif 'conductor' in COMMAND_LINE_TARGETS:
      ves_dirs = [ conductorSubdirs ]
      baseEnv.Alias('conductor', conductorSubdirs)
   else:
      ves_dirs = [ openSubdirs, builderSubdirs, conductorSubdirs, xplorerSubdirs, ceSubdirs, veiSubdirs]

   ##Run SConscript files in all of those folders.
   for d in ves_dirs:
      SConscript( dirs = d )

   ##Setup the install flag to install VE-Suite
   if 'install' in COMMAND_LINE_TARGETS:
      ves_pkg.build( install=True )
   else:
      ves_pkg.build( install=False )
   
   ##Install it if the packages have been setup to do so
   baseEnv.Alias('install',PREFIX)
   Default('.')

