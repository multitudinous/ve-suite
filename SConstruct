#!python
EnsureSConsVersion(0,96)
SConsignFile()

import os, sys, re,string, smtplib,platform
import distutils.util
import commands
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
os.environ['PATH'] = '%s%s%s' %(local_fp_dir, os.path.pathsep, os.environ['PATH'])

# Set FLAGPOLL_PATH to the local flagpoll if necessary.
if not os.environ.has_key('FLAGPOLL_PATH'):
   os.environ['FLAGPOLL_PATH'] = pj(os.getcwd(), 'Tools', 'flagpoll')

import SConsAddons.Util as sca_util
import SConsAddons.Options as asc_opt
import SConsAddons.Options.Options 
import SConsAddons.Options.VTK
import SConsAddons.Options.OSG
#import SConsAddons.Options.OPAL
#import SConsAddons.Options.ODE
import SConsAddons.Options.Xerces
import SConsAddons.Options.WxWidgets
import SConsAddons.AutoDist as sca_auto_dist
import SConsAddons.Options.FlagPollBasedOption as fp_opt
from SConsAddons.EnvironmentBuilder import EnvironmentBuilder

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
if GetPlatform() == 'linux':
   kernelVersion = pt.libc_ver()[1]+'-'+pt.dist()[0]+'-'+pt.dist()[1]
else:
   kernelVersion = pt.release()

buildUUID = GetPlatform()+'.'+kernelVersion+'.'+machineType+'.'+GetArch()
buildDir = 'build.'+buildUUID
Export('buildDir')

########### Some utility functions
def GetTag(execTag = False, osgTag = False,
           patentedTag = False, clusterTag = False):
    """Creates a combined tag for libraries and programs."""
    ##Combine the tags.
    finalTag = ''
    if execTag:
        exec_tag = '_tao'
        finalTag += exec_tag
    if osgTag:
        osg_tag = "_osg"
        finalTag += osg_tag
    if patentedTag:
        patented_tag = "_vep"
        finalTag += patented_tag
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
         #contents = re.sub(re.escape(key), re.escape(value), contents)
         contents = re.sub(key, value, contents)

      # Write out the target file with the new contents
      open(targets[0], 'w').write(contents)
      os.chmod(targets[0], 0755)
   return 0

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

opts.Add('VtkVersion', 'Set the VTK version so that the VTK version specific include dir can be found', '5.0')
vtk_options = SConsAddons.Options.VTK.VTK("vtk","5.0", True, True,
                        ['vtkImaging','vtkGraphics','vtkCommon','vtkHybrid',
                         'vtkIO','vtkexpat','vtkFiltering','vtkRendering', 
                         'vtkParallel','vtkpng','vtktiff','vtksys','vtkjpeg', 
                         'vtkexoIIc','vtkftgl','vtkfreetype','vtkDICOMParser', 
                         'vtkzlib','vtkNetCDF','verdict'])
opts.AddOption( vtk_options )
osg_options = SConsAddons.Options.OSG.OSG("osg","1.2", True, True, 
                        ['osgText', 'osgProducer', 'Producer', 'osgText',
                         'osgGA', 'osgDB', 'osgUtil', 'osg', 'OpenThreads',
                         'osgSim', 'osgFX'])
opts.AddOption( osg_options )
xerces_options = SConsAddons.Options.Xerces.Xerces("xerces","1.0", True, True)
opts.AddOption( xerces_options )
opts.Add('AprVersion', 'Set the APR version so that the proper apr pkg-config files can be found', '1.0')
opts.Add('VRJugglerVersion', 'Set the VRJuggler version so that the proper flagpoll files can be found', '2.0.2')
opts.Add('BoostVersion', 'Set the Boost version so that the proper Boost flagpoll files can be found', '1.33.1')
opts.Add('VPRVersion', 'Set the VPR version so that the proper VPR flagpoll files can be found', '1.0.2')
wxwidgets_options = SConsAddons.Options.WxWidgets.WxWidgets("wxwidgets","2.8", True, True)
opts.AddOption( wxwidgets_options )
opts.Add('prefix', 'Installation prefix', '/usr/local')
##opts.Add('build_test', 'Build the test programs', 'yes')
opts.Add('StaticOnly', 'If not "no" then build only static library', 'no')
opts.Add('MakeDist', 'If true, make the distribution packages as part of the build', 'no')
opts.Add('Patented', 'If true, make the patented version of VE-Suite', 'yes')
opts.Add('tao', 'If true, use TAO in the build', 'yes')
##Added options for velauncher build.
##opts.Add('LauncherExe', 'If true, builds velauncher.py as an executable', 'yes')
##opts.Add('CxPath', "Set CXPATH to find 
##opts.Add('PythonHome', "Set PYTHONHOME to find python's executable and libs", '')
##End added velauncher build options.
opts.Add('buildLog', 'Provide a file name for the build log if you would like a log', '')
opts.Add('options_file', 'Provide a file name for the options caches', '')
opts.Add('SVN_Previous_Date', 'Previous Date to create a change log from','')
##opts.Add('arch', 'CPU architecture (ia32, x86_64, or ppc)',
##         cpu_arch_default)
Export('opts', 'vtk_options', 'osg_options','xerces_options','wxwidgets_options')#,'ode_options','opal_options')

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

base_bldr = EnvironmentBuilder()
##base_bldr.addOptions( opts )
baseEnv = base_bldr.buildEnvironment()
baseEnv[ 'ENV' ] = os.environ
help_text += opts.GenerateHelpText(baseEnv)
baseEnv.Help(help_text)

if not SConsAddons.Util.hasHelpFlag():
   # now lets process everything
   opts.Process(baseEnv, None, True)                   # Update the options
   
   # check the apr and apu utilities
   # there is probably an easier way to do this so feel free to simplify
   aprVersion = '0.9'
   aprCommand = 'apr'
   apuCommand = 'apr-util'
   #apuLib = 'aprutil'
   if baseEnv.has_key('AprVersion'):
      aprVersion = baseEnv[ 'AprVersion' ]
      if baseEnv[ 'AprVersion' ] >= "1.0":
         aprCommand = 'apr-1'
         apuCommand = 'apr-util-1'
         #apuLib = 'aprutil-1'

   fgpApr = sca_util.FlagPollParser( aprCommand )
   if not fgpApr.validate( baseEnv, "apr.h", aprVersion ):
      Exit(1)

   fgpApu = sca_util.FlagPollParser( apuCommand )
   if not fgpApu.validate( baseEnv, "apu.h", aprVersion ):
      Exit(1)

   fgpBullet = sca_util.FlagPollParser('bullet')
   if not fgpBullet.validate( baseEnv, "btBulletCollisionCommon.h", '0.1' ):
      Exit(1)

   fgpTAO = sca_util.FlagPollParser('TAO')
   if not fgpTAO.validate( baseEnv, "ace/ACE.h", '1.5' ):
      Exit(1)

   fgpVrjuggler = sca_util.FlagPollParser('vrjuggler')
   if not fgpVrjuggler.validate( baseEnv, "vrj/vrjConfig.h", baseEnv['VRJugglerVersion']):
      Exit(1)

   fgpBoost = sca_util.FlagPollParser('Boost.Filesystem')
   if not fgpBoost.validate( baseEnv, "boost/filesystem/operations.hpp", baseEnv['BoostVersion']):
      Exit(1)

   ## Try to save the options if possible and if the user did
   ## not specify an options file
   try:                                   
      if not ARGUMENTS.has_key("options_file"):
         opts.Save(options_cache, baseEnv)
   except LookupError, le:
      pass

   ## read the builder options after they have been added to the env
   ##base_bldr.readOptions( baseEnv )
   ##base_bldr = base_bldr.clone()
   baseEnv = base_bldr.applyToEnvironment( baseEnv.Copy() )
   ## load environment of the shell that scons is launched from   
   baseEnv.Append( CPPPATH = [pj('#',buildDir)] )
   baseEnv.Append( CPPDEFINES = ['_TAO','VE_PATENTED','_OSG','VTK44'] )
   baseEnv.Append(BUILDERS = builders)
   #setup the build dir
   baseEnv.BuildDir(buildDir, '.', duplicate = 0)
   Export('baseEnv')

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
   Export('PREFIX', 'LIBDIR', 'distDir')##, 'LauncherExe')

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

   # Build up the provides vars for the .fpc files
   provides = "osg"
   arch = GetArch()
   inst_paths = {}
   inst_paths['base'] = os.path.abspath(osg_options.getSettings()[0][1])
   inst_paths['lib'] = pj(inst_paths['base'], baseEnv['libdir'])
   #inst_paths['flagpoll'] = pj( baseEnv['prefix'],baseEnv['libdir'], 'flagpoll')
   inst_paths['flagpoll'] = pj( '#','VE_Installer','fpc')
   inst_paths['bin'] = pj(inst_paths['base'], 'bin')   
   inst_paths['include'] = pj(inst_paths['base'], 'include')   
   OSG_VERSION = ( int('1'), int('2'), int('0') )
   cppdom_version_str = '1.2.0'
   # Build up substitution map
   submap = {
      '@provides@'                  : provides,
      '@prefix@'                    : inst_paths['base'],
      '@exec_prefix@'               : '${prefix}',
      '@cppdom_cxxflags@'           : '',
      '@includedir@'                : inst_paths['include'],
      '@cppdom_extra_cxxflags@'     : '',
      '@cppdom_extra_include_dirs@' : '',
      '@cppdom_libs@'               : '-losg -losgDB -losgGA -losgUtil -lOpenThreads -losgText -losgSim -losgProducer -lProducer',
      '@libdir@'                    : inst_paths['lib'],
      '@lib_subdir@'                : baseEnv['libdir'],
      '@VERSION_MAJOR@'             : str(OSG_VERSION[0]),
      '@VERSION_MINOR@'             : str(OSG_VERSION[1]),
      '@VERSION_PATCH@'             : str(OSG_VERSION[2]),
      '@arch@'                      : arch,
      '@version@'                   : cppdom_version_str,
   }
   ##env.ConfigBuilder('gmtl.pc','gmtl.pc.in',submap = gmtl_pc_submap)
   ##installed_targets += env.Install(pj(PREFIX, 'share', 'pkgconfig'), 'gmtl.pc')
   installed_targets = []
   name_parts = ['osg', cppdom_version_str, arch]
   pc_filename = "-".join(name_parts) + ".fpc"
   tempName = pj(inst_paths['flagpoll'], pc_filename)
   cppdom_pc = baseEnv.ConfigBuilder(tempName, 
                           pj('#','VE_Installer','fpc','osg.fpc.in'), submap = submap)
   if 'install' in COMMAND_LINE_TARGETS:
   	installed_targets = baseEnv.Install( pj( baseEnv['prefix'], baseEnv['libdir'], 'flagpoll'), tempName)
     	baseEnv.Alias('install',[installed_targets,PREFIX])
   #baseEnv.AddPostAction(cppdom_pc, Chmod('$TARGET', 0644))
   #baseEnv.Install(inst_paths['bin'], cppdom_pc)
   
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
   shareSubdirs = pj(buildDir,'share')
   docsSubdirs = pj ('#', 'share', 'docs')

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
   elif 'docs' in COMMAND_LINE_TARGETS:
      ves_dirs = [ docsSubdirs ]
      baseEnv.Alias('docs', docsSubdirs)
   else:
      ves_dirs = [ openSubdirs, builderSubdirs, conductorSubdirs, xplorerSubdirs, ceSubdirs, veiSubdirs, shareSubdirs]

   # Build the test suite if asked.
   if 'testsuite' in COMMAND_LINE_TARGETS:
      ves_dirs.append(pj('#', 'test'))
      baseEnv.Alias('testsuite', pj('#', 'test'))

   ## directory for dzr files
   ves_dirs += [pj( buildDir,'VE_Installer','mk')]
   ## directory for examples
   ves_dirs += [pj( buildDir,'VE_TestSuite')]

   ##Run SConscript files in all of those folders.
   for d in ves_dirs:
      SConscript( dirs = d )

   ##Setup the install flag to install VE-Suite
   if 'install' in COMMAND_LINE_TARGETS:
      ves_pkg.build( install=True )
   else:
      ves_pkg.build( install=False )
      
   ##Compression calls.
   ##NOTE: If VE_Suite isn't the only thing written to PREFIX,
   ## these will compress everything in PREFIX. Could get ugly.
   ##Tar it if set to do so.
   if 'tar' in COMMAND_LINE_TARGETS:
      tarFile = "vesuite.tar.gz"
      baseEnv.Alias('tar', tarFile)
      baseEnv.Append(TARFLAGS = '-c -z')
      baseEnv.Tar(tarFile, PREFIX)
   ##Zip it if set to do so.
   ##NOTE: If zip says it's missing files & crashing,
   ## try deleting .sconsign.dblite from the VE_Suite directory.
   if 'zip' in COMMAND_LINE_TARGETS:
      zipFile = "vesuite.zip"
      baseEnv.Alias('zip', zipFile)
      ##zipped = [pj( PREFIX, 'bin' ),
      ##          pj( PREFIX, 'include' ),
      ##          pj( PREFIX, 'lib' ),
      ##          pj( PREFIX, 'share' )]
      baseEnv.Zip(zipFile, PREFIX)
   
   Default('.')

