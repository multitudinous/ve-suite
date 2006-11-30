#!python
EnsureSConsVersion(0,96)
SConsignFile()

import os, sys, string, smtplib
from subprocess import *
pj = os.path.join

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

GetPlatform = sca_util.GetPlatform
Export('GetPlatform')
GetArch = sca_util.GetArch
Export('GetArch')

##WX_HOME_DIR = '/home/vr/Applications/TSVEG/Libraries/Release/Opt/wxGTK-2.6.2/'

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
buildDir = 'build.' + GetPlatform() + '.' + GetArch()
baseEnv.BuildDir(buildDir, '.', duplicate = 0)

##See scons users guide section 15 on variant builds
##Setup some project defaults
baseEnv.Append( BUILDDIR = buildDir,
                CPPDEFINES = ['_TAO','VE_PATENTED','_OSG','VTK44'] )

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


Export('baseEnv')
Export('Platform')
Export('buildDir')

env = baseEnv.Copy()

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
options_cache = 'options.cache.' + GetPlatform()
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
opts.Add('cluster', 'If true, build the cluster version of VE-Xplorer', 'no')
opts.Add('AprVersion', 'Set the APR version so that the proper apr pkg-config files can be found', '1.0')
##opts.Add('arch', 'CPU architecture (ia32, x86_64, or ppc)',
##         cpu_arch_default)
Export('opts', 'vtk_options', 'osg_options','xerces_options','wxwidgets_options')
  
help_text = """--- VE-Suite Build system ---
Targets:
   To build VE-Suite:
   > scons
   
   To install VE-Suite:
   install - Install VE-Suite
   > scons install prefix=build/test' to install in a test build directory
 
   Make sure that vrjuggler, Boost.Filesystem, ACE/TAO, and apr/apr-util are
   in your FLAGPOLL_PATH or PKGCONFIG_PATH or (DY)LD_LIBRARY_PATH. This is
   necessaary to auto-detect the dependencies.
"""

help_text += """
You can store configuration options in the file: options.custom
This file will be loaded each time.  Note: Options are cached in the file
%s
""" % options_cache

##help_text = help_text + """Options:
##   optimize=yes    Generate optimized code.
##   profile=yes     Turn on generation of profiling code.
##   
##"""

help_text = help_text + opts.GenerateHelpText(baseEnv)

baseEnv.Help(help_text)

if not SConsAddons.Util.hasHelpFlag():
   ##opts.Apply(baseEnv,True)
   opts.Process(baseEnv, None, True)                   # Update the options

   try:                                   # Try to save the options if possible
      opts.Save(options_cache, baseEnv)
   except LookupError, le:
      pass
   
   # Update environment for boost options
   ##if boost_options.isAvailable():
   ##   boost_options.updateEnv(baseEnv)
   # Update environment for vtk options
   ##if vtk_options.isAvailable():
   ##   vtk_options.apply(baseEnv)

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
      VE_CE/UnitWrapper
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

   ves_dirs = [ openSubdirs, builderSubdirs, conductorSubdirs, xplorerSubdirs, ceSubdirs]

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

