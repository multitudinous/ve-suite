
import os, sys, string
pj = os.path.join

import practice

def GetPlatform():
   "Determines what platform this build is being run on."
   if string.find(sys.platform, 'irix') != -1:
      return 'irix'
   elif string.find(sys.platform, 'linux') != -1:
      return 'linux'
   elif string.find(sys.platform, 'freebsd') != -1:
      return 'linux'
   elif string.find(sys.platform, 'cygwin') != -1:
      return 'win32'
   elif string.find(os.name, 'win32') != -1:
      return 'win32'
   elif string.find(sys.platform, 'sun') != -1:
      return 'sun'
   else:
      return sys.platform

Export('GetPlatform')

def BuildLinuxEnvironment():
   "Builds a base envirnoment for other modules to build on set up for Linux"
   global optimize, profile, builders
   profile = 'no'
   optimize = 'no'
   env = Environment(ENV = os.environ,
                     CXX = 'g++',
                     LINK = 'g++')
   CXXFLAGS = ['-g']
   CPPDEFINES = ['HAVE_CONFIG_H', '_LINUX']
   LINKFLAGS = CXXFLAGS
   libpath = ['/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80/lib',
              '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.linux-rh80-deps/lib',
              '/home/users/mccdo/vrjuggler-builds/Suse-9.1-alpha4/lib',
              '/home/users/mccdo/cppdom-0.3.2/Suse-9.1/lib',
              '/home/users/mccdo/vtk-builds/Linux/lib/vtk']
   env.Append(CXXFLAGS = CXXFLAGS,
              LINKFLAGS = LINKFLAGS,
              CPPDEFINES = CPPDEFINES)
   #SetupVRJuggler(env)
   #SetupVTK(env)
   #SetupOmniORB(env)
   
#   CXXFLAGS.append('-J6')
#   CXXFLAGS.append('-all')
#   CXXFLAGS.append('-w2')
#   CXXFLAGS.append('-B dymanic')

   # Reinitialize to avoid duplication
   CXXFLAGS = []
   CPPDEFINES = CPPDEFINES
   LINKFLAGS = []
   LIBPATH = []
   LIBS = ['m', 'vrj', 'sonix', 'gadget', 'jccl', 'vpr',
             'cppdom', 'boost_filesystem-gcc-mt-d','boost_filesystem-gcc-mt-1_31','pthread', 'vtkImaging',
             'vtkGraphics', 'vtkCommon', 'vtkHybrid', 'vtkIO', 'vtkFiltering',
             'vtkRendering', 'vtkParallel']
   # Enable profiling?
   if profile != 'no':
      CXXFLAGS.extend([])
      LINKFLAGS.extend([])

   # Debug or optimize build?
   if optimize != 'no':
      CPPDEFINES.append('NDEBUG')
      CXXFLAGS.extend(['-O2'])
   else:
      CPPDEFINES.append('_DEBUG')
      CXXFLAGS.extend(['-g'])
   # IRIX sucks; no environment variable to specify additional header paths
   CPPPATH = []
   os.environ['NEW_ENVIRON'] = '/home/users/mccdo/svn_VE_Suite/VE_Suite'
   newenviron = os.environ['NEW_ENVIRON']
   CPPPATH.append(newenviron)
   os.environ['VTK_BASE_DIR'] = '/home/users/mccdo/vtk-builds/Linux'
   vtkbasedir = os.environ['VTK_BASE_DIR']
   CPPPATH.append(os.path.join(vtkbasedir, 'include', 'vtk'))
   os.environ['VJ_BASE_DIR'] = '/home/users/mccdo/vrjuggler-builds/Suse-9.1-alpha4'
   vjbasedir = os.environ['VJ_BASE_DIR']
   CPPPATH.append(os.path.join(vjbasedir, 'include'))
   os.environ['VJ_DEPS_DIR'] = '/home/users/mccdo/cppdom-0.3.2/Suse-9.1'
   vjdepsdir2 = os.environ['VJ_DEPS_DIR']
   CPPPATH.append(os.path.join(vjdepsdir2, 'include'))
   
   #if os.environ.has_key('CPLUS_INCLUDE_PATH'):
   #   path = os.environ['CPLUS_INCLUDE_PATH']
   #   CPPPATH = string.split(path, ':')
   vjdepsdir1 = os.environ['VJ_DEPS_DIR']
   # This is required so you can build std C++ headers with MipsPro
   CPPPATH.append(os.path.join(vjdepsdir1, 'include', 'boost', 'compatibility', 'cpp_c_headers'))
   CPPPATH.append(os.path.join(newenviron, 'VE_Xplorer'))
   CPPPATH.append(os.path.join(newenviron, 'VE_Builder', 'Utilities'))
   env.Append(CXXFLAGS    = CXXFLAGS,
              CPPDEFINES = CPPDEFINES,
              LINKFLAGS   = LINKFLAGS,
              CPPPATH = CPPPATH,
              LIBPATH = libpath,
              LIBS = LIBS)
   return env

def BuildIRIXEnvironment():
   "Builds a base environment for other modules to build on set up for IRIX"
   global optimize, profile, builders
   profile = 'no'
   optimize = 'no'
   env = Environment(ENV = os.environ,
                     CXX = 'CC',
                     LINK = 'CC')
   CXXFLAGS = ['-n32', '-mips4', '-LANG:std']
   CPPDEFINES = ['HAVE_CONFIG_H', '_IRIX']
   LINKFLAGS = CXXFLAGS
   libpath = ['/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-pthread/lib32',
              '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps/lib32',
              '/home/users/mccdo/vtk-builds/IRIX32/lib/vtk']
   env.Append(CXXFLAGS = CXXFLAGS,
              LINKFLAGS = LINKFLAGS,
              CPPDEFINES = CPPDEFINES)
   #SetupVRJuggler(env)
   #SetupVTK(env)
   #SetupOmniORB(env)
   
   CXXFLAGS.append('-J6')
   CXXFLAGS.append('-all')
   CXXFLAGS.append('-w2')
   CXXFLAGS.append('-B dymanic')

   # Reinitialize to avoid duplication
   CXXFLAGS = []
   CPPDEFINES = CPPDEFINES
   LINKFLAGS = []
   LIBPATH = []
   LIBS = ['m', 'vrj', 'sonix', 'gadget', 'jccl', 'vpr', 'X11',
             'cppdom', 'boost_filesystem-mp-mt', 'pthread', 'vtkImaging',
             'vtkGraphics', 'vtkCommon', 'vtkHybrid', 'vtkIO', 'vtkFiltering',
             'vtkRendering', 'vtkParallel']
   # Enable profiling?
   if profile != 'no':
      CXXFLAGS.extend([])
      LINKFLAGS.extend([])

   # Debug or optimize build?
   if optimize != 'no':
      CPPDEFINES.append('NDEBUG')
      CXXFLAGS.extend(['-O2'])
   else:
      CPPDEFINES.append('_DEBUG')
      CXXFLAGS.extend(['-g', '-gslim'])
   # IRIX sucks; no environment variable to specify additional header paths
   CPPPATH = []
   os.environ['NEW_ENVIRON'] = '/home/users/mccdo/svn_VE_Suite/VE_Suite'
   newenviron = os.environ['NEW_ENVIRON']
   CPPPATH.append(newenviron)
   vtkbasedir = os.environ['VTK_BASE_DIR']
   CPPPATH.append(os.path.join(vtkbasedir, 'include', 'vtk'))
   vjbasedir = os.environ['VJ_BASE_DIR']
   CPPPATH.append(os.path.join(vjbasedir, 'include'))
   vjdepsdir2 = os.environ['VJ_DEPS_DIR']
   CPPPATH.append(os.path.join(vjdepsdir2, 'include'))
   
   #if os.environ.has_key('CPLUS_INCLUDE_PATH'):
   #   path = os.environ['CPLUS_INCLUDE_PATH']
   #   CPPPATH = string.split(path, ':')
   vjdepsdir1 = os.environ['VJ_DEPS_DIR']
   # This is required so you can build std C++ headers with MipsPro
   CPPPATH.append(os.path.join(vjdepsdir1, 'include', 'boost', 'compatibility', 'cpp_c_headers'))
   CPPPATH.append(os.path.join(newenviron, 'VE_Xplorer'))
   CPPPATH.append(os.path.join(newenviron, 'VE_Builder', 'Utilities'))
   env.Append(CXXFLAGS    = CXXFLAGS,
              CPPDEFINES = CPPDEFINES,
              LINKFLAGS   = LINKFLAGS,
              CPPPATH = CPPPATH,
              LIBPATH = libpath,
              LIBS = LIBS)
   return env


def BuildBaseEnvironment():
   "Builds a base environment for other modules to build on."
   
   if GetPlatform() == 'linux':
      return BuildLinuxEnvironment()
   elif GetPlatform() == 'irix':
      return BuildIRIXEnvironment()
   else:
      print '[ERR] Unsupported Platform ' + GetPlatform()
      sys.exit(1)



baseEnv = BuildBaseEnvironment()
Export('baseEnv')
Platform = GetPlatform()
Export('Platform')
buildDir = 'build.' + GetPlatform()
BuildDir(buildDir, '.', duplicate = 0)

subdirs = Split("""
   Preprocessor
   Translator 
   Utilities
""")

subdirs = map(lambda s: pj(buildDir, 'VE_Builder', s), subdirs)

SConscript(dirs = subdirs)

#for d in subdirs:
#    SConscript(os.path.join( d, 'SConscript'))
#   SConscriptChdir(0)
#   SConscript(pj( d, 'SConscript'))
Default('.')
