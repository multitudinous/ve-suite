# Uses the SCons Configure contexts to check for the existance of boost
# on the host platform

import os
from SCons.Options import *
from SCons import SConf
from SCons import Conftest
pj = os.path.join

def Options(options):
   """
   Adds the options for boost to the SCons Options object
   Options defined:
      boost_root                 - The prefix to get to the boost install
      boost_includes             - The directory that contains the boost header
                                   files.
   """
   # Save me typing; o = options;
   o = options
   # SCons Options are formatted in the following manner:
   # option_object = OptionType(key, help_text, default_value)
   
   # Boost Root
   vtk_base_dir = PathOption('VTK_BASE_DIR', 
                           'VTK_BASE_DIR installation default=/usr/local',
                           '/usr/local')
   if arguments.get('vtk_base_dir',0) != '':
      vtk_base_dir=arguments.get('vtk_base_dir',0)
         
   # Boost include directory
   o.Add('boost_includes', 
   'Boost header file directory\t default=/usr/local/include/boost-1_31',
   path.join('vtk_base_dir','include','vtk'))
   o.AddOptions(vtk_base_dir)

def Config(configContext, env):
   """
   Uses the given ConfigContext to test for Boost

   configContext -- the config context to use
   env           -- the SCons environment we are modifying
   """
   # Save me typing; note the following
   # cc is now the ConfigContext
   # ccm is a shortcut for ConfigContext.Message; this displays a message
   #     to the user
   # ccr is a shortcut for ConfigContext.Result
   #     this displays a message to the user concerning the result of a 
   #     test
   cc = configContext
   check_context = SConf.CheckContext(cc)
   ccm = check_context.Message
   ccr = check_context.Result
   ccm('Performing Boost tests.\n')

   # Check for boost/version.hpp
   ccm('Checking for boost/version.hpp\n')
   res = cc.CheckCXXHeader( pj('boost', 'version.hpp') ) 
   if res == 0:
      ccm('[ERR] boost/version.hpp not found.')
      return 0
   
   # Check to see what the version of boost is
   ccm('Determining the version of boost\n')
   
   # Define a simple program that will NOT compile if boost version is
   # less than 103100
   version_check = """
   #include "boost/version.hpp"

   #define BOOST_MAJOR BOOST_VERSION / 100000
   #define BOOST_MINOR BOOST_VERSION / 100 % 1000
   #define BOOST_SUB_MINOR BOOST_VERSION % 100
   
   int main(int argc, char* argv)
   {
      #if BOOST_MAJOR < 1 || BOOST_MINOR < 31
         error_boost_version_is_to_low;
      #endif
      return 0;  
   }
   """
   res = cc.TryCompile(version_check, '.cpp')
   if res == 0:
      ccm("[ERR] The version of Boost found is too low.  VR Juggler currently requires Boost 1.31.0 or higher.\n")
      return 0
   # Figure out if we're on a lame compiler that doesn't support std:: headers
   ccm("Checking for std C++ headers...\n")
   res = cc.CheckCXXHeader('cstdlib', '<>')
   if res == 0:
      ccm("Checking to see if boost can make up for the lack of headers...\n")
      res = cc.CheckCXXHeader(pj('boost', 'compatibility', 'cpp_c_headers'), 
                              '<>')
      if res == 0:
         ccm("[ERR] This compiler doesn't support modern C++ header files <cstdlib>, <cstring>, et. al.  VR Juggler requires a modern C++ compiler to build.\n")
         return 0
   return 1

def Update(env):
   """
   Updates an environment's paths with the boost options that have
   been specified in the environment.
   """
   env.AppendUnique(LIBPATH = [], CPPPATH = [])
   dict = env.Dictionary()
   CPPPATH = []
   LIBPATH = []
   # Check to see what the values of the boost options are.

   boost_root = dict['boost_root']
   boost_includes = dict['boost_includes']
   if boost_includes != '':
      if boost_includes not in dict['CPPPATH']:
         CPPPATH.append(boost_includes)

   libdir = pj(boost_root, 'lib')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
   env.Append(CPPPATH = CPPPATH,
              LIBPATH = LIBPATH)
