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
   vtk_base_dir = PathOption('vtk_base_dir', 
                             'VTK_BASE_DIR installation default=/usr/local',
                             '/home/vr/Applications/TSVEG/Libraries/VTK4.4/IRIX32')
   
   jdk_home_dir = PathOption('jdk_home_dir',
                         'JDK_HOME installation default=/usr/local',
                         '/usr/java2')

   vj_base_dir = PathOption('vj_base_dir',
                            'VJ_BASE installation default=/usr/local',
                            '/home/vr/Applications/TSVEG/Libraries/vrjuggler-2.0a4/IRIX32-pthread')  
                            
   vj_deps_dir = PathOption('vj_deps_dir',
                            'VJ_DEPS_DIR installation default=/usr/local',
                            '/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps')
                             
   wx_home = PathOption('wx_home', 
                        'WX_HOME installation default=/usr/local',
                        '/home/vr/Applications/TSVEG/Libraries/wxGTK-2.4.2/IRIX32')    
                        
   omni_home = PathOption('omni_home',
                          'OMNI_HOME installation default=/usr/local',
                          '/home/vr/Applications/TSVEG/Libraries/omniORB-4.0.3/IRIX32') 
   
   xercescroot = PathOption('xercescroot',
                          'xercescroot installation default=/usr/local',
                          '/home/vr/Applications/TSVEG/Libraries/xerces-2.5/IRIX32') 
                          
   pythonpath = PathOption('pythonpath',
                          'xercescroot installation default=/usr/local',
                          '/home/users/nelsonn/svn_Test/VE_Suite/test/junk') 
                                        
   add = PathOption('add',
                    'add installation default=/usr/local',
                    '/usr/')
                                   
   suite = PathOption('suite',
                    'suite installation default=/usr/local',
                    '/home/users/nelsonn/svn_Test/VE_Suite/')

   o.AddOptions(vtk_base_dir, vj_base_dir, vj_deps_dir, jdk_home_dir, add, suite,
                wx_home, omni_home, xercescroot, pythonpath)                       

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
   dict = env.Dictionary()
   cc = configContext
   check_context = SConf.CheckContext(cc)
   ccm = check_context.Message
   ccr = check_context.Result
   #ccm('Performing VTK_BASE_DIR tests.\n')

   res = cc.CheckCXXHeader('vtkIOStream.h')
#   res1 = cc.CheckCXXHeader('jvmpi.h')
   res2 = cc.CheckCXXHeader('vpr/vprConfig.h')
   res3 = cc.CheckCXXHeader('omniconfig.h')
   res4 = cc.CheckCXXHeader('wx/wx.h')
   res5 = cc.CheckCXXHeader('omnithread.h')
   res6 = cc.CheckCXXHeader('xercesc/util/XMemory.hpp')

   if res == 0:
      ccm('[ERR] vtk_base_dir could not be found.\n')
      ccm('vtk_base_dir=/home/vr/Applications/TSVEG/Libraries/VTK4.4/IRIX32\n')

#   if res1 == 0:
#      ccm('[ERR] jdk_home_dir could not be found.\n')
#      ccm('jdk_home_dir=/usr/java2\n')

   if res2 == 0:
      ccm('[ERR] vj_base_dir could not be found.\n')
      ccm('vj_base_dir=/home/vr/Applications/TSVEG/Libraries/vrjuggler-2.0a4/IRIX32-pthread\n')

   if res3 == 0:
      ccm('[ERR] vj_deps_dir could not be found.\n')
      ccm('vj_deps_dir=/home/vr/Juggler/2.0/vrjuggler-2.0-alpha4.irix-n32-deps\n')
   
   if res4 == 0:
      ccm('[ERR] wx_home could not be found.\n')
      ccm('wx_home=/home/vr/Applications/TSVEG/Libraries/wxGTK-2.4.2/IRIX32\n')
   
   if res5 == 0:
      ccm('[ERR] omni_home could not be found.\n')
      ccm('omni_home=/home/vr/Applications/TSVEG/Libraries/omniORB-4.0.3/IRIX32\n')
   
   if res6 == 0:
      ccm('[ERR] xercescroot could not be found.\n')
      ccm('xercescroot=/home/vr/Applications/TSVEG/Libraries/xerces-2.5/IRIX32\n')
      
   pythonpath = pj( dict['omni_home'], 'lib/python2.4/site-packages')
     
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
   
   vtk_base_dir = dict['vtk_base_dir']
   vtk_includes_dir = pj( dict['vtk_base_dir'],'include','vtk' )
   if vtk_includes_dir != '':
      if vtk_includes_dir not in dict['CPPPATH']:
         CPPPATH.append(vtk_includes_dir)
   
   libdir = pj(vtk_base_dir, 'lib', 'vtk')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
   
   jdk_home_dir = dict['jdk_home_dir']
   jdk_includes = pj( dict['jdk_home_dir'],'include' )
   if jdk_includes != '':
      if jdk_includes not in dict['CPPPATH']:
         CPPPATH.append(jdk_includes)

   libdir = pj(jdk_home_dir, 'lib')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
             
   vj_base_dir = dict['vj_base_dir']
   vj_includes_dir = pj( dict['vj_base_dir'],'include')
   vj_boost_dir = pj( dict['vj_base_dir'],'include','boost','compatibility','ccp_c_headers')
   if vj_includes_dir != '':
      if vj_includes_dir not in dict['CPPPATH']:
         CPPPATH.append(vj_includes_dir)
   if vj_boost_dir != '':
      if vj_boost_dir not in dict['CPPPATH']:
         CPPPATH.append(vj_boost_dir)

   libdir = pj(vj_base_dir, 'lib32')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)

   vj_deps_dir = dict['vj_deps_dir']
   vj_deps_includes_dir = pj( dict['vj_deps_dir'],'include' )
   vj_deps_boost_dir = pj( dict['vj_deps_dir'],'include', 'boost', 'compatibility', 'cpp_c_headers' )
   if vj_deps_includes_dir != '':
      if vj_deps_includes_dir not in dict['CPPPATH']:
         CPPPATH.append(vj_deps_includes_dir)
   if vj_deps_boost_dir != '':
      if vj_deps_boost_dir not in dict['CPPPATH']:
         CPPPATH.append(vj_deps_boost_dir)

   libdir = pj(vj_deps_dir, 'lib32')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
   
   omni_home = dict['omni_home']
   omni_includes = pj( dict['omni_home'],'include' )
   if omni_includes != '':
      if omni_includes not in dict['CPPPATH']:
         CPPPATH.append(omni_includes)

   libdir = pj(omni_home, 'lib')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
              
   xercescroot = dict['xercescroot']
   xercesc_includes = pj( dict['xercescroot'],'include' )
   if xercesc_includes != '':
      if xercesc_includes not in dict['CPPPATH']:
         CPPPATH.append(xercesc_includes)

   libdir = pj(xercescroot, 'lib')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
              
   pythonpath = dict['pythonpath']
   python_includes = pj( dict['pythonpath'],'include' )
   if python_includes != '':
      if python_includes not in dict['CPPPATH']:
         CPPPATH.append(python_includes)

   libdir = pj(pythonpath, 'lib')
   if libdir not in dict['LIBPATH']:
      LIBPATH.append(libdir)
      
   add = dict['add']
   add_includes = pj( dict['add'],'include','CC' )
   if add_includes != '':
      if add_includes not in dict['CPPPATH']:
         CPPPATH.append(add_includes)

#   libdir = pj(add, 'lib32')
#   if libdir not in dict['LIBPATH']:
#      LIBPATH.append(libdir)
      
   suite = dict['suite']
   suite_pre = pj( dict['suite'],'VE_Builder','Preprocessor' )
   suite_trans = pj( dict['suite'],'VE_Builder','Translator' )
   suite_util = pj( dict['suite'],'VE_Builder','Utilities' )
   suite_xpl = pj( dict['suite'],'VE_Xplorer')
   if suite_pre != '':
      if suite_pre not in dict['CPPPATH']:
         CPPPATH.append(suite_pre)
   if suite_trans != '':
      if suite_trans not in dict['CPPPATH']:
         CPPPATH.append(suite_trans)
   if suite_util != '':
      if suite_util not in dict['CPPPATH']:
         CPPPATH.append(suite_util)
   if suite_xpl != '':
      if suite_xpl not in dict['CPPPATH']:
         CPPPATH.append(suite_xpl)
         
   LIBS = ['m', 'vrj', 'sonix', 'gadget', 'jccl', 'vpr', 'X11',
             'cppdom', 'boost_filesystem-mp-mt', 'pthread', 'vtkImaging',
             'vtkGraphics', 'vtkCommon', 'vtkHybrid', 'vtkIO', 'vtkFiltering',
             'vtkRendering', 'vtkParallel']
             
   #CXX = 'CC'
   #LINK = CXX
   CXXFLAGS = ['-n32', '-mips3', '-LANG:std']
   wx = pj(dict['wx_home'], 'bin', 'wx-config --libs --cxxflags')
   env.ParseConfig( wx )
   LINKFLAGS = CXXFLAGS
   #juggler = pj(dict['vj_base_dir'], 'bin', 'vrjuggler-config --cxxflags N32 --libs N32')
   #env.ParseConfig( juggler )
   #--libs N32 --extra-libs N32 
   cppdom = pj(dict['vj_base_dir'], 'bin', 'tweek-config --cxxflags N32')
   env.ParseConfig( cppdom )
   
   env.Append(CXXFLAGS = CXXFLAGS,
              CPPPATH = CPPPATH,
              LINKFLAGS = LINKFLAGS,
              LIBPATH = LIBPATH,
              LIBS = LIBS)
   
