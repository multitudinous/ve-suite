import os, sys, string, smtplib
pj = os.path.join

print "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n---IT BEGINS.---" ##TESTER

import practice
import vtk2 ##Only used in IRIX environment.

# set this to true to enable build emailing
email = 'false'

# fill this in with interested parties
peopleToEmail = ['mccdo@iastate.edu','jaredabo@gmail.com','biv83@yahoo.com','biv@iastate.edu']

# replace with your local outgoing SMTP server
smtpServer = 'mailhub.iastate.edu'


SetOption('implicit_cache', 1)

# this is who the build email will come from
fromAddress = 'mikelem@iastate.edu'


def EmailResults() :
#	file = open('BuildLog.txt', 'r')
#	buffer = MIMEText(file.read())
#	buffer['Subject'] = 'Editor Build Log'
#	file.close()

	s = smtplib.SMTP()
	s.connect(smtpServer) 
	s.sendmail(fromAddress, peopleToEmail, 'Guess what I did, Gerrick.  Thats right, I just built SCons, AGAIN.  HA HA!! ')
	s.close()



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
    "Builds a base environment for other modules to build on set up for Linux"
    global optimize, profile, builders
    profile = 'no'
    optimize = 'no'
    env = Environment(ENV = os.environ,
                            CXX = 'g++',
                            LINK = 'g++')

    CXXFLAGS = ['-g']
    CPPDEFINES = ['HAVE_CONFIG_H', '_LINUX', 'VTK_STREAMS_FWD_ONLY', '_TAO', '_REENTRANT', 'ACE_HAS_AIO_CALLS', '_GNU_SOURCE', 'ACE_HAS_EXCEPTIONS', '__ACE_INLINE__', '_OSG', 'VTK44', 'WXUSINGDLL', '__WXGTK__']



    LINKFLAGS = CXXFLAGS
    libpath = ['/home/vr/Applications/TSVEG/Libraries/Release/Debug/VTK/Linux-RHEL_4/lib/vtk',
                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/xercesc/Linux-RHEL_4/lib',
                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG-1.0/Linux-RHEL_4/lib',
                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE_TAO/Linux-RHEL_4/lib']

#                  '/home/users/mccdo/wxWindows/wxGTK-2.6.2/install-rhel_4/lib'
#                  '/usr/X11R6/lib'
#                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/corona-1.0.2/Linux-SuSE92/bin'
#'/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1/vrjuggler-2.0.1-linux-rhel4-i686/lib',
#                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1/vrjuggler-2.0.1-linux-rhel4-i686/bin',
#                  '/home/vr/Applications/TSVEG/Libraries/Release/Opt/vrjuggler-2.0.1-deps/vrjuggler-2.0.1-linux-rhel4-i686-deps/lib'

#    env.Append(CXXFLAGS = CXXFLAGS,
#                  LINKFLAGS = LINKFLAGS,
#                  CPPDEFINES = CPPDEFINES)
    #SetupVRJuggler(env)
    #SetupVTK(env)
    #SetupOmniORB(env)
    
    #CXXFLAGS.append('-J6')
    #CXXFLAGS.append('-all')
    #CXXFLAGS.append('-w2')
    #CXXFLAGS.append('-B dymanic')
    #CXXFLAGS.append('pthread')                        

    # Reinitialize to avoid duplication
#    CXXFLAGS = []
#    CPPDEFINES = CPPDEFINES
#    LINKFLAGS = []
#    LIBPATH = []
    LIBS = []

##Global variables exported to other SConscripts.
#'dl',
    xerces = ['xerces-c']
    vtk = ['vtkImaging', 'vtkGraphics', 'vtkCommon', 'vtkHybrid', 'vtkIO', 'vtkFiltering',
           'vtkRendering', 'vtkParallel']
    osg = ['osg', 'osgDB', 'osgGA', 'osgUtil', 'OpenThreads'] ##'osgFX'
    ace_tao = ['TAO_IORInterceptor', 'TAO_ObjRefTemplate', 'TAO_Valuetype', 'TAO', 'ACE',
               'pthread', 'TAO_CosNaming', 'TAO_Svc_Utils', 'TAO_IORTable', 'TAO_Messaging',
               'TAO_PortableServer', 'TAO_BiDirGIOP']
    boost = ['boost_filesystem-gcc-mt-d', 'boost_filesystem-gcc-mt-1_33']
#    vrjuggler = [ 'jccl', 'vrj', 'vrj_ogl','sonix', 'gadget', 'vpr', 'SM', 'ICE', 'X11', 'm', 'cppdom', 'uuid' ]

    Export ('xerces', 'vtk', 'osg', 'ace_tao', 'boost')


    # Enable profiling?
    if profile != 'no':
        CXXFLAGS.extend([])    ##Does nothing.
        LINKFLAGS.extend([])  ##Ditto.

    # Debug or optimize build?
    if optimize != 'no':
        CPPDEFINES.append('NDEBUG')
        CXXFLAGS.extend(['-O2'])
    else:
#        CPPDEFINES.append('_DEBUG')
        CXXFLAGS.extend(['-g'])


    CPPPATH = []
    ##CPPPATH = [pj(os.getenv("VJ_BASE_DIR"), 'bin')]
    ##print "CPPPATH: " + str(CPPPATH)
#    if os.environ[SCENE_GRAPH] == OSG:
#        CPPDEFINES = _OSG
#    else:
#        CPPDEFINES = _PERFORMER
    



    #if os.environ.has_key('CPLUS_INCLUDE_PATH'):
    #    path = os.environ['CPLUS_INCLUDE_PATH']
    #    CPPPATH = string.split(path, ':')
    vjdepsdir1 = os.environ['VJ_DEPS_DIR']

    env.Append(CXXFLAGS = CXXFLAGS,
                  CPPDEFINES = CPPDEFINES,
                  LINKFLAGS = LINKFLAGS,
                  CPPPATH = CPPPATH,
                  LIBPATH = libpath,
                  LIBS = LIBS)
    return env

def BuildIRIXEnvironment():
    "Builds a base environment for other modules to build on set up for IRIX"
    env = Environment(ENV = os.environ,
                        CPPPATH = [],
                        LIBPATH = [],
                        LIBS = [])
    cc = Configure(env)
    o = Options(['unix.cache', 'vtk2.py'])
    vtk2.Options(o)
    Help(o.GenerateHelpText(env))
    o.Update(env)
    vtk2.Update(env)
    vtk2.Config(cc, env)
    o.Update(env)
    o.Save('unix.cache',env)
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
##BuildDir(buildDir, '.', duplicate = 1)
BuildDir(buildDir, '.', duplicate = 0)

env = baseEnv.Copy()

openSubdirs = Split("""
    VE_Open
    VE_Open/XML
    VE_Open/XML/Shader
    VE_Open/XML/CAD
    VE_Open/XML/Model
    VE_Open/skel
""")

conductorSubdirs = Split("""
    Network
    GUIPlugin
    Utilities
    Framework
""")

ceSubdirs = Split("""
    VE_CE/Utilities
    VE_CE
""")

xplorerSubdirs = Split("""
    VE_Xplorer/VE_Util  
    VE_SceneGraph
    VE_SceneGraph/Utilities
    VE_TextureBased
    VE_Xplorer/VE_XplorerHandlers
""")

builderSubdirs = Split("""
    Translator
    Preprocessor
    vtkTo3DTexture/tcGUI
    Utilities
""") 

##Tack on path prefixes to subdirs specified above.
builderSubdirs = map(lambda s: pj(buildDir, 'VE_Builder', s), builderSubdirs)
openSubdirs = map(lambda s: pj(buildDir, s), openSubdirs)
conductorSubdirs = map(lambda s: pj(buildDir, 'VE_Conductor', s), conductorSubdirs)
xplorerSubdirs = map(lambda s: pj(buildDir, s), xplorerSubdirs)
ceSubdirs = map(lambda s: pj(buildDir, s), ceSubdirs)

##Run SConscript files in all of those folders.
##REDUNDANT: SConscript(['#/VE_Conductor/Network/SConscript'])
SConscript(['#/VE_Xplorer/VE_GraphicalPlugin/SConscript'])
SConscript(['#/VE_Xplorer/VE_GE/SConscript'])
SConscript(dirs = openSubdirs)
SConscript(dirs = ceSubdirs)
SConscript(dirs = conductorSubdirs)
SConscript(dirs = xplorerSubdirs)
SConscript(dirs = builderSubdirs)

SConscript(['#/VE_Conductor/DefaultPlugin/SConscript'])
##SConscript(['#/VE_Switcher/SConscript'])

#SConscript(['#/VE_CE/SConscript'])

#vesuiteSubdirs = Split("""
#    VE_Open
#    VE_Open/VE_XML
#    VE_Open/VE_XML/CAD
#    VE_Conductor/Network
#    VE_CE
#    VE_Conductor
#    VE_Xplorer
#    VE_Builder/Translator
#    VE_Builder/Preprocessor
#    VE_Builder/vtk3DTexture/tcGUI
#    VE_Builder/Utilities
#""")
#vesuiteSubdirs = map(lambda s: pj(buildDir, s), vesuiteSubdirs)
#SConscript(dirs = vesuiteSubdirs)

#EmailResults()

Default('.')
