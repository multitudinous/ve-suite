import os, sys, string, smtplib
import practice
##import vtk2 ##Only used in IRIX environment. OBSOLETE.

pj = os.path.join

##NOTE: Put in path to flagpoll & flagpoll files.

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
	s = smtplib.SMTP()
	s.connect(smtpServer) 
	s.sendmail(fromAddress, peopleToEmail, 'Guess what I did, Gerrick.  Thats right, I just built SCons, AGAIN.  HA HA!! ')
	s.close()

##Placeholder for cfdHostType function.
cfdHostType = "RHEL_4"
libPath = pj('#', 'lib', cfdHostType)
Export('cfdHostType')
Export('libPath')

def GetPlatform():
    """Determines what platform this build is being run on."""
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
    """Builds a base environment for other modules to build on set up for Linux"""
    global optimize, profile, builders
    profile = 'no'
    optimize = 'no'
    env = Environment(ENV = os.environ,
                            CXX = 'g++',
                            LINK = 'g++')

    CXXFLAGS = ['-g']
    CPPDEFINES = ['HAVE_CONFIG_H', '_LINUX', 'VTK_STREAMS_FWD_ONLY', '_TAO',
                  '_REENTRANT', 'ACE_HAS_AIO_CALLS', '_GNU_SOURCE',
                  'ACE_HAS_EXCEPTIONS', '__ACE_INLINE__', '_OSG', 'VTK44',
                  'WXUSINGDLL', '__WXGTK__']



    LINKFLAGS = CXXFLAGS
    libpath = ['/home/vr/Applications/TSVEG/Libraries/Release/Debug/VTK/Linux-RHEL_4/lib/vtk-5.0',
               '/home/vr/Applications/TSVEG/Libraries/Release/Opt/xercesc/Linux-RHEL_4/lib',
               '/home/vr/Applications/TSVEG/Libraries/Release/Opt/OSG-1.0/Linux-RHEL_4/lib',
               '/home/vr/Applications/TSVEG/Libraries/Release/Opt/ACE-TAO-5.5/Linux-RHEL_4/lib']
    LIBS = []

##Global variables exported to other SConscripts.
    xerces = ['xerces-c']
    vtk = ['vtkImaging', 'vtkGraphics', 'vtkCommon', 'vtkHybrid', 'vtkIO', 'vtkFiltering',
           'vtkRendering', 'vtkParallel', 'vtkexpat', 'vtkpng', 'vtktiff', 'vtksys', 'vtkjpeg',
           'vtkexoIIc', 'vtkftgl', 'vtkfreetype', 'vtkDICOMParser', 'vtkzlib', 'vtkMPEG2Encode',
           'vtkNetCDF']
    osg = ['osg', 'osgDB', 'osgGA', 'osgUtil', 'OpenThreads']
    ace_tao = ['TAO_IORInterceptor', 'TAO_ObjRefTemplate', 'TAO_Valuetype', 'TAO', 'ACE',
               'pthread', 'TAO_CosNaming', 'TAO_Svc_Utils', 'TAO_IORTable', 'TAO_Messaging',
               'TAO_PortableServer', 'TAO_BiDirGIOP', 'TAO_AnyTypeCode']
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
    if os.getenv('VE_PATENTED') == 'TRUE':
        env.Append(CXXFLAGS = '-DVE_PATENTED')
    return env

##OBSOLETE
##def BuildIRIXEnvironment():
##    """Builds a base environment for other modules to build on set up for IRIX"""
##    env = Environment(ENV = os.environ,
##                        CPPPATH = [],
##                        LIBPATH = [],
##                        LIBS = [])
##    cc = Configure(env)
##    o = Options(['unix.cache', 'vtk2.py'])
##    vtk2.Options(o)
##    Help(o.GenerateHelpText(env))
##    o.Update(env)
##    vtk2.Update(env)
##    vtk2.Config(cc, env)
##    o.Update(env)
##    o.Save('unix.cache', env)
##    return env

def BuildBaseEnvironment():
    "Builds a base environment for other modules to build on."
    if GetPlatform() == 'linux':
        return BuildLinuxEnvironment()
##    elif GetPlatform() == 'irix':
##        return BuildIRIXEnvironment()
    else:
        print '[ERR] Unsupported Platform ' + GetPlatform()
        sys.exit(1)

baseEnv = BuildBaseEnvironment()
Platform = GetPlatform()
Export('baseEnv')
Export('Platform')
buildDir = 'build.' + GetPlatform()
BuildDir(buildDir, '.', duplicate = 0)
##BuildDir(buildDir, '.', duplicate = 1)

env = baseEnv.Copy()
if os.getenv('SCENE_GRAPH') == 'OSG':
    osg_tag = "_osg"
else:
    osg_tag = "_pf"
if os.getenv('VE_PATENTED') == 'TRUE':
    patented_tag = "_vep"
else:
    patented_tag = ''
if os.getenv('TAO_BUILD') == 'TRUE':
    exec_tag = '_tao'
else:
    exec_tag = ''
Export('osg_tag')
Export('patented_tag')
Export('exec_tag')

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
    DefaultPlugin
""")

ceSubdirs = Split("""
    VE_CE/Utilities
    VE_CE
""")

xplorerSubdirs = Split("""
    Utilities  
    SceneGraph
    SceneGraph/Utilities
    TextureBased
    XplorerHandlers
    GraphicalPlugin
""")##    XplorerNetwork
##    GE
##    DefaultGraphicalPlugin
##""")

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
##REDUNDANT: SConscript(['#/VE_Conductor/Network/SConscript'])
SConscript(dirs = openSubdirs)
SConscript(dirs = ceSubdirs)
SConscript(dirs = conductorSubdirs)
SConscript(dirs = xplorerSubdirs)
##SConscript(dirs = builderSubdirs)

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
