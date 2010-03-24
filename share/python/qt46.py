
"""

Tool-specific initialization for Qt.

"""
import os.path
import re
import os, sys, string
pj = os.path.join
import SConsAddons.Util as sca_util
from SCons.Script import *  # the usual scons stuff you get in a SConscript

def qtTargetBuilder( target, source, env ):
    #print target.path
    #print source.abspath
    targets = []
    #print os.path.dirname( source )
    #print os.path.basename( source )
    #print os.path.split( source )
    # need to ensure sources is a list or is made into a list
    for targets in target:
        print targets
    for sources in source: 
        print sources
    #    basename = os.path.splitext(os.path.basename(sources))[0]
    #    basedir = os.path.dirname( sources )
        #targets.append( pj( basedir, prefix + basename + suffix ) )
    return (target + targets, source )
    #return targets

def generate(env,**kw):
    if sca_util.GetPlatform() == 'win32':
        return None
    # Find qt tools exectuable
    sys.stdout.write("Searching for uic...\n")
    taoidl_cmd = WhereIs('uic')

    if None == taoidl_cmd:
        sys.stdout.write("Could not find uic. Please make sure uic is in your PATH.\n")
        return None
    else:
        sys.stdout.write("Found uic %s\n" % taoidl_cmd )

    # Find tao_idl exectuable
    sys.stdout.write("Searching for moc...\n")
    taoidl_cmd = WhereIs('moc')

    if None == taoidl_cmd:
        sys.stdout.write("Could not find moc. Please make sure moc is in your PATH.\n")
        return None
    else:
        sys.stdout.write("Found moc %s\n" % taoidl_cmd )

    # Find tao_idl exectuable
    sys.stdout.write("Searching for rcc...\n")
    taoidl_cmd = WhereIs('rcc')

    if None == taoidl_cmd:
        sys.stdout.write("Could not find rcc. Please make sure rcc is in your PATH.\n")
        return None
    else:
        sys.stdout.write("Found rcc %s\n" % taoidl_cmd )

    uic ='uic'
    # Setup uic
    uicCmd = '%s ${SOURCES} -o ${TARGET}' %(uic) 
    bld = Builder(action = uicCmd, prefix = "ui_", suffix = ".h", single_source = True )
    env.Append(BUILDERS = {'qt_uic': bld})

    moc ='moc'
    # setup moc
    mocCmd = '%s ${SOURCES} -o ${TARGET}' %(moc) 
    bld = Builder(action = mocCmd, prefix = "moc_", suffix = ".cpp", single_source = True)
    env.Append(BUILDERS = {'qt_moc': bld})
    # cxx->moc variant
    bld = Builder(action = mocCmd, prefix = "", suffix = ".moc", single_source = True)
    env.Append(BUILDERS = {'qt_cxxmoc': bld})

    rcc ='rcc'
    # Setup rcc
    rccCmd = '%s ${SOURCES} -o ${TARGET}' %(rcc) 
    bld = Builder(action = rccCmd, prefix = "qrc_", suffix = ".cxx", single_source = True )
    env.Append(BUILDERS = {'qt_rcc': bld})

def applyQtBuildFlags(env):
    env.AppendUnique( CPPDEFINES = ['QT_ON'] )
    if sca_util.GetPlatform() == 'win32':
        env['no_import_lib'] = 1
        env['WINDOWS_INSERT_MANIFEST'] = "True"
        #lib_env2.AppendUnique( LIBS = ['opengl'])
    elif sca_util.GetPlatform() == 'darwin':
        env.Append( LINKFLAGS = ['-framework','QtCore', '-framework','QtGui','-framework','QtOpenGL','-framework','OpenGL'])
        env.AppendUnique( CXXFLAGS =['-F/Library/Frameworks/QtOpenGL.framework','-F/Library/Frameworks/QtCore.framework','-F/Library/Frameworks/QtGui.framework'] )
    else:
        env.AppendUnique( LIBS= ['QtCore','QtGui'] )
