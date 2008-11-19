import re
from SCons.Script import *  # the usual scons stuff you get in a SConscript
import fnmatch
import os
import os.path 
from SCons.Defaults import SharedCheck, ProgScan
from SCons.Script.SConscript import SConsEnvironment
import glob
tools_verbose = True
#import filter

#########################################################################
# this is from: http://www.scons.org/wiki/InnoSetupBuilder?highlight=%28Builder%24%29

import SCons.Builder;
import os;

def runInno(source,target,env,for_signature):
    INNO_DEFINES=" ".join(['/D'+define for define in env['ISCCDEFINES']]);
    (TARGETDIR,TARGETFILE) = os.path.split(str(target[0]));
    return '%(ISCC)s %(SOURCE)s %(DEFINES)s /O "%(TARGETDIR)s" /F"%(TARGETFILE)s" %(ISCCOPTIONS)s ' % \
        {
            'ISCC':env['ISCC'],
            'ISCCOPTIONS':env['ISCCOPTIONS'],
            'DEFINES':INNO_DEFINES,
            'SOURCE':str(source[0]),
            'TARGETDIR':TARGETDIR,
            'TARGETFILE':TARGETFILE
        }

def generate(env,**kw):
    env['ISCC']='iscc';
    env['ISCCOPTIONS']='/q';
    env['ISCCDEFINES']=[];
    env['BUILDERS']['InnoInstaller'] = SCons.Builder.Builder( generator=runInno,
                                                                                        src_suffix='.iss');

def exists(env):
    return env.WhereIs("iscc");
    
#Example of how to use it
installer=env.InnoInstaller(target='setup_x64',source='setup_x64.iss',ISCCDEFINES=['APP_NAME="Your App Name"','APP_VERSION="10.4"']);
env.Depends(installer,ALL_THE_DEPENDENCIES); # need to do this manually since there is no dep scanner yet
