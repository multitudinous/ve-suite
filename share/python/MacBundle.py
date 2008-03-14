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



def TOOL_BUNDLE(env):
    """defines env.LinkBundle() for linking bundles on Darwin/OSX, and
       env.MakeBundle() for installing a bundle into its dir.
       A bundle has this structure: (filenames are case SENSITIVE)
       sapphire.bundle/
         Contents/
           Info.plist (an XML key->value database; defined by BUNDLE_INFO_PLIST)
           PkgInfo (trivially short; defined by value of BUNDLE_PKGINFO)
           MacOS/
             executable (the executable or shared lib, linked with Bundle())
    Resources/
         """
    if 'BUNDLE' in env['TOOLS']: return
    platform = 'darwin'
    if platform == 'darwin':
        if tools_verbose:
            print " running tool: TOOL_BUNDLE"
        env.Append(TOOLS = 'BUNDLE')
        # This is like the regular linker, but uses different vars.
        # XXX: NOTE: this may be out of date now, scons 0.96.91 has some bundle linker stuff built in.
        # Check the docs before using this.
        #LinkBundle = SCons.Builder.Builder(action=[SharedCheck, "$BUNDLECOM"],
        #                                   emitter="$SHLIBEMITTER",
        #                                   prefix = '$BUNDLEPREFIX',
        #                                   suffix = '$BUNDLESUFFIX',
        #                                   target_scanner = ProgScan,
        #                                   src_suffix = '$BUNDLESUFFIX',
        #                                   src_builder = 'SharedObject')
        #env['BUILDERS']['LinkBundle'] = LinkBundle
        #env['BUNDLEEMITTER'] = None
        #env['BUNDLEPREFIX'] = ''
        #env['BUNDLESUFFIX'] = ''
        #env['BUNDLEDIRSUFFIX'] = '.bundle'
        #env['FRAMEWORKS'] = ['-framework Carbon', '-framework System']
        #env['BUNDLE'] = '$SHLINK'
        #env['BUNDLEFLAGS'] = ' -bundle'
        #env['BUNDLECOM'] = '$BUNDLE $BUNDLEFLAGS -o ${TARGET} $SOURCES $_LIBDIRFLAGS $_LIBFLAGS $FRAMEWORKS'
        # This requires some other tools:
        TOOL_WRITE_VAL(env)
        TOOL_SUBST(env)
        # Common type codes are BNDL for generic bundle and APPL for application.
        def MakeBundle(env, bundledir, app,
                       key, info_plist,
                       typecode='BNDL', creator='SapP',
                       icon_file='#macosx-install/sapphire-icon.icns',
                       subst_dict=None,
                       resources=[]):
            """Install a bundle into its dir, in the proper format"""
            # Substitute construction vars:
            for a in [bundledir, key, info_plist, icon_file, typecode, creator]:
                a = env.subst(a)
            if SCons.Util.is_List(app):
                app = app[0]
            if SCons.Util.is_String(app):
                app = env.subst(app)
                appbase = os.path.basename(app)
            else:
                appbase = os.path.basename(str(app))
            if not ('.' in bundledir):
                bundledir += '.$BUNDLEDIRSUFFIX'
            bundledir = env.subst(bundledir) # substitute again
            suffix=bundledir[string.rfind(bundledir,'.'):]
            if (suffix=='.app' and typecode != 'APPL' or
                suffix!='.app' and typecode == 'APPL'):
                raise Error, "MakeBundle: inconsistent dir suffix %s and type code %s: app bundles should end with .app and type code APPL."%(suffix, typecode)
            if subst_dict is None:
                subst_dict={'%SHORTVERSION%': '$VERSION_NUM',
                            '%LONGVERSION%': '$VERSION_NAME',
                            '%YEAR%': '$COMPILE_YEAR',
                            '%BUNDLE_EXECUTABLE%': appbase,
                            '%ICONFILE%': os.path.basename(icon_file),
                            '%CREATOR%': creator,
                            '%TYPE%': typecode,
                            '%BUNDLE_KEY%': key}
            env.Install(bundledir+'/Contents/MacOS', app)
            f=env.SubstInFile(bundledir+'/Contents/Info.plist', info_plist,
                            SUBST_DICT=subst_dict)
            #env.Depends(f,SCons.Node.Python.Value(key+creator+typecode+env['VERSION_NUM']+env['VERSION_NAME']))
            env.WriteVal(target=bundledir+'/Contents/PkgInfo',
                         source=SCons.Node.Python.Value(typecode+creator))
            resources.append(icon_file)
            for r in resources:
                if SCons.Util.is_List(r):
                    env.InstallAs(os.path.join(bundledir+'/Contents/Resources',
                                               r[1]),
                                  r[0])
                else:
                    env.Install(bundledir+'/Contents/Resources', r)
            return [ SCons.Node.FS.default_fs.Dir(bundledir) ]
        # This is not a regular Builder; it's a wrapper function.
        # So just make it available as a method of Environment.
        SConsEnvironment.MakeBundle = MakeBundle

def TOOL_WRITE_VAL(env):
    if tools_verbose:
        print " running tool: TOOL_WRITE_VAL"
    env.Append(TOOLS = 'WRITE_VAL')
    def write_val(target, source, env):
        """Write the contents of the first source into the target.
        source is usually a Value() node, but could be a file."""
        f = open(str(target[0]), 'wb')
        f.write(source[0].get_contents())
        f.close()
    env['BUILDERS']['WriteVal'] = Builder(action=write_val)
    

def Glob( includes = Split( '*' ), excludes = None, dir = '.'):
   """Similar to glob.glob, except globs SCons nodes, and thus sees
   generated files and files from build directories.  Basically, it sees
   anything SCons knows about.  A key subtlety is that since this function
   operates on generated nodes as well as source nodes on the filesystem,
   it needs to be called after builders that generate files you want to
   include.
   It will return both Dir entries and File entries
   """
   def fn_filter(node):
      fn = os.path.basename(str(node))
      match = 0
      for include in includes:
         if fnmatch.fnmatchcase( fn, include ):
            match = 1
            break
      if match == 1 and not excludes is None:
         for exclude in excludes:
            if fnmatch.fnmatchcase( fn, exclude ):
               match = 0
               break
      return match
   def filter_nodes(where):
       children = filter(fn_filter, where.all_children(scan=0))
       nodes = []
       for f in children:
           nodes.append(gen_node(f))
       return nodes
   def gen_node(n):
       """Checks first to see if the node is a file or a dir, then
       creates the appropriate node. [code seems redundant, if the node
       is a node, then shouldn't it just be left as is?
       """
       if type(n) in (type(''), type(u'')):
           path = n
       else:
           path = n.abspath
       if os.path.isdir(path):
           return Dir(n)
       else:
           return File(n)
   here = Dir(dir)
   nodes = filter_nodes(here)
   node_srcs = [n.srcnode() for n in nodes]
   src = here.srcnode()
   if src is not here:
       for s in filter_nodes(src):
           if s not in node_srcs:
               # Probably need to check if this node is a directory
               nodes.append(gen_node(os.path.join(dir,os.path.basename(str(s)))))
   return nodes


def ensureWritable(nodes):
    for node in nodes:
        if os.path.exists(node.path) and not (stat(node.path)[0] & 0200):
           chmod(node.path, 0777)
    return nodes
# Copy given patterns from inDir to outDir
def DFS(root, skip_symlinks = 1):
    """Depth first search traversal of directory structure.  Children
    are visited in alphabetical order."""
    stack = [root]
    visited = {}
    while stack:
        d = stack.pop()
        if d not in visited:  ## just to prevent any possible recursive
                              ## loops
            visited[d] = 1
            yield d
        stack.extend(subdirs(d, skip_symlinks))
def subdirs(root, skip_symlinks = 1):
    """Given a root directory, returns the first-level subdirectories."""
    try:
        dirs = [os.path.join(root, x) for x in os.listdir(root)]
        dirs = filter(os.path.isdir, dirs)
        if skip_symlinks:
            dirs = filter(lambda x: not os.path.islink(x), dirs)
        dirs.sort()
        return dirs
    except OSError, IOError: return []
def copyFiles (env, outDir, inDir):
    inDirNode = env.Dir(inDir)
    outDirNode = env.Dir(outDir)
    subdirs = DFS (inDirNode.name)
    files = []
    for subdir in subdirs:
        files += glob.glob (os.path.join (subdir, '*'))
    outputs = []
    for f in files:
        if os.path.isfile (f):
           outputs += ensureWritable (env.InstallAs (outDirNode.abspath + '/' + f, env.File (f)))
    return outputs
def InstallBundle (env, target_dir, bundle):
    """Move a Mac OS-X bundle to its final destination"""
    # check parameters!
    if os.path.exists(target_dir) and not os.path.isdir (target_dir):
       raise SCons.Errors.UserError, "InstallBundle: %s needs to be a directory!"%(target_dir)
    bundledirs = env.arg2nodes (bundle, env.fs.File)
    outputs = []
    for bundledir in bundledirs:
        suffix = bundledir.name [bundledir.name.rfind ('.'):]
        if (os.path.exists(bundledir.name) and not os.path.isdir (bundledir.name)) or suffix != '.app':
           raise SCons.Errors.UserError, "InstallBundle: %s needs to be a directory with a .app suffix!"%(bundledir.name)
    # copy all of them to the target dir
        outputs += copyFiles (env,target_dir, bundledir)
    return outputs

def TOOL_SUBST(env):
    """Adds SubstInFile builder, which substitutes the keys->values of SUBST_DICT
    from the source to the target.
    The values of SUBST_DICT first have any construction variables expanded
    (its keys are not expanded).
    If a value of SUBST_DICT is a python callable function, it is called and
    the result is expanded as the value.
    If there's more than one source and more than one target, each target gets
    substituted from the corresponding source.
    """
    env.Append(TOOLS = 'SUBST')
    def do_subst_in_file(targetfile, sourcefile, dict):
        """Replace all instances of the keys of dict with their values.
        For example, if dict is {'%VERSION%': '1.2345', '%BASE%': 'MyProg'},
        then all instances of %VERSION% in the file will be replaced with 1.2345 etc.
        """
        try:
            f = open(sourcefile, 'rb')
            contents = f.read()
            f.close()
        except:
            raise SCons.Errors.UserError, "Can't read source file %s"%sourcefile
        for (k,v) in dict.items():
            contents = re.sub(k, v, contents)
        try:
            f = open(targetfile, 'wb')
            f.write(contents)
            f.close()
        except:
            raise SCons.Errors.UserError, "Can't write target file %s"%targetfile
        return 0 # success

    def subst_in_file(target, source, env):
        if not env.has_key('SUBST_DICT'):
            raise SCons.Errors.UserError, "SubstInFile requires SUBST_DICT to be set."
        d = dict(env['SUBST_DICT']) # copy it
        for (k,v) in d.items():
            if callable(v):
                d[k] = env.subst(v())
            elif SCons.Util.is_String(v):
                d[k]=env.subst(v)
            else:
                raise SCons.Errors.UserError, "SubstInFile: key %s: %s must be a string or callable"%(k, repr(v))
        for (t,s) in zip(target, source):
            return do_subst_in_file(str(t), str(s), d)

    def subst_in_file_string(target, source, env):
        """This is what gets printed on the console."""
        return '\n'.join(['Substituting vars from %s into %s'%(str(s), str(t))
                          for (t,s) in zip(target, source)])

    def subst_emitter(target, source, env):
        """Add dependency from substituted SUBST_DICT to target.
        Returns original target, source tuple unchanged.
        """
        d = env['SUBST_DICT'].copy() # copy it
        for (k,v) in d.items():
            if callable(v):
                d[k] = env.subst(v())
            elif SCons.Util.is_String(v):
                d[k]=env.subst(v)
        Depends(target, SCons.Node.Python.Value(d))
        return target, source

    subst_action=SCons.Action.Action(subst_in_file, subst_in_file_string)
    env['BUILDERS']['SubstInFile'] = Builder(action=subst_action, emitter=subst_emitter)
