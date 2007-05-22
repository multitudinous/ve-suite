Using osgPolyTrans and osgOQ
============================

Last updated: Phase I RC1, April 18 2007



ptViewer
========

ptViewer is an osgviewer-like application designed to test and demonstrate osgPolyTrans and osgOQ.

Example uses
------------

    ptViewer cow.osg
        Loads OSG's standard "cow.osg" model and uses osgOQ to manage it. Not really very impressive, but the point is that ptViewer will apply osgOQ to any model you load. It's actually useful for larger models.

    ptViewer usmc23_test_asm.igs
        Loads the IGES file using the osgPolyTrans plugin. By default, this will convert the file to an intermediate OpenFlight format, then load the OpenFlight file, manage it with osgOQ, and display it.

    ptViewer --stock
        Loads a stock scene consisting of a complex orange ball inside a partially opened box. This is a canned osgOQ demo. Make sure the "data" directory in this source tree is included in OSG_FILE_PATH, or ptViewer will fail to load the orange sphere.

Key commands
------------

    F5 -- Applies to "--stock" only. Modifies the transform nodes above the orange balls.
    F6 -- Toggles osgOQ's enable state. Use this to turn occlusion queries off and compare performance.
    F7 -- When osgOQ is enabled, this displays a representation of the bounding volumes used in the query tests.
    F8 -- Increases the debug verbosity for three frames. This causes every OcclusionQueryNode in the tree to dump information to std::out. This can generate thousands of lines of output for large models.
    'o' -- Dumps the model out to an .osg file so you can see the osgOQ node arrangement. Practical only for small models.
    
    ptViewer also supports the key commands available in the osgviewer application.

Command line arguments
----------------------

    --BufferSize <n> -- Specifies the number of occlusion query buffers. If negative, osgOQ will use as many buffers as it thinks it needs. If non-negative, queries are limited and as a result parts of the scene graph might not receive any occlusion testing.
    
    ptViewer also supports the command line arguments available in the osgviewer application.


osgPolyTrans
============

osgPolyTrans is an OSG plugin that interfaces with PolyTrans to import PolyTrans-supported model files.

Configuration file
------------------

osgPolyTrans can be controlled with several parameters specified in a config file. To specify a config file, set the OSG_POLYTRANS_CONFIG_FILE environment variable to the file name. (osgPolyTrans will use the OSG_FILE_PATH to help locate the file if this is set to a relative path.)

An example configuration file is in the data directory of the source tree. This file contains comments describing most of the parameters.

Options
-------

Your application can control some parameters using the osgDB::Options string passed in to readNodeFile(). Eventually, all parameters settable in the config file will also be available as Options. Currently, only BufferSize is settable in the Options string. For an example, see the ptViewer source code, which sets the BufferSize option when the --BufferSize command line argument is present.

Using osgPolyTrans in your application
--------------------------------------

Unlike other OSG plugins, you must explicitly tell OSG to load the osgolyTrans plugin using the osgDB::Registry::LoadLibrary() method. This allows the osgPolyTrans plugin to support all file types, including file names suffixed by revision numbers. For an example of using LoadLibrary to load the plugin, look near the start of ptViewer's main() entry point.

When you call readNodeFile() to load a file with a revision number, specify the exact file name plus revision number to load. osgolyTrans is currently incapable of appending a revision number or selecting the highest/latest revision number.

Intermediate file issues
------------------------

By default, osgPolyTrans uses OpenFlight as an intermediate file type, which introduces two potential problems.

 1) The OSG OpenFlight loader is a memory pig and you could exhaust memory loading the intermediate OpenFlight file.
 2) The PolyTrans OpenFlight exporter allows the user to specify that the file should be written to a subfolder. If set, this could interfere with sgPolyTrans' caching scheme.

You can specify that osgPolyTrans use Wavefront .obj format instead of OpenFlight by setting the "COMHelper_IntermediateFileType" parameter in the config file. Note that Wavefront files, while smaller than equivalent OpenFlight files, can be slower to load.

Caching
-------

osgPolyTrans supports caching to avoid redundantly converting unmodified files. However, this is currently not a perfect solution. For caching to work, you should follow these rules:

 * Don't set the "COMHelper_BaseIntermediateFileName" option in the config file.
 * Don't set the "DeleteIntermediateFile" option (or set it to "false", which is the default).
 * Don't set the "CachedLoad" option (or set it to "true", which is the default).
 * Don't use models with lots of external references (sub-models). osgPolyTrans caching assumes the cache is valid if the cached file date is more recent than the source file date, and doesn't consider the dates of any externally referenced files.
 * osgPolyTrans expects to find the cache file in the same directory as the source file. Don't move or delete this file. If the file was exported from PolyTrans to a subfolder, osgPolyTrans' cache system won't find it and the file will be reconverted.

If the cache doesn't appear to be working (PolyTrans always does a conversion), set OSG_NOTIFY_LEVEL to INFO and look for explanation messages prefixed with "osgdb_PolyTrans:".


osgOQ
=====

osgOQ is an OSG NodeKit that employs OpenGL's occlusion query feature to avoid rendering complex geometry.

Configuration file
------------------

osgOQ can be controlled with several parameters specified in a config file. To specify a config file, set the OSGOQ_CONFIG_FILE environment variable to the file name. (osgOQ will use the OSG_FILE_PATH to help locate the file if this is set to a relative path.)

An example configuration file is in the "data" directory of the source tree. This file contains comments describing most of the parameters.

Using osgOQ in your application
-------------------------------

Place a scene graph under osgOQ control by creating an OcclusionQueryRoot (OQR) node and adding your subgraph as a child. For an example of how to do this, open the ptViewer source (ptViewer.cpp) and search for "OcclusionQueryRoot".

The OQR traverses your scene graph and inserts OcclusionQueryNodes (OQNs) at strategic locations to perform the actual occlusion testing.

You can control the OQR's behavior by creating an OcclusionQueryContext (OQC), setting its parameters, and passing it to the OQR constructor. If you don't pass an OQC to the OQR's constructor, the OQR creates its own OQC.

The OQC constructor checks to see if the OSGOQ_CONFIG_FILE environment variable was set, and if so, it loads that file and uses stored values to override default behavior. All of the parameters you can set by calling OQC methods can also be set in the config file.

You should never need to create an OQN directly. Let the OQR do this for you. However, it's reasonable that developers might want more control over OQN placement, so a future version could support applications creating their own OQNs directly and inserting them as needed. (In fact, this might already work, but hasn't been tested.)

Flat versus non-flat
--------------------

The OQR provides two algorithms for placing OQNs, Flat and NonFlat. You can specify the algorithm with the "UseNonFlatQueryNodePlacement" config file option, which defaults to "true".

NonFlat is the default, which allows subgraphs that are children of OQNs to also contain other OQNs. This algorithm can create rendering artifacts. If your viewpoint is inside the top-most OQN bounding volume, but the back of the bounding volume isn't visible due to intervening geometry, then osgOQ will believe the entire subgraph is not visible. This results in flickering as a sequence of frames alternates between drawing and not drawing the subgraph.

For visually-correct rendering, set "UseNonFlatQueryNodePlacement" to "false". This ensures that, given any node and its path to the root node, only one OQN will be in that path.

Threading models
----------------

In order to not block rendering, osgOQ reads occlusion query results after rendering a frame, then uses those results for the next frame. To encapsulate this algorithm in a NodeKit requires that osgOQ read the previous frame's results during the current frame's cull traversal. This means that the rendering context that was current when drawing (when the queries were issued) must be current to the thread performing the cull (when the queries are retrieved).

This can pose a problem for osgViewer-based applications that use non-default thread models. Some of these models prohibit the cull traversal from accessing the rendering context used during the draw traversal. This can cause osgOQ to crash and/or spew OpenGL errors. To avoid this problem, osgViewer-based apps should use the default thread model. This might be enhanced in a future osgOQ release.
