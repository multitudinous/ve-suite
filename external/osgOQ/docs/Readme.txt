osgOQ
=====

Last updated: Pre-beta, November 28 2007



Quick Start
===========

1. Set the OSG_ROOT environment variable to point to the root of your OpenSceneGraph directory (containing 'include' and 'lib' dirs). On WIndows, this is usually "OSG_ROOT=C:\Program Files\OpenSceneGraph".
2. Use CMake to generate project files for your platform.
3. Build everything.
4. Add the binary locations to your PATH.
5. Add the 'data' directory to OSG_FILE_PATH.
6. Run "ptViewer --stock".
   The orange spheres are occluded when you rotate the model, which should produce a faster frame rate. Toggle occlusion queries off and on using the F6 key.



osgOQ
=====

osgOQ is an OSG NodeKit that employs OpenGL's occlusion query feature to avoid rendering complex geometry.

Using osgOQ in your application
-------------------------------

The key component of the osgOQ NodeKit is the OcclusionQueryNode (OQN). This Group-derived node will perform an occlusion test to determine whether or not to render its children subtrees.

In your application, you can place OQNs at any location within your scene graph. You can arrange them hierarchically if you wish, in which case subortinate OQNs only perform their tests if the ancestor OQN test passes.

osgOQ features several NodeVisitors for performing routine osgOQ-related operations.

    OcclusionQueryFlatVisitor -- Automatically inserts OQNs into a scene graph, but avoids any hierarchical nesting (any given parent path will only contain one OQN).

    OcclusionQueryNonFlatVisitor -- Automatically inserts OQNs into a scene graph using hierarchical OQNs if needed.

    PerDrawableQueryVisitor -- If a Geode in a scene graph contains several Drawables, but only one Drawables is large enough to merit occlusion query testing, this NodeVisitor will rearrange the scene graph so that the large Drawable is the only Drawable owned by parent Geodes. This allows an OQN to be placed above the owning Geode.

    EnableQueryVisitor -- Enables or disables occlusion query testing in visited OQNs.

    DebugDisplayVisitor -- Enables or disables display of a wireframe representation of the query geometry. Useful for debugging. Note that the debug display is automatically disabled if occlusion query testing is disabled.

    RemoveOcclusionQueryVisitor -- This NodeVisitor scrubs a scene graph to remove all occurances of OQNs.

    StatisticsVisitor -- Collects and displays statistics regarding OQN performance.


Flat versus non-flat
--------------------

There are two NodeVisitors to automatically insert OQNs into your scene graph, OcclusionQueryFlatVisitor and OcclusionQueryNonFlatVisitor.

NonFlat allows subgraphs that are children of OQNs to also contain other OQNs. This algorithm can create rendering artifacts. If your viewpoint is inside the top-most OQN bounding volume, but the back of the bounding volume isn't visible due to intervening geometry, then osgOQ will believe the entire subgraph is not visible. This results in flickering as a sequence of frames alternate between drawing and not drawing the subgraph.

For visually-correct rendering, use OcclusionQueryFlatVisitor, or avoid nesting OQNs if you place them manually. This ensures that, given any node and its path to the root node, only one OQN will be in that path.

Algorithm
---------

Internally, the OQN has an instance of a QueryGeometry (QG), which is a Drawable that performs occlusion queries. The QG uses the OQN children's bounding box as geometry during the query.

This process is repeated every frame.

    1. During the draw traversal, the QG issues the occlusion query, then adds a reference to itself to the Camera's post-draw callback. At the end of the draw traversal, the post-draw callback retrieves all query results and stores them back in the QG.

    2. In the subsequent frame's cull traversal, the OQN checks the query result from the previous frame to determine visibility. It traverses its children if it decides they are visible.

    3. A camera pre-draw callback clears the list of references owned by the post-draw callback.

One artifact of this algorithm is a latency before visible geometry is rendered. This latency is at least one frame. Because occlusion queries are inherently expensive, for efficiency purposes, osgOQ performs query testing every N frames (settable by the config file or programmatically). For this reason, the latency could be longer than one frame.

Configuration file
------------------

osgOQ can be controlled with several parameters specified in a config file. To specify a config file, set the OSGOQ_CONFIG_FILE environment variable to the file name. (osgOQ will use the OSG_FILE_PATH to help locate the file if this is set to a relative path.)

An example configuration file is in the "data" directory of the source tree. This file contains comments describing the parameters.

Config file options:

 * VisibilityThreshold
     Example:
     VisibilityThreshold 500

     If the occlusion query test indicates that the number of visible pixels is greater than this value, render the child geometry. Otherwise, don't render and continue to test for visibility in future frames. Default if unspecified: 500 pixels.

 * OccluderThreshold
     Example:
     OccluderThreshold 5000

     This option affects the behavior of the OcclusionQueryFlatVisitor, OcclusionQueryNonFlatVisitor, and the PerDrawableQueryVisitor.
     If the child geometry has less than the specified number of vertices, don't perform occlusion query testing (it's an occluder). Otherwise, perform occlusion query testing (it's an occludee). Default if unspecified: 5000 vertices.

 * QueryFrameCount
     Example:
     QueryFrameCount 5

     Wait the specified number of frames between queries. To disable this optimization, set to 1. Non-positive values are clamped to 1. Default if unspecified: 5 frames.

 * DebugBoundingVolume
     Example:
     DebugBoundingVolume false

     If "true" or "Y", bounding volumes will be rendered. This can be useful in development and debugging. Default if unspecified: false.

Known Bugs
----------

osgOQ currently leaks resources by not deleting OpenGL query IDs.



ptViewer
========

ptViewer is an osgviewer-like application designed to test and demonstrate osgOQ.

Example uses
------------

    ptViewer cow.osg
        Loads OSG's standard "cow.osg" model and uses osgOQ to manage it. Not really very impressive, but the point is that ptViewer will apply osgOQ to any model you load. It's actually useful for larger models.

    ptViewer --stock
        Loads a stock scene consisting of a complex orange ball inside a partially opened box. This is a canned osgOQ demo. Make sure the "data" directory in this source tree is included in OSG_FILE_PATH, or ptViewer will fail to load the orange sphere.

Key commands
------------

    F4 -- Applies to "--stock" only. Modifies the transform nodes above the orange balls.
    F6 -- Toggles osgOQ's enable state. Use this to turn occlusion queries off and compare performance.
    F7 -- When osgOQ is enabled, this displays a representation of the bounding volumes used in the query tests.
    F8 -- Increases the debug verbosity. This causes every OcclusionQueryNode in the tree to dump information to std::out. This can generate thousands of lines of output for large models.
    'o' -- Dumps out the model to an .osg file so you can see the osgOQ node arrangement. Practical only for small models.
    
    ptViewer also supports the key commands available in the osgviewer application.

Command line arguments
----------------------

    --stock - Loads the stock scene. 'data' directory must be in OSG_FILE_PATH.

    ptViewer also supports the command line arguments available in the osgviewer application.



oqTest00
========

ptViewer uses a NodeVisitor to automatically place OcclusionQueryNodes at strategic insertion points in the scene graph. In contrast, oqTest00 shows how to insert OcclusionQueryNodes manually.

This test application is otherwise quite similar to ptViewer.
