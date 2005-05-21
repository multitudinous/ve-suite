========================================
Setting up dependency build environment
========================================

   VE-Suite is dependent on a couple of libraries that are available
to download and build from the web.
   1) VTK (Visualization ToolKit) http://www.vtk.org/get-software.php
   2) OSG (OpenSceneGraph) http://www.openscenegraph.org/
   3) OpenThreads http://openthreads.sourceforge.net/
   4) Producer http://www.andesengineering.com/Producer/index.html 
   5) wxWidgets http://www.wxwidgets.org/

   Before building VE-Suite these libraries should be downloaded (and built
if necessary) for the desired platform (i.e. Linux,IRIX,Windows). I suggest
creating a common directory for doing this. At the command prompt 
on UNIX style platforms, for example, type:

>mkdir ve_depends
>cd ve_depends

On windows the directory might look something like:
C:\ve_depends

NOTE: For OpenSceneGraph to build correctly, under ve_depends a common 
OSG directory should be created to contain OSG,OpenThreads,Producer.
For example, on UNIX:

>cd ve_depends
>mkdir OSG
>cd OSG

Place the downloaded files for Producer,OpenThreads and OpenSceneGraph in
the OSG directory.
========================================================
General instructions for extracting the directory trees
========================================================
   Download the tar/zip files into the ve_depends directory. Then extract 
the files which will create the individual dependency directory trees.
To extract the directory trees on UNIX use:

gunzip for .gz files
tar -xvf for .tar files

On windows use something like WinZip to extract the files.

=================================
Building the dependency libraries
=================================
   Each library has it's own specific set of instructions for building
the library.   See BUILD_OSG.txt,BUILD_WX.txt,BUILD_VTK.txt for specific
instructions for building each library.

 
