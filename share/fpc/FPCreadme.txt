NOTE: To have scons's flagpoll work properly, you will need to install the latest version of flagpoll here:
https://realityforge.vrsource.org/view/FlagPoll/FlagpollDownloads

And add this line to your .cshrc file:
setenv FLAGPOLL_PATH *Insert path to this directory here*

To use the fpc files copy the *.in files to *.fpc files. Once copied
edit the files to reflect where the various software libraries
are installed. Additional items to check are the vtk version, lib
directories on 64bit platforms, and include paths.