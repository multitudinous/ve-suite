There are a number of changes that are required to OSG to support MacOS 10.7 and the new clang compilers. The one change that is most important is the one discussed here:

http://forum.openscenegraph.org/viewtopic.php?t=9121

Here is an explanation from the list:

Unfortunately, LLVM (or the GCC frontend) is smart enough to turn the cast + call of the base version back into the floating point version - but the symbol look-up then resolves to the osg wrapper - and hence the problem we're seeing. Since osg/Math was created, OS-X does define the 'f' versions - but I'm not sure which version started doing so - the 10.6 SDK math.h includes the 'f' variants, but plain gcc 4.2 wasn't smart enough to turn the cast-to-double + call pattern into the float version. 

on the OSG mail list:

Posted: Wed Sep 14, 2011 10:44 pm    Post subject: Release builds with gcc-llvm on Mac (Xcode 4), cosf/sinf recursion

that corresponds with commit:

r12877 | robert | 2011-11-07 07:36:50 -0700 (Mon, 07 Nov 2011) | 4 lines

The remainder of the changes in the 10.7 patch are just to correct for a more strict compiler.
