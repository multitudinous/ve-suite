/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id: Doc_Readme.txt $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
VE-Suite Documentation Readme

The makefile creates documentation from the .xml files listed in makefile. The following make commands are supported:

html: HTML documentation
chunk-html: HTML document split into multiple pages
txt: Text documentation (makes HTML docs as intermediate step)
pdf: PDF documentation
all: Makes all of the above
clean: Remove .fo files
clobber: Remove the made documentation and the directories storing them.

All output except chunk-html is put in a directory named after the .xml file used. Chunk-html output is put in the <name>-multipage directory instead. For example:
If you build documentation from vrjuggler.xml:
-HTML, TXT, and PDF documentation goes into the vrjuggler directory.
-Chunked HTML documentation goes into the vrjuggler-multipage directory.

REQUIREMENTS

This makefile requires an installation of the docbook programs and Version 1.4.2 or newer of the Java runtime environment. Before running it, you also need to set the JAVA_HOME variable to point to the directory containing bin/java (For example, with Java 1.5.0_08 on Linux, a likely setting would be JAVA_HOME=/usr/java/jdk1.5.0_08) and set VE_SUITE_HOME to VE Suite's directory.

--Libraries--

This makefile uses functions found in the libxml library. If you don't have it installed (i.e. if 'xmllint --help' doesn't work), you can download it here:

http://www.xmlsoft.org/

--HTML2Text--

This makefile uses the html2text program to conversions to text files. You can download it here:

http://userpage.fu-berlin.de/~mbayer/tools/html2text.html

--Docbook Tools--

The makefile is configured to use the docbook programs on the VRAC intranet. If you need to install a local copy of docbook, you can download them (~30 MB) from either:

http://www.infiscape.com/~patrick/docbook.tar.bz2
-or-
http://www.infiscape.com/~patrick/docbook.7z

Unpack the docbook directory.

--Java--

The docbook tools require Version 1.4.2 or never of the Java runtime environment. You can download it here:

http://java.sun.com/javase/downloads/index.jsp

Set the environment variable JAVA_HOME to point to the directory containing bin/java (for example, with Java 1.5.0_08 on Linux, a likely setting would be JAVA_HOME=/usr/java/jdk1.5.0_08).

Once you've installed the docbook tools, you'll need to modify docbook.mk to point to them. Open VE_Installer/mk/docbook.mk with a text editor and change these settings at the beginning of it:

DOCBOOK_ROOT?= <docbook directory path>
DOCBOOK_XSL_VERSION?= etc. (see below)
The makefile uses the VERSION settings to find the tools inside the docbook directory. Change the VERSION settings to match version number of their folders, as shown below:

docbook-xsl-<DOCBOOK_XSL_VERSION>
xalan-j_<XALAN_VERSION>
saxon-<SAXON_VERSION>
fop-<FOP_VERSION>
batik-<BATIK_VERSION>

Now you should be ready to use your local docbook tools with this makefile.

--HTML Images--

To customize the location docbook looks for HTML images in, set IMG_PATH before you activate it.
Example: setenv IMG_PATH http://www.vrac.iastate.edu/imagedump/
Be sure to include the trailing slash.
Note: IMG_PATH is not applied to any filerefs that start with "/" or contain "//:".

NOTES:

-The makefile grabs templates from the oasis-open website for the documentation. It won't be able to run if www.oasis-open.org is down.
-Everything in the .xml files' corresponding directories is deleted when "make clobber" is called. Don't put other files in them.
