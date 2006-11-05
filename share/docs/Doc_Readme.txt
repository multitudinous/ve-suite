VE-Suite Documentation Readme
-Jeff Groves, Nov. 3, 2006

The makefile creates documentation from the .xml files listed in makefile. The following make commands are supported:

html: HTML documentation
txt: Text documentation (makes HTML docs as intermediate step)
pdf: PDF documentation
all: Makes all of the above
clean: Remove .fo files
clobber: Remove the made documentation and the directories storing them.

REQUIREMENTS

This makefile requires an installation of the docbook programs and Version 1.4.2 or newer of the Java runtime environment. Before running it, you also need to set the JAVA_HOME variable to point to the directory containing bin/java. (For example, with Java 1.5.0_08 on Linux, a likely setting would be JAVA_HOME=/usr/java/jdk1.5.0_08)

--Java--

You can download Java here:

http://java.sun.com/javase/downloads/index.jsp

Set the environment variable JAVA_HOME to point to the directory containing bin/java, as explained above.

--Docbook Tools--

The makefile is configured to use the docbook programs on the VRAC intranet. If you need to install a local copy of docbook, you can download them (~30 MB) from either:

http://www.infiscape.com/~patrick/docbook.tar.bz2
-or-
http://www.infiscape.com/~patrick/docbook.7z

Unpack the docbook directory. They require Version 1.4.2 or never of the Java runtime environment. You can download it here:

http://java.sun.com/javase/downloads/index.jsp

Set the environment variable JAVA_HOME to point to the directory containing bin/java (for example, with Java 1.5.0_08 on Linux, a likely setting would be JAVA_HOME=/usr/java/jdk1.5.0_08).

Once you've installed the docbook tools, you'll need to modify docbook.mk to point to them. Open docbook.mk with a text editor and change these settings at the beginning of it:

DOCBOOOK_ROOT?= <docbook directory path>
The makefile uses the VERSION settings to find the tools inside the docbook directory. Change the VERSION settings to match version number of their folders, as shown below:

docbook-xsl-<DOCBOOK_XSL_VERSION>
xalan-j_<XALAN_VERSION>
saxon-<SAXON_VERSION>
fop-<FOP_VERSION>
batik-<BATIK_VERSION>

Now you should be ready to use your local docbook tools with this makefile.

NOTES:

-The makefile grabs templates from the oasis-open website for the documentation. It won't be able to run if www.oasis-open.org is down.
-Everything in the .xml files' corresponding directories is deleted when "make clobber" is called. Don't put other files in them.
