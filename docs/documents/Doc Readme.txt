VE-Suite Documentation Readme
-Jeff Groves, Nov. 1, 2006

The makefile creates documentation from the .xml files listed in makefile. The following make commands are supported:

html: HTML documentation
txt: Text documentation (makes HTML docs as intermediate step)
pdf: PDF documentation
all: Makes all of the above
clean: Remove .fo files
clobber: Remove the made documentation and the directories storing them.

REQUIREMENTS
Most of the commands used to create the documentation are gathered from the VE-Suite network. However, these commands are used locally:
html2txt
openjade

NOTES:
-The makefile grabs templates from the oasis-open website for the documentation. It won't be able to run if www.oasis-open.org is down.
-Everything in the .xml files' corresponding directories is deleted when "make clobber" is called. Don't put other files in them.
