# $Id: Makefile 16737 2005-02-03 19:47:26Z patrickh $

default: html

# Need for Saxon on RHEL64: setenv JAVA_HOME /usr/lib/jvm/jre-1.4.2-gcj

#VESUITE=	vesuite
#README=		README
#INSTALL=	INSTALL
# XML_FILES=	$(README).xml $(INSTALL).xml
#XML_FILES=	$(VESUITE).xml

XML_FILES=\
 vesuite.xml \
 README.xml \
 INSTALL.xml

XSLT_TOOL=Saxon 
#FO_VERSION=XEP


# $(VESUITE).html: $(VESUITE).xml
# $(VESUITE).txt: $(VESUITE).html
# $(VESUITE).pdf: $(VESUITE).fo

# $(README).txt: $(README).html
# $(README).pdf: $(README).fo
# $(INSTALL).txt: $(INSTALL).html
# $(INSTALL).pdf: $(INSTALL).fo

include docbook.mk
