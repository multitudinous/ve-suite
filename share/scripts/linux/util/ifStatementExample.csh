#!/bin/csh
# sets up environment to build and/or run VE-Xplorer
if ( -e /etc/redhat-release ) then
   set firstWord=`cat /etc/redhat-release | awk -F" " '{print $1}'`
   #echo "firstWord is" ${firstWord}
   if ( ${firstWord} == "Red" ) then
      set thirdWord=`cat /etc/redhat-release | awk -F" " '{print $3}'`
      if ( ${thirdWord} == "Enterprise" ) then
         # extract some words from file to create something like RHEL_3
         setenv CFDHOSTTYPE `cat /etc/redhat-release | awk -F" " '{print "RHEL_" $7}'`
      else
         # extract some words from file to create something like RedHat_8.0 or RedHat_9
         setenv CFDHOSTTYPE `cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
      endif
   else if ( ${firstWord} == "Fedora" ) then
      # extract some words from file to create something like Fedora_1
      setenv CFDHOSTTYPE `cat /etc/redhat-release | awk -F" " '{print $1 "_" $4}'`
   else if ( ${firstWord} == "CentOS" ) then
      # extract some words from file to create something like Fedora_1
      setenv CFDHOSTTYPE `cat /etc/redhat-release | awk -F" " '{print $1 "_" $3}'`
   endif
else if ( -e /etc/SuSE-release ) then
   # extract some words from file to create something like SuSE_9.2_x86-64
   setenv CFDHOSTTYPE `head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3 "_" $4}'`
else
   #echo "uname is" `uname`
   setenv CFDHOSTTYPE `uname`
endif

#if creation of CFDHOSTTYPE caused parenthesis to be inserted, then remove...
setenv CFDHOSTTYPE `echo \"$CFDHOSTTYPE\" | sed -e 's/(//g' | sed -e 's/)//g' | sed -e 's/"//g'`

if ( ! $?TAO_BUILD ) then

   echo "do something"

else if ( $TAO_BUILD != "TRUE" && $TAO_BUILD != "FALSE" ) then

   echo "do something"

endif

#echo "specifying libraries for" ${CFDHOSTTYPE}
if ( ${CFDHOSTTYPE} == "IRIX64" ) then

   echo "do something"
      
else if ( ${CFDHOSTTYPE} =~ RedHat_* || ${CFDHOSTTYPE} =~ Fedora_* || ${CFDHOSTTYPE} =~ RHEL_* ) then

   echo "do something"

else if ( ${CFDHOSTTYPE} =~ S*SE_*.*_*64 ) then

   echo "do something"

else if ( ${CFDHOSTTYPE} =~ S*SE_*.*_i* ) then

   echo "do something"

else if ( ${CFDHOSTTYPE} == "SunOS" ) then

   echo "do something"

else if ( ${CFDHOSTTYPE} == "Darwin" ) then

   echo "do something"

else
   echo "ERROR: Unsupported operating system"
   echo "       OMNI_HOME, etc. are undefined"
endif

if ( $?OSG_HOME ) then

   echo "do something"

endif

