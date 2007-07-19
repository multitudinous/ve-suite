#!/bin/sh
# this is a bourne shell script

if [[ -e /etc/redhat-release ]]
then
   #echo "found /etc/redhat-release"
   # extract some words from file to create something like RedHat_8.0
   #export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
   export firstWord=`cat /etc/redhat-release | awk -F" " '{print $1}'`
   #echo "firstWord is" ${firstWord}
   if [[ ${firstWord} = "Red" ]]
   then
      export thirdWord=`cat /etc/redhat-release | awk -F" " '{print $3}'`
      if [[ ${thirdWord} = "Enterprise" ]]
      then
         # extract some words from file to create something like RHEL_3
         export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print "RHEL_" $7}'`
      else
         # extract some words from file to create something like RedHat_8.0 or RedHat_9
         export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 $2 "_" $5}'`
      fi
   elif [[ ${firstWord} = "Fedora" ]]
   then
      # extract some words from file to create something like Fedora_1
      export CFDHOSTTYPE=`cat /etc/redhat-release | awk -F" " '{print $1 "_" $4}'`
   fi
elif [[ -e /etc/SuSE-release ]]
then
   #echo "found /etc/SuSE-release"
   # extract first and third words from file to create something like SuSE_9.1
   export CFDHOSTTYPE=`head -1 /etc/SuSE-release | awk -F" " '{print $1 "_" $3 "_" $4}'`
else
   echo "uname is" `uname`
   export CFDHOSTTYPE=`uname`
fi

#if creation of CFDHOSTTYPE caused parenthesis to be inserted, then remove...
export CFDHOSTTYPE=`echo \"$CFDHOSTTYPE\" | sed -e 's/(//g' | sed -e 's/)//g' | sed -e 's/"//g'`

if [[ ! $?TAO_BUILD ]]
then

   echo "do something"

elif [[ ${TAO_BUILD} != "TRUE" && ${TAO_BUILD} != "FALSE" ]]
then

   echo "do something"

fi


case "$CFDHOSTTYPE" in
   IRIX*) 

   echo "do something"

;;
   RedHat*|RHEL_*|Fedora_*) 

   echo "do something"

;;
   SuSE*) 

   echo "do something"

;;
   *)
   echo "ERROR: Unsupported operating system"
   echo "       OMNI_HOME, etc. are undefined"
;;
esac

if [[ $?OSG_HOME ]]
then

   echo "do something"

fi
