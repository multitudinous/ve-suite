#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
# used for transient data processing: to concatenate many planes into one polydata object

setenv TAO_MACHINE localhost
setenv TAO_PORT 1234
${TAO_ROOT}/orbsvcs/Naming_Service/Naming_Service -ORBEndPoint iiop://${TAO_MACHINE}:${TAO_PORT} &
sleep 5
Exe_server -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService
