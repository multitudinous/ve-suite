#!/bin/csh -f
# -f means fast: don't read .cshrc file for set variables
setenv TAO_MACHINE ids7.scilan
setenv TAO_PORT 1234

# Start the nameserver, wait a few seconds, then start the executive...
${TAO_ROOT}/orbsvcs/Naming_Service/Naming_Service -ORBEndPoint iiop://${TAO_MACHINE}:${TAO_PORT} &
sleep 5
Exe_server -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService &
