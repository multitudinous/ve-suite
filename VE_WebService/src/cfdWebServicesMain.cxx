//***insert license here***\\

//#include "cfdAppWrapper.h"
//#include "cfdVjObsWrapper.h"
#include "cfdWebServices.h"
#ifdef _TAO
#include "Executive_i.h"
#include <orbsvcs/CosNamingC.h>
#include <tao/BiDir_GIOP/BiDirGIOP.h>
#else
#include <omniORB4/CORBA.h>
#endif
#include <iostream>

cfdWebServices webServices;

int main(int argc, char* argv[])
{
   webServices.initCorba(argc, argv)


}