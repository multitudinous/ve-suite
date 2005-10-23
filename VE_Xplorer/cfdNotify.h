#include <Performer/pr.h>
#include <iostream>


void notifyHandler( pfNotifyData* data )
{
   std::cout << data->severity << std::endl;
   std::cout << "\t" << data->emsg << std::endl;
   std::cout << data->pferrno << std::endl;
}
