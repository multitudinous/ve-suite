#include <Performer/pr.h>
#include <iostream>


void notifyHandler( pfNotifyData* data )
{
   std::cout << data->emsg << std::endl;
}
