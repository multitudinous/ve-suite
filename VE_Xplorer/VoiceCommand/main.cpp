#include <iostream>
#include "SAPIVoiceInterpreter.h"

int main(int argc, char** argv)
{
   SAPIVoiceInterpreter *vI = new SAPIVoiceInterpreter(argc, argv);

   if(vI->SAPIInit())
   {
	  vI->Listen();
   }

   delete vI;
   return 0;
}
