#include <iostream>
#include <fstream>
#include <sstream>
//#include <stdlib.h>
#include <string>

int main (int argc, char **argv)
{
   std::string inPartFileName;
   
   
   
   char textLine[256];
   int i,j;
   for (i=1;i<=8;i++)
   {
      std::ostringstream infileName;
      infileName<<"file"<<i<<".part";
      inPartFileName=infileName.str();
      std::ifstream inPartFile;
      inPartFile.open((inPartFileName).c_str());
      std::cout<<inPartFileName<<std::endl;
      if(i!=1)
      {
         for(j=0;j<19;j++)
         {
            inPartFile.getline(textLine,256);
         }

      }
      
      std::ofstream outPartFile;
      outPartFile.precision(10);
      outPartFile.open("file.part", std::ios::app|std::ios::ate);
      //outPartFile.open("file.part",std::ios::app);
      while(!inPartFile.eof())
      {
         inPartFile.getline(textLine,256);
         outPartFile<<textLine<<std::endl;
         
      }
      inPartFile.close();
      outPartFile.close();

   }

   return 0;


}
