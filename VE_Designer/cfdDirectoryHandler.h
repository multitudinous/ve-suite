#ifndef CFD_DIRECTORY_HANDLER_H
#define CFD_DIRECTORY_HANDLER_H

#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <fstream>

class cfdDirectoryHandler
{
   cfdDirectoryHandler() {}
   ~cfdDirectoryHandler() {}
   
   public:
   void changeToNewWorkingDirectory(char* dirname);
   void changeToOldWorkingDirectory(char* dirname);
   void setDirectory(char* dirname);
   char* getDirectory();

   private:
   DIR* dir;
   char* directory;
   char* currentworkingdirectory;

};
