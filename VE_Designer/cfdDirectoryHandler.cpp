#include "cfdDirectoryHandler.h"

void cfdDirectoryHandler::changeToNewWorkingDirectory(char* newdirname)
{
    
   //store the current directory so we can change back to it
   if ( (this->currentworkingdirectory = getcwd(NULL, 100)) == NULL )
   {
      std::cerr<<"Couldn't get the current working directory!"<<std::endl;
      exit(1);
   }

   //load up the frames from the specified directory
   if ( newdirname )
   {
      //try to open the directory
      this->dir = opendir( newdirname);
      if (this->dir == NULL)
      {
         std::cerr << "Cannot open directory: " << newdirname << std::endl;
         exit(1);
      }
      //change into this directory so that we can find the files
      chdir( newdirname );
   }
 
}

void cfdDirectoryHandler::changeToOldWorkingDirectory(char* olddirname)
{
   
   closedir(this->dir);
   this->directory = 0;
   chdir(olddirname);
}

void cfdDirectoryHandler::setDirectory(char* cdir)
{
   this->directory = cdir;
}

char* cfdDirectoryHandler::getDirectory()
{
   this->directory = getcwd(NULL,100);
   return this->directory;
}

