/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdTeacher.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _WIN32
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#else
#error ("NOT Portable to Windows Yet!!")
#endif

#include "cfdTeacher.h"
#include <iostream>
#include <vpr/Util/Debug.h>
#include <Performer/pfdu.h>
#include <Performer/pf/pfDCS.h>

cfdTeacher::cfdTeacher( char specifiedDir[], pfGroup * groupNode )
{
   this->directory = NULL;
   this->setDirectory( specifiedDir );

   vprDEBUG(vprDBG_ALL,1) << "PFB directory : \"" << this->directory << "\""
                          << std::endl << vprDEBUG_FLUSH;

   // initialize in case the directory is not there...
   this->numFiles = 0;

   //try to open the directory
   DIR* dir = opendir( this->directory );
   if (dir == NULL) 
   {
      vprDEBUG(vprDBG_ALL,0) <<"Cannot open directory \"" << this->directory 
                             << "\"" << std::endl << vprDEBUG_FLUSH;
      return;
   }

   char *cwd;
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit(1);
   }

   //change into this directory so that we can find the files
   chdir( this->directory );

   //get the name of each file
   direct* file = 0;
   while((file = readdir(dir)) != NULL) 
   {
      //assume all pfb files in this directory should be loaded
      if(strstr(file->d_name, ".pfb")) 
      {
         char * fileName = new char[strlen(file->d_name)+1];
         strcpy( fileName, file->d_name );
         this->pfbFileNames.push_back( fileName );
         vprDEBUG(vprDBG_ALL,0) << "Found performer binary : " << fileName
                                << std::endl << vprDEBUG_FLUSH;
      }
   }
   // how many performer binaries found ?
   this->numFiles = this->pfbFileNames.size();
   vprDEBUG(vprDBG_ALL,1) << "Number of performer binaries: " << numFiles
                          << std::endl << vprDEBUG_FLUSH;

   closedir( dir );  //close the directory
   dir = 0;
   file = 0;

   this->DCS = new pfDCS;
   this->node = new pfNode * [ this->numFiles ];
   
   for (int i=0; i<this->numFiles; i++)
   {
      this->node[ i ] = pfdLoadFile( this->pfbFileNames[ i ] );
      //this->DCS->addChild( this->node[i] );
   }

   groupNode->addChild( this->DCS );

   //change back to the original directory
   chdir( cwd );
}

cfdTeacher::~cfdTeacher( )
{
   int i;

   for ( i = 0; i < this->numFiles; i++)
   {  
      this->DCS->removeChild( this->node[i] );
      pfDelete( this->node[i] );
   }
   pfDelete( this->DCS );

   // delete vector of pfbFileNames
   for ( i = 0; i < this->numFiles; i++ )
   {
      vprDEBUG(vprDBG_ALL,1) << "deleting pfbFileName " 
         << this->pfbFileNames[i] << std::endl << vprDEBUG_FLUSH;
      delete [] this->pfbFileNames[i];
   }

   delete [] this->directory;
   this->directory = NULL;

   vprDEBUG(vprDBG_ALL,1) << "exiting cfdTeacher destructor"
                          << std::endl << vprDEBUG_FLUSH;
}

pfNode * cfdTeacher::getpfNode( int i )
{
   return this->node[i];
}

pfDCS * cfdTeacher::getpfDCS()
{
   return this->DCS;
}

int cfdTeacher::getNumberOfFiles()
{
   return this->numFiles;
}

char * cfdTeacher::getFileName( int i )
{
   if ( i >= 0 && i < this->numFiles )
   {
      return this->pfbFileNames[i];
   }
   else
   {
      return NULL;
   }
}
char * cfdTeacher::getDirectory()
{
   return this->directory;
}

void cfdTeacher::setDirectory( char * dir )
{
   if ( this->directory )
   {
      delete [] this->directory;
   }

   if ( dir == NULL )
   {
      this->directory = NULL;
      return;
   }

   this->directory = new char [ strlen(dir) + 1 ];
   strcpy( this->directory, dir );
}

