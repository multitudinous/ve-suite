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
#include <windows.h>
#include <direct.h>
#endif   // _WIN32

#include "cfdTeacher.h"
#include "cfdGroup.h"
#include "cfdDCS.h"
#include "cfdNode.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"
#include "cfdWriteTraverser.h"

#include <iostream>
#include <string>
#include <sstream>

#include <vpr/Util/Debug.h>
#include <gmtl/MatrixOps.h>
#include <gmtl/Matrix.h>

cfdTeacher::cfdTeacher( char specifiedDir[], cfdDCS* worldDCS )
{
   this->directory = NULL;
   this->setDirectory( specifiedDir );

   vprDEBUG(vprDBG_ALL,1) << "PFB directory : \"" << this->directory << "\""
                          << std::endl << vprDEBUG_FLUSH;

   // initialize in case the directory is not there...
   this->numFiles = 0;
   pfb_count = 0;
   _cfdWT = NULL;
   this->DCS = NULL;
   this->node = NULL;

   char *cwd;

#ifndef WIN32
   //try to open the directory
   DIR* dir = opendir( this->directory );
   if (dir == NULL) 
   {
      vprDEBUG(vprDBG_ALL,0) <<"Cannot open directory \"" << this->directory 
                             << "\"" << std::endl << vprDEBUG_FLUSH;
      return;
   }

   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit(1);
   }

   //change into this directory so that we can find the files
   chdir( this->directory );

   //get the name of each file
   direct* file = 0;
   while ( (file = readdir(dir)) != NULL ) 
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
   closedir( dir );  //close the directory
   dir = 0;
   file = 0;

#else
   //windows compatibility
   //biv--this code will need some checking
   //BIGTIME!!!!!!!
   BOOL finished;
   HANDLE hList;
   TCHAR* directory;//[MAX_PATH+1];
   WIN32_FIND_DATA fileData;
   char buffer[MAX_PATH];
   cwd = _getcwd(buffer,MAX_PATH);

   // Get the proper directory path
   //sprintf(directory, "%s\\*", this->directory);
   std::ostringstream dirStringStream;
   dirStringStream << this->directory << "\\*";
   std::string dirString = dirStringStream.str();
   directory = (char*)dirString.c_str();

   // Get the first file
   hList = FindFirstFile(directory, &fileData);
   if (hList == INVALID_HANDLE_VALUE)
   { 
      vprDEBUG(vprDBG_ALL,0) << "Cannot open directory \"" << this->directory 
                             << "\"" << std::endl << vprDEBUG_FLUSH;
      return;
   }
   else
   {
      finished = FALSE;
      while ( ! finished )
      {
         //assume all pfb files in this directory should be loaded
         if ( strstr(fileData.cFileName, ".pfb") )
         {
            char * fileName = new char[strlen(fileData.cFileName)+1];
            strcpy( fileName, fileData.cFileName );
            this->pfbFileNames.push_back( fileName );
            vprDEBUG(vprDBG_ALL,0) << "Found performer binary : " << fileName
                                   << std::endl << vprDEBUG_FLUSH;
         }
         if ( !FindNextFile(hList, &fileData) )
         {
            if ( GetLastError() == ERROR_NO_MORE_FILES )
            {
               finished = TRUE;
            }
         }
      }
   }
#endif
   // how many performer binaries found ?
   this->numFiles = this->pfbFileNames.size();
   vprDEBUG(vprDBG_ALL,1) << "Number of performer binaries: " << numFiles
                          << std::endl << vprDEBUG_FLUSH;

   pfb_count = 0;
   _cfdWT = NULL;
   this->DCS = new cfdDCS();
   this->node = new cfdNode * [ this->numFiles ];
   
   for (int i=0; i<this->numFiles; i++)
   {
      this->node[ i ] = new cfdNode();
      this->node[ i ]->LoadFile( this->pfbFileNames[ i ] );
      //this->DCS->addChild( this->node[i] );
   }

   _worldDCS = worldDCS;
   _worldDCS->AddChild( this->DCS );

   //change back to the original directory
   chdir( cwd );
}

cfdTeacher::~cfdTeacher( )
{
   int i;
   for ( i = 0; i < this->numFiles; i++)
   {  
      this->DCS->RemoveChild( this->node[i] );
      delete this->node[i];
   }
   delete this->DCS;

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

cfdNode * cfdTeacher::getpfNode( int i )
{
   return this->node[i];
}

cfdDCS * cfdTeacher::GetcfdDCS()
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

bool cfdTeacher::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_PFB_FILE )
   {
      vprDEBUG(vprDBG_ALL,1) << "LOAD_PFB_FILE: numChildren = " 
         << this->GetcfdDCS()->GetNumChildren()
         << ", cfdTeacher_state = "
         << commandArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE )
         << std::endl << vprDEBUG_FLUSH;

      if ( this->GetcfdDCS()->GetNumChildren() == 0 )
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: addChild" 
                                << std::endl << vprDEBUG_FLUSH;
            
         this->GetcfdDCS()->AddChild( 
            this->getpfNode( commandArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE ) ) );
      }
      else
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: replaceChild" 
                                << std::endl << vprDEBUG_FLUSH;
         int child = commandArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE );
         this->GetcfdDCS()->ReplaceChild( this->GetcfdDCS()->GetChild( 0 ), this->getpfNode( child ) );
      }
      return true;
   }
   else if ( ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_PFB_FILE ) ||
             ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL ) )
   {      
      vprDEBUG(vprDBG_ALL,2) << " cfdTeacher::CheckCommandId : CLEAR_ALL or CLEAR_PFB_FILE "
                             << std::endl  << vprDEBUG_FLUSH;
      if ( this->DCS != NULL )
      {
         if ( this->GetcfdDCS()->GetNumChildren() > 0 )
         {
            this->GetcfdDCS()->RemoveChild( this->GetcfdDCS()->GetChild( 0 ) );
         }
         return true;
      }
   }
   else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == RECORD_SCENE )
   {
      // Needs to be moved to cfdTeacher...soon.
      // Generate a .pfb filename...
      const char* pfb_filename;//[100];
      //sprintf( pfb_filename , "%s/stored_scene_%i.pfb",
      //         this->getDirectory(), this->pfb_count );
      std::ostringstream dirStringStream;
      dirStringStream << this->getDirectory() << "/stored_scene_" 
                        << this->pfb_count << ".pfb";
      std::string dirString = dirStringStream.str();
      pfb_filename = dirString.c_str();

      vprDEBUG(vprDBG_ALL,0) << "scene stored as " << pfb_filename
                             << std::endl << vprDEBUG_FLUSH;

      // store the world DCS matrix..
      gmtl::Matrix44f m;
      m = this->_worldDCS->GetMat();

      // temporarily reset the world DCS matrix to the identity
      gmtl::Matrix44f I;
      // Make an identity matrix
      gmtl::identity( I );
      this->_worldDCS->SetMat( I );

      //biv--convert the cfdSequence nodes to pfSequence nodes
      //for proper viewing in perfly
      writePFBFile(_worldDCS,(char*)pfb_filename);
      //pfdStoreFile( worldDCS, pfb_filename );

      // store the active geometry and viz objects as a pfb
      // (but not the sun, menu, laser, or text)
      int store_int = 0;

      vprDEBUG(vprDBG_ALL,1) << "|   Stored Scene Output " << store_int
                             << std::endl << vprDEBUG_FLUSH;
      
      // restore the world DCS matrix...
      this->_worldDCS->SetMat( m );


      // increment the counter and reset the id to -1...
      this->pfb_count ++;
      return true;
   }

   return false;
}

void cfdTeacher::UpdateCommand()
{
   std::cerr << "doing nothing in cfdVectorBase::UpdateCommand()" << std::endl;
}
// Need to fix later

void cfdTeacher::writePFBFile( cfdNode* graph,char* fileName)
{
   //make sure we have a writer
   if(!_cfdWT){
      _cfdWT = new cfdWriteTraverser(fileName);
   }else{
      _cfdWT->setOutputFileName(fileName);
   }
   //set the graph
   _cfdWT->setNode(graph);

   //set the "swapping" callback
   _cfdWT->setCallback(1);

   //write out the file
   //_cfdWT->activateSequenceNodes();
   _cfdWT->writePfbFile();
}

