/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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

#include "VE_Xplorer/cfdTeacher.h"
#include "VE_SceneGraph/cfdGroup.h"
#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdNode.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Xplorer/cfdCommandArray.h"
#include "VE_Xplorer/cfdWriteTraverser.h"
#include "VE_SceneGraph/cfdPfSceneManagement.h"
#include <iostream>
#include <string>
#include <sstream>

#include <vpr/Util/Debug.h>
#include <gmtl/MatrixOps.h>
#include <gmtl/Matrix.h>
#include <gmtl/gmtl.h>

#include <boost/filesystem/operations.hpp> // includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

cfdTeacher::cfdTeacher( std::string specifiedDir, VE_SceneGraph::cfdDCS* worldDCS )
{
   this->directory = specifiedDir;

   vprDEBUG(vprDBG_ALL,1) << "PFB directory : \"" << this->directory << "\""
                          << std::endl << vprDEBUG_FLUSH;

   // initialize in case the directory is not there...
   this->numFiles = 0;
   pfb_count = 0;
   _cfdWT = NULL;
   this->DCS = NULL;
   this->node = NULL;
   this->_worldDCS = 0;
   pfb_count = 0;
   _cfdWT = NULL;
   this->DCS = new VE_SceneGraph::cfdDCS();
   this->DCS->SetName( "Teacher Node" );
   _worldDCS = worldDCS;
   _worldDCS->AddChild( this->DCS );

   char *cwd;

#ifndef WIN32
   //try to open the directory
   DIR* dir = opendir( this->directory.c_str() );
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
   chdir( this->directory.c_str() );

   //get the name of each file
   direct* file = 0;
   while ( (file = readdir(dir)) != NULL ) 
   {
      //assume all pfb files in this directory should be loaded
      if(strstr(file->d_name, ".pfb")||strstr(file->d_name, ".ive")) 
      {
         //char * fileName = new char[strlen(file->d_name)+1];
         //strcpy( fileName, file->d_name );
         this->pfbFileNames.push_back( std::string(file->d_name) );
         //delete [] fileName;
         vprDEBUG(vprDBG_ALL,0) << "Found performer binary : " << this->pfbFileNames.back()
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
   TCHAR* tdirectory;//[MAX_PATH+1];
   WIN32_FIND_DATA fileData;
   char buffer[MAX_PATH];
   cwd = _getcwd(buffer,MAX_PATH);

   // Get the proper directory path
   //sprintf(directory, "%s\\*", this->directory);
   std::ostringstream dirStringStream;
   dirStringStream <<"./"<<this->directory << "\\*";
   std::string dirString = dirStringStream.str();
   tdirectory = (char*)dirString.c_str();

   // Get the first file
   hList = FindFirstFile(tdirectory, &fileData);

   boost::filesystem::path dir_path( this->directory.c_str() );
   try
   {
      if ( boost::filesystem::is_directory( dir_path ) )
      {
         finished = FALSE;
         while ( ! finished )
         {
            //assume all pfb files in this directory should be loaded
            if ( strstr(fileData.cFileName, ".pfb")||strstr(fileData.cFileName, ".ive") )
            {
               //char * fileName = new char[strlen(fileData.cFileName)+1];
               //strcpy( fileName, fileData.cFileName );
               std::ostringstream filenameStream;
               filenameStream <<"./STORED_FILES/"<<fileData.cFileName;
               //std::string fileString = filenameStream.str();
			   this->pfbFileNames.push_back( filenameStream.str() );
               //delete [] fileName;
               vprDEBUG(vprDBG_ALL,0) << "Found performer binary : " << this->pfbFileNames.back()
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
   }
   catch ( const std::exception& ex )
   {
	   std::cout << ex.what() << std::endl;
   }
#endif
   // how many performer binaries found ?
   this->numFiles = this->pfbFileNames.size();
   vprDEBUG(vprDBG_ALL,1) << "Number of performer binaries: " << numFiles
                          << std::endl << vprDEBUG_FLUSH;

   
   this->node = new VE_SceneGraph::cfdNode * [ this->numFiles ];
   
   for (int i=0; i<this->numFiles; i++)
   {
      this->node[ i ] = new VE_SceneGraph::cfdNode();
	   this->node[ i ]->LoadFile( (char*)this->pfbFileNames[ i ].c_str() );
   }
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

   vprDEBUG(vprDBG_ALL,1) << "exiting cfdTeacher destructor"
                          << std::endl << vprDEBUG_FLUSH;
}

VE_SceneGraph::cfdNode * cfdTeacher::getpfNode( int i )
{
   return this->node[i];
}

VE_SceneGraph::cfdDCS * cfdTeacher::GetcfdDCS()
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
	   return (char*)this->pfbFileNames[i].c_str();
   }
   else
   {
      return NULL;
   }
}

bool cfdTeacher::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == LOAD_PFB_FILE )
   {
      vprDEBUG(vprDBG_ALL,1) << "LOAD_PFB_FILE: numChildren = " 
         << this->GetcfdDCS()->GetNumChildren()
         << ", cfdTeacher_state = "
         << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << std::endl << vprDEBUG_FLUSH;

      if ( this->GetcfdDCS()->GetNumChildren() == 0 )
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: addChild" 
                                << std::endl << vprDEBUG_FLUSH;
            
         this->GetcfdDCS()->AddChild( 
            this->getpfNode( (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) ) );
      }
      else
      {
         vprDEBUG(vprDBG_ALL,2) << "LOAD_PFB_FILE: replaceChild" 
                                << std::endl << vprDEBUG_FLUSH;
         int child = (int)commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
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
	  //check if the path to STORED_FILES exists
	  boost::filesystem::path dir_path( this->directory );
	  try
	  {
	     boost::filesystem::is_directory( dir_path );
	  }
	  catch ( const std::exception& ex )
	  {
		 std::cout << ex.what() << std::endl;
		 boost::filesystem::create_directory(dir_path);
		 std::cout << "...so we made it for you..." << std::endl;
	  }

      // Generate a .pfb filename...
      const char* pfb_filename;
      std::ostringstream dirStringStream;
      dirStringStream << this->directory << "/stored_scene_" 
#ifdef _PERFORMER
                        << this->pfb_count << ".pfb";
#elif _OSG
         << this->pfb_count << ".ive";
#endif
      std::string dirString = dirStringStream.str();
      pfb_filename = dirString.c_str();

      vprDEBUG(vprDBG_ALL,0) << "scene stored as " << pfb_filename
                             << std::endl << vprDEBUG_FLUSH;

      // store the world DCS matrix..
      if ( _worldDCS )
      {
         gmtl::Matrix44f m = this->_worldDCS->GetMat();

         //temporarily reset the world DCS matrix to the identity
         gmtl::Matrix44f I;

         // Make an identity matrix
         gmtl::identity( I );
         this->_worldDCS->SetMat( I );
         float scaleUnity[ 3 ];
         scaleUnity[ 0 ] = scaleUnity[ 1 ] = scaleUnity[ 2 ] = 1.0f;
         this->_worldDCS->SetScaleArray( scaleUnity );
      
         writePFBFile(this->_worldDCS,(char*)pfb_filename);

         // The following is a hack until osg or juggler 
         // allow us to pull scale info out of a 4x4 matrix
         float* scaleArray = this->_worldDCS->GetScaleArray();
         float tempScale = 1.0f / scaleArray[ 0 ];
         gmtl::Matrix44f scaleMat;
         gmtl::setScale( scaleMat, tempScale );
         gmtl::Matrix44f mTemp = scaleMat * m;
         // restore the world DCS matrix...
         //this->_worldDCS->SetMat( m );
         this->_worldDCS->SetMat( mTemp );
         this->_worldDCS->SetScaleArray( scaleArray ); 
      }
      else
      {
         writePFBFile(VE_SceneGraph::cfdPfSceneManagement::instance()->GetRootNode(),
                    (char*)pfb_filename);
      }
      // store the active geometry and viz objects as a pfb
      // (but not the sun, menu, laser, or text)
      int store_int = 0;

      vprDEBUG(vprDBG_ALL,1) << "|   Stored Scene Output " << store_int
                             << std::endl << vprDEBUG_FLUSH;
      
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

void cfdTeacher::writePFBFile( VE_SceneGraph::cfdNode* graph,char* fileName)
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

