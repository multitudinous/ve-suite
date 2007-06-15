/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _WIN32
#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>
#else
//#error ("NOT Portable to Windows Yet!!")
#include <windows.h>
#include <direct.h>
#endif

#include <iostream>
#include "cfdTransientFlowManager.h"
#include "cfdFrame.h"
#include "cfdDataSet.h"
#include "cfdTransientVizHandler.h"
#include "cfdTransientSet.h"
#include "fileIO.h"
#include <vpr/Util/Debug.h>

cfdTransientFlowManager::cfdTransientFlowManager()
{
   this->frames = NULL;
   this->order = NULL;
   this->directory = NULL;
   this->frameDataType = 0;
   this->numFrames = 0;
   this->isTimeToUpdateSequence = 1;
}

cfdTransientFlowManager::~cfdTransientFlowManager()
{
   if ( this->frames != NULL )
   {
      delete [] this->frames;
      this->frames = NULL;
   }

   int num = this->frameFileNames.size();
   for ( int i = 0; i < num; i++ )
   {
      delete [] this->frameFileNames[ i ];

      delete [] this->order;
      this->order = NULL;
   }

   if ( num > 0 )
      this->frameFileNames.clear();
}

//////////////////////////////////////////////////////////
//load the frames for the animation from the specified directory and 
//create the sequence node
//////////////////////////////////////////////////////
int cfdTransientFlowManager::StoreFrameFileNames()
{
   vprDEBUG(vprDBG_ALL,1) << " Storing Frame File Names"
                          << std::endl << vprDEBUG_FLUSH;

   char * cwd;
#ifndef WIN32
   //store the current directory so we can change back to it
   if ( (cwd = getcwd(NULL, 100)) == NULL )
   {
      std::cerr<<"Couldn't get the current working directory!"<<std::endl;
      exit(1);
   }

   //load up the frames from the specified directory
   if ( this->directory )
   {
      //try to open the directory
      DIR* dir = opendir( this->directory );
      direct* file = 0;
      if (dir == NULL)
      {
         vprDEBUG(vprDBG_ALL,0) << "Cannot open directory: " << this->directory
                                << std::endl << vprDEBUG_FLUSH;
         return 0;
      }
      //change into this directory so that we can find the files
      chdir( this->directory );
      
      //get the name of each file
      while( (file = readdir(dir)) != NULL )
      {
         //assume all files in this directory are part of the sequence
         //if found a vtk file or a geometry file add it to list of names
         if( strstr(file->d_name, ".vtk") ||
             strstr(file->d_name, ".iv" ) || 
             strstr(file->d_name, ".flt") ||
             strstr(file->d_name, ".stl") ) 
         {
            char * pathAndFileName = new char[
                        strlen(this->directory) + strlen(file->d_name) + 2 ];
            strcpy(pathAndFileName,this->directory);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,file->d_name);

            this->frameFileNames.push_back(pathAndFileName);
            vprDEBUG(vprDBG_ALL,1) << " pathAndFileName : " 
               << pathAndFileName << std::endl << vprDEBUG_FLUSH;
            //increment the number of frames found
            this->numFrames++;
         }
      };
         vprDEBUG(vprDBG_ALL,1) << " Number of frames: " << this->numFrames
                              << std::endl << vprDEBUG_FLUSH;
         //close the directory
         closedir(dir);
         dir = 0;
         file = 0;
      }else{
         std::cerr << "Error!!! Frames directory not specified!!" << std::endl;
         return 0;
      }
#else
   //biv--this code will need testing
   //BIGTIME!!!!!!!
   char buffer[_MAX_PATH];
   BOOL finished;
   HANDLE hList;
   TCHAR directory[MAX_PATH+1];
   WIN32_FIND_DATA fileData;

   //windows compatibility
   //get the current working directory
   if ((cwd = _getcwd(buffer, _MAX_PATH)) == NULL){
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      return 0;
   }

   // Get the proper directory path for transient files
   sprintf(directory, "%s\\*", this->directory);

   //get the first file
   hList = FindFirstFile(directory, &fileData);
  
   //check to see if directory is valid
   if(hList == INVALID_HANDLE_VALUE){ 
	   std::cerr<<"No transient files found in: "<<this->directory<<std::endl;
      return 0;
   }else{
      // Traverse through the directory structure
      finished = FALSE;
      while (!finished){
         //add the file name to our data list
		 //assume all vtk files in this directory are part of the sequence
		 if(strstr(fileData.cFileName, ".vtk")||
			strstr(fileData.cFileName, ".iv" ) || 
            strstr(fileData.cFileName, ".flt") ||
            strstr(fileData.cFileName, ".stl")){
            char* pathAndFileName = new char[
                  strlen(this->directory) + strlen(fileData.cFileName) + 2 ];
            strcpy(pathAndFileName,this->directory);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            this->frameFileNames.push_back( pathAndFileName );
            vprDEBUG(vprDBG_ALL, 1) << " pathAndFileName : " 
                                    << pathAndFileName << std::endl << vprDEBUG_FLUSH;
            //increment the number of frames found
            this->numFrames++;
		 }
		 //check to see if this is the last file
		 if(!FindNextFile(hList, &fileData)){
            if(GetLastError() == ERROR_NO_MORE_FILES){
               finished = TRUE;
			}
		 }
	  }
   }
   //close the handle
   FindClose(hList);
   //make sure we are in the correct directory
   chdir(cwd);
#endif
      // The directory must contain only transient files of a particular type
      // (ie, y-plane slices)
      // Filenames should be something like:
      //   grid_0.vtk ==> time step 0
      //      . . .
      //   grid_21.vtk==> time step 21
      // The important components of the filename are the underscore before
      // the integer, the integer, the period, and the extension. 
      // The extension must be "vtk" for data files
      // or "iv", "flt", or "stl" for geometry files.  
      // The integer may be zero-padded (grid_0021.vtk is OK). 

      //Now numerically order the list of names because readdir doesn't
      this->order  = new int [ this->numFrames ];
      for (int j = 0; j < this->numFrames; j++)
      {
         int number = fileIO::extractIntegerBeforeExtension( this->frameFileNames[ j ] );
         this->order[ number ] = j;
         vprDEBUG(vprDBG_ALL,1) << "\t" << j << "\t" << number << "\t" 
            << this->frameFileNames[ j ] << std::endl << vprDEBUG_FLUSH;
      }
   
   //change back to the original directory
   chdir( cwd );
   vprDEBUG(vprDBG_ALL,2) << " finished StoreFrameFileNames"
                          << std::endl << vprDEBUG_FLUSH;
   return this->numFrames;
}

void cfdTransientFlowManager::LoadFrames()
{
   vprDEBUG(vprDBG_ALL,1) << "Loading frames" << std::endl << vprDEBUG_FLUSH;

   // if you have existing frames delete the frames
   if ( this->frames != NULL )
   {
      vprDEBUG(vprDBG_ALL,2) << "Deleting frames" << std::endl << vprDEBUG_FLUSH;
      delete [] this->frames;
   }

   //inc lets you control whether every frame, every other frame, etc. is written
   int inc = 1;
   //if( this->numFrames > 150 ) inc = 2;

   this->frames = new cfdFrame[ this->numFrames ];
   for(int i = 0; i < this->numFrames; i+=inc)
   {
      //create the new frame for each time step
      vprDEBUG(vprDBG_ALL,0) << " === Creating frame : " << i << " ==="
                             << " frameDataType = " << this->frameDataType
                             << std::endl << vprDEBUG_FLUSH;
      this->frames[ i ].SetParameterFile( this->paramFile, this->member );
      this->frames[ i ].SetFrameDataType( this->frameDataType );
      vprDEBUG(vprDBG_ALL,2) << "\tSet Arrow" << std::endl << vprDEBUG_FLUSH;
      this->frames[ i ].SetArrow( this->GetActiveDataSet()->GetArrow() );
      this->frames[ i ].SetArrowSize( this->GetVectorScale() );
// the following code won't work because we removed the static functions from
// cfdobjects.
/*
      this->frames[ i ].SetActiveDataSets( this->GetActiveDataSet(),
                                           this->GetActiveMeshedVolume(),
                                           this->GetActiveParticleData(),
                                           this->GetActiveSurfaceData() );

*/
      vprDEBUG(vprDBG_ALL,1) << "\t" << this->frameFileNames[ this->order[ i ] ]
                             << std::endl << vprDEBUG_FLUSH;
      vprDEBUG(vprDBG_ALL,2) << " End Creating Frame"
                             << std::endl << vprDEBUG_FLUSH;
   }
   vprDEBUG(vprDBG_ALL,1) << " End of Frames" << std::endl << vprDEBUG_FLUSH;
}

void cfdTransientFlowManager::SetParameterFile( cfdTransientVizHandler *paramFile, int member )
{
   this->paramFile = paramFile;
   this->member = member;
}

void cfdTransientFlowManager::Update( void )
{
   if ( this->frameDataType == cfdFrame::GEOM )
   {
      vprDEBUG(vprDBG_ALL,1) << " cfdTransientFlowManager::Update -- "
                             << "Won't LoadFrames for GEOM"
                             << std::endl << vprDEBUG_FLUSH;
   }
   
   // replace existing frames with new ones
   if ( this->isTimeToUpdateSequence && 
        this->frameDataType != cfdFrame::GEOM )
   {
      this->LoadFrames();
      this->isTimeToUpdateSequence = 0;
   }

   // A hack for now
   // Need to have something specifically for pfsequence
   this->updateFlag = true;
   vprDEBUG(vprDBG_ALL,1) << " Finished Update for TransientFlowManager."
                          << std::endl << vprDEBUG_FLUSH;
}
   
int cfdTransientFlowManager::GetNumberOfFrames( void )    
{
   return this->numFrames;
}

void cfdTransientFlowManager::SetDirectory( char * dir )
{
   this->directory = dir;
}

char * cfdTransientFlowManager::GetDirectory()
{
   return this->directory;
}

void cfdTransientFlowManager::SetFrameDataType( int type )
{
   this->frameDataType = type;
}

int cfdTransientFlowManager::GetFrameDataType()
{
   return this->frameDataType;
}

void cfdTransientFlowManager::CreateNodeList( void )
{
   vprDEBUG(vprDBG_ALL,0) << " Creating node list for frameDataType "
                          << this->frameDataType
                          << std::endl << vprDEBUG_FLUSH;

   //inc lets you control whether every frame, every other frame, etc. is written
   int inc = 1;
   //if( this->numFrames > 150 ) inc = 2;
   for(int i = 0; i < this->numFrames; i+=inc)
   {
      this->frames[ i ].CreateFrame( this->frameFileNames[ this->order[ i ] ] );
      //this->_geodes.push_back( this->frames[ i ].GetcfdNode() );
      //vprDEBUG(vprDBG_ALL,1) << " Creating Geodes : " << i << " : "
      //                       << this->_geodes[ i ]
      //                       << std::endl << vprDEBUG_FLUSH;
   }
}
