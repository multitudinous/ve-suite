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
 * File:          $RCSfile: cfdReadParam.cxx,v $
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
//#error ("NOT Portable to Windows Yet!!")
#include <windows.h>
#include <direct.h>
#endif

#include "cfdReadParam.h"
#include "cfdTransientInfo.h"
//#include "cfd1DTextInput.h"
#include "cfdTransientSet.h"
#include "cfdDataSet.h"
#include "cfdFileInfo.h"
#include "fileIO.h"

#include <Performer/pf/pfDCS.h>

#include <cstdio>
#include <iostream>
#include <fstream>
#include <cstdlib>
#include <cmath>
//#include <istream>

#include <vpr/Util/Debug.h>

cfdReadParam::cfdReadParam( char *filein_name )
{
//files.reserve(10);   //added
   this->numGeoms = 0;
   this->bmpFile     = 0;
   this->soundFile   = 0;
   this->diameter    = 0.0;
   this->worldTrans[0] = this->worldTrans[1] = this->worldTrans[2] = 0.0f;
   this->worldRot[0] = this->worldRot[1] = this->worldRot[2] = 0.0f;
   this->worldScale[0] = this->worldScale[1] = this->worldScale[2] = 1.0f;
   this->scalarBarPos[0] = this->scalarBarPos[1] = this->scalarBarPos[2] = 0.0;
   this->scalarBarZRot = 0.0;
   this->scalarBarH = this->scalarBarW = 0.0;
   this->frames = 0;

   // IHCC Model - should be deleted at a later date
   ihccModel = false;
   // MUST BE LAST: do not initialize variables after this function
   data_read (filein_name);
}

cfdReadParam::~cfdReadParam()
{
   int temp = (int)this->files.size();
   if ( temp > 0 )
   {
      vprDEBUG(vprDBG_ALL,2) << " Deleting files"
                             << std::endl << vprDEBUG_FLUSH;
      for (int i=0; i < temp; i++)
      {
         vprDEBUG(vprDBG_ALL,2) << "\tcfdReadParam:Deleting file " << i 
                                << std::endl << vprDEBUG_FLUSH;
         delete this->files[ i ];
      }
      vprDEBUG(vprDBG_ALL,2) << "\tcfdReadParam:clearing files"
                             << std::endl << vprDEBUG_FLUSH;
      files.clear();
   }

   temp = (int)this->dataSets.size();
   if ( temp > 0 )
   {
      for (int i=0; i < temp; i++)
      {
         vprDEBUG(vprDBG_ALL,2) << "\tcfdReadParam:Deleting dataSet " << i 
                                << std::endl << vprDEBUG_FLUSH;
         delete this->dataSets[ i ];
      }
      vprDEBUG(vprDBG_ALL,2) << "\tcfdReadParam:clearing dataSets"
                             << std::endl << vprDEBUG_FLUSH;
      this->dataSets.clear();
   }

   temp = (int)this->transientInfo.size();
   // If we have any transient data
   if ( temp > 0 )
   {
      for (int i=0; i< temp; i++)
      {
         vprDEBUG(vprDBG_ALL,2) << "\tDeleting transientInfo " << i 
                                 << std::endl << vprDEBUG_FLUSH;
                             
         delete this->transientInfo[ i ];
      }
      vprDEBUG(vprDBG_ALL,2) << "\tcfdReadParam:clearing transientInfo"
                             << std::endl << vprDEBUG_FLUSH;
      this->transientInfo.clear();
   }

   delete [] guiVal;
}

void cfdReadParam::CreateNewDataSet()
{
   this->dataSets.push_back( new cfdDataSet() );
}

int cfdReadParam::GetNumberOfDataSets()
{
   return (int)this->dataSets.size();
}

cfdDataSet * cfdReadParam::GetDataSet( int i )
{
   if ( 0 <= i && i < (int)this->dataSets.size() )
      return this->dataSets[ i ];
   else
      return NULL;
}

cfdDataSet * cfdReadParam::GetDataSetWithName( const char * vtkFilename )
{
   int numDataSets = this->GetNumberOfDataSets();
   for ( int i = 0; i < numDataSets; i++ )
   {
      //std::cout << this->GetDataSet( i )->GetFileName() << std::endl;
      if ( ! strcmp(this->GetDataSet( i )->GetFileName(),vtkFilename) )
      {
         return this->GetDataSet( i );
         break;
      }
   }
   return NULL;
}

char * readDirName( std::ifstream &inFile, char * description )
{
  char textLine[256];
   char * dirName = new char [256];
   inFile >> dirName;
   
   inFile.getline( textLine, 256 );   //skip past remainder of line
#ifndef WIN32  
   
   //try to open the directory

   DIR* dir = opendir( dirName );
   if (dir == NULL) 
   {
      vprDEBUG(vprDBG_ALL,0) << " " << description << " \""
         << dirName << "\" does not exist"
         << std::endl << vprDEBUG_FLUSH;
      delete [] dirName;
      dirName = NULL;
   }
   else
   {
      vprDEBUG(vprDBG_ALL,0) << " " << description << " = \""
                             << dirName << "\""
                             << std::endl << vprDEBUG_FLUSH;
   }
   closedir( dir );
#else


#endif
   return dirName;
}

int readID( std::ifstream &inFile )
{
   int id;
   inFile >> id;
   char textLine [ 256 ];
   inFile.getline( textLine, 256 );   //skip past remainder of line
   return id;
}

void cfdReadParam::data_read ( char * filein_name )
{
   if ( ! fileIO::isFileReadable( filein_name ) ) 
   {
      std::cerr << "\nError: Could not open the input file " 
                << filein_name << " !" << std::endl;
      while( !fileIO::isFileReadable( filein_name ) )
      {
         std::cout << "\nEnter correct filename: " << std::endl; 
         std::cin >> filein_name;        
      }
   }
   
   std::ifstream inFile( filein_name, std::ios::in ); 

   // Read the information in the file.
   param_read( inFile );
   std::cout <<  "|   Finished reading the parameter file." << std::endl;
}

void cfdReadParam::param_read( std::ifstream &inFile )
{
   int numObjects;
   inFile >> numObjects;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   for(int i=0;i<numObjects;i++)
   {
      int id;
      inFile >> id;
      inFile.getline( textLine, 256 );   //skip past remainder of line
    
      switch(id){
         case 0:
            readWorldDCS( inFile );
            break;
         case 1:
            readScalarBar( inFile );
            break;
         case 2:
            //set1DText( inFile );
            break;
         case 5:
            BMPLoader( inFile );
            break;
         case 6:
            std::cerr << "Type 6 is no longer used: Use type 8" << std::endl;
            exit(1);
            break;
         case 7: 
            std::cerr << "Type 7 is no longer used: Use type 8" << std::endl;
            exit(1);
            break;
         case 8:
            Vtk( inFile );
            break;
         case 9: 
            Stl( inFile );
            break;
         case 10:
            getTransientInfo( inFile );
            break;
         case 11:
            soundData(inFile);
            break;
         case 12:
	         IMGReader(inFile);
            break;
         case 13:
	         ihccModel = true;
            break;
         case 14:
            quatCamFile( inFile);
            break;
         default:
            std::cerr << "ERROR : Unknown Type: " << id << std::endl;
            exit ( 1 );
      }
   }

   vrxprConfigFiles( inFile );
   this->numGeoms = files.size();
   guiVal = NULL;
}

//read the user specified params for the transient data 
void cfdReadParam::getTransientInfo( std::ifstream &inFile )
{
   // how many directories contain transient vtk data?
   int numTransientSets;
   inFile >> numTransientSets;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   int ii = transientInfo.size();
   this->transientInfo.push_back( new cfdTransientInfo() );

   vprDEBUG(vprDBG_ALL,0) << " transient DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   float scale[3], trans[3], rotate[3];   // pfDCS stuff
   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   pfDCS * dcs = new pfDCS;
   dcs->setScale( scale[0], scale[1], scale[2] );
   dcs->setTrans( trans[0], trans[1], trans[2] );
   dcs->setRot( rotate[0], rotate[1], rotate[2] );
   this->transientInfo[ ii ]->SetDCS( dcs );

   // read the directories...
   char ** transientDataDir = new char * [ numTransientSets ];
   for( int i = 0; i < numTransientSets; i++ )
   {
      transientDataDir[ i ] = readDirName( inFile, "transientDataDir" );
      int id = readID( inFile );
      vprDEBUG(vprDBG_ALL,0) << "\tbutton id = " << id
                             << std::endl << vprDEBUG_FLUSH;
      if ( transientDataDir[ i ] )
      {
         cfdTransientSet * cfdtransientset = new cfdTransientSet( 
                                              transientDataDir[ i ], id, dcs );
         cfdtransientset->SetParameterFile( this );
         this->transientInfo[ ii ]->LoadTransientSet( cfdtransientset );
      }
      else
      {
         vprDEBUG(vprDBG_ALL,0) << " did not find transient data directory "
                                << i << std::endl << vprDEBUG_FLUSH;
      }
   }

   char * geometryDir = readDirName( inFile, "geometryDir" );
   this->transientInfo[ ii ]->SetGeometryDir( geometryDir );

   vprDEBUG(vprDBG_ALL,0) << " transient geometry DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   pfDCS * geometryDcs = new pfDCS;
   geometryDcs->setScale( scale[0], scale[1], scale[2] );
   geometryDcs->setTrans( trans[0], trans[1], trans[2] );
   geometryDcs->setRot( rotate[0], rotate[1], rotate[2] );
   this->transientInfo[ ii ]->SetGeometryDCS( geometryDcs );

   // read geometry transparency flag
   inFile >> this->transientInfo[ ii ]->trans;
   vprDEBUG(vprDBG_ALL,0) << " trans flag = " 
                          << this->transientInfo[ ii ]->trans
                          << std::endl << vprDEBUG_FLUSH;

   // read geometry color flag and color if flag = 1
   inFile >> this->transientInfo[ ii ]->color;
   vprDEBUG(vprDBG_ALL,0) << " color flag = " 
                          << this->transientInfo[ ii ]->color
                          << std::endl << vprDEBUG_FLUSH;

   this->transientInfo[ ii ]->stlColor[ 0 ] = -1.0;
   this->transientInfo[ ii ]->stlColor[ 1 ] = -1.0;
   this->transientInfo[ ii ]->stlColor[ 2 ] = -1.0;
   if( this->transientInfo[ ii ]->color )
   {
      for(int i=0; i<3; i++)
      {
         inFile >> this->transientInfo[ ii ]->stlColor[ i ];
      }
      vprDEBUG(vprDBG_ALL,0) << "   stlColor: " 
                       << this->transientInfo[ ii ]->stlColor[ 0 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 1 ] << " : "
                       << this->transientInfo[ ii ]->stlColor[ 2 ]
                       << std::endl << vprDEBUG_FLUSH;
   }
   inFile.getline( textLine, 256 );   //skip past remainder of line
      
   double dur;
   inFile >> dur;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   this->transientInfo[ ii ]->SetDuration( dur );
   vprDEBUG(vprDBG_ALL,0) << " duration = " << dur
                          << std::endl << vprDEBUG_FLUSH;
}

void cfdReadParam::Vtk( std::ifstream &inFile )
{  
   int ii = this->dataSets.size();
   this->dataSets.push_back( new cfdDataSet() );

   vprDEBUG(vprDBG_ALL,0) << " ************************************* "
                          << std::endl << vprDEBUG_FLUSH;

   vprDEBUG(vprDBG_ALL,0) << " vtk DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;

   float scale[3], trans[3], rotate[3];   // pfDCS stuff
   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   pfDCS * dcs = new pfDCS;
   dcs->setScale( scale[0], scale[1], scale[2] );
   dcs->setTrans( trans[0], trans[1], trans[2] );
   dcs->setRot( rotate[0], rotate[1], rotate[2] );
   this->dataSets[ ii ]->SetDCS( dcs );

   // get vtk data set name...
   char vtk_filein[ 256 ];
   inFile >> vtk_filein;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if (fileIO::isFileReadable( vtk_filein ) ) 
   {
      vprDEBUG(vprDBG_ALL,0) << " vtk file = " << vtk_filein 
                             << ", dcs = "  << this->dataSets[ ii ]->GetDCS()
                             << std::endl << vprDEBUG_FLUSH;
      this->dataSets[ ii ]->SetFileName( vtk_filein );
   }
   else
   {
      std::cerr << "ERROR: unreadable vtk file = " << vtk_filein 
           << ".  You may need to correct your param file."
           << std::endl;
      exit(1);
   }

   char * precomputedDataSliceDir = readDirName( inFile, "precomputedDataSliceDir" );
   this->dataSets[ ii ]->SetPrecomputedDataSliceDir( precomputedDataSliceDir );
   delete [] precomputedDataSliceDir;
   precomputedDataSliceDir = NULL;

   char * precomputedSurfaceDir = readDirName( inFile, "precomputedSurfaceDir" );
   this->dataSets[ ii ]->SetPrecomputedSurfaceDir( precomputedSurfaceDir );
   delete [] precomputedSurfaceDir;
   precomputedSurfaceDir = NULL;

   this->LoadSurfaceFiles( this->dataSets[ ii ]->GetPrecomputedSurfaceDir() );
}  

void cfdReadParam::LoadSurfaceFiles( char * precomputedSurfaceDir )
{
    if ( precomputedSurfaceDir == NULL )
   {
      vprDEBUG(vprDBG_ALL,1) << "precomputedSurfaceDir == NULL" 
                             << std::endl << vprDEBUG_FLUSH;
      return;
   }

   vprDEBUG(vprDBG_ALL,1) << "Loading surface files from " 
      << precomputedSurfaceDir << std::endl << vprDEBUG_FLUSH;

   //store the current directory so we can change back to it
   char *cwd;
#ifndef WIN32
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit(1);
   }

   //open the directory (we already know that it is valid)

   DIR* dir = opendir( precomputedSurfaceDir );
   //change into this directory so that vtk can find the files
   chdir( precomputedSurfaceDir );
   
   //get the name of each file
   direct* file = 0;
   while( (file = readdir(dir)) != NULL )
   {
      //assume all vtk files in this directory are to be loaded
      if(strstr(file->d_name, ".vtk"))
      {
         char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(file->d_name)+2];
         strcpy(pathAndFileName,precomputedSurfaceDir);
         strcat(pathAndFileName,"/");
         strcat(pathAndFileName,file->d_name);

         if ( fileIO::isFileReadable( file->d_name ) ) 
         {
            vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
                                   << std::endl << vprDEBUG_FLUSH;

            int ii = dataSets.size();
            this->dataSets.push_back( new cfdDataSet() );
            this->dataSets[ ii ]->SetFileName( pathAndFileName );

            // set the dcs matrix the same as the last file
            this->dataSets[ ii ]->SetDCS( this->dataSets[ ii-1 ]->GetDCS() ); 

            // precomputed data that descends from a flowdata.vtk should
            // automatically have the same color mapping as the "parent" 
            this->dataSets[ ii ]->SetParent( 
                                      this->dataSets[ ii-1 ]->GetParent() );
         }
         else
         {
            std::cerr << "ERROR: unreadable file = " << pathAndFileName
                      << ".  You may need to correct your param file."
                      << std::endl;
            exit(1);
         }
      }
   };
   //close the directory
   closedir( dir );
   dir = 0;
   file = 0;

   //change back to the original directory
   chdir( cwd );
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
      return ;
   }

   // Get the proper directory path for transient files
   sprintf(directory, "%s\\*", precomputedSurfaceDir);

   //get the first file
   hList = FindFirstFile(directory, &fileData);
  
   //check to see if directory is valid
   if(hList == INVALID_HANDLE_VALUE){ 
	   std::cerr<<"No precomputed surface files found in: "<<precomputedSurfaceDir<<std::endl;
      return ;
   }else{
      // Traverse through the directory structure
      finished = FALSE;
      while (!finished){
         //add the file name to our data list
		 //assume all vtk files in this directory are part of the sequence
		 //assume all vtk files in this directory are to be loaded
         if(strstr(fileData.cFileName, ".vtk")){
            char* pathAndFileName = new char[strlen(precomputedSurfaceDir)+
                                          strlen(fileData.cFileName)+2];
            strcpy(pathAndFileName,precomputedSurfaceDir);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            if ( fileIO::isFileReadable( pathAndFileName ) ) {
               vprDEBUG(vprDBG_ALL,0) << "\tsurface file = " << pathAndFileName
                                      << std::endl << vprDEBUG_FLUSH;

               int ii = dataSets.size();
               this->dataSets.push_back( new cfdDataSet() );
               this->dataSets[ ii ]->SetFileName( pathAndFileName );

               // set the dcs matrix the same as the last file
               this->dataSets[ ii ]->SetDCS( this->dataSets[ ii-1 ]->GetDCS() ); 

               // precomputed data that descends from a flowdata.vtk should
               // automatically have the same color mapping as the "parent" 
               this->dataSets[ ii ]->SetParent( 
                                      this->dataSets[ ii-1 ]->GetParent() );
			}else{
               std::cerr << "ERROR: unreadable file = " << pathAndFileName
                         << ".  You may need to correct your param file."
                         << std::endl;
               exit(1);
			}
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

}

void cfdReadParam::Stl( std::ifstream &inFile )
{
   files.push_back( new fileInfo() );
   int ii = this->files.size() - 1;

   inFile >> this->files[ ii ]->trans;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " geometry transparency flag = "
                          << this->files[ ii ]->trans
                          << std::endl << vprDEBUG_FLUSH;

   // read color flag
   inFile >> this->files[ ii ]->color;
   vprDEBUG(vprDBG_ALL,0) << " stl color flag = " << this->files[ ii ]->color
                          << std::endl << vprDEBUG_FLUSH;

   // read color if color flag = 1
   if( this->files[ ii ]->color )
   {
      for(int i=0;i<3;i++)
      {
         inFile >> this->files[ ii ]->stlColor[ i ];
      }
      vprDEBUG(vprDBG_ALL,0) << "\tcolor: " 
                       << this->files[ ii ]->stlColor[ 0 ] << " : "
                       << this->files[ ii ]->stlColor[ 1 ] << " : "
                       << this->files[ ii ]->stlColor[ 2 ]
                       << std::endl << vprDEBUG_FLUSH;
   }
   inFile.getline( textLine, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,0) << " geometry DCS parameters:" 
                          << std::endl << vprDEBUG_FLUSH;
   float scale[3], trans[3], rotate[3];   // pfDCS stuff
   this->read_pf_DCS_parameters( inFile, scale, trans, rotate);

   pfDCS * dcs = new pfDCS;
   dcs->setScale( scale[0], scale[1], scale[2] );
   dcs->setTrans( trans[0], trans[1], trans[2] );
   dcs->setRot( rotate[0], rotate[1], rotate[2] );
   this->files[ ii ]->dcs = dcs;

   inFile >> this->files[ ii ]->fileName;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   int test1 = fileIO::isFileReadable( this->files[ ii ]->fileName );
   if ( test1 == 1 )
   { 
      vprDEBUG(vprDBG_ALL,0) << " geometry fileName = "
                             << this->files[ ii ]->fileName
                             << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std::cerr << "ERROR: unreadable geometry file = " 
           << this->files[ ii ]->fileName 
           << ".  You may need to correct your param file." << std::endl;
      exit(1);
   }
}
void cfdReadParam::soundData( std::ifstream &inFile )
{
   soundFile = soundFile + 1;
   soundFiles.push_back( new fileInfo() );
   int size = soundFiles.size() - 1;

   //ambient
   inFile >> soundFiles[ size ]->ambient;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //retriggerable
   inFile >> soundFiles[ size ]->retriggerable;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //volume
   inFile >> soundFiles[ size ]->volume;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //pitchbend
   inFile >> soundFiles[ size ]->pitchbend;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //cutoff
   inFile >> soundFiles[ size ]->cutoff;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //PositionX
   inFile >> soundFiles[ size ]->soundPositionX;
   inFile.getline( textLine, 256 );   //skip past remainder of line   
   //PositionY
   inFile >> soundFiles[ size ]->soundPositionY;
   inFile.getline( textLine, 256 );   //skip past remainder of line   
   //PositionZ
   inFile >> soundFiles[ size ]->soundPositionZ;
   inFile.getline( textLine, 256 );   //skip past remainder of line   
   //filename
   inFile >> soundFiles[ size ]->fileName;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   //Sound name
   inFile >> soundFiles[ size ]->soundName;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if (fileIO::isFileReadable( soundFiles[ size ]->fileName ) ) 
   {
      std::cout << "   fileName= " << soundFiles[ size ]->fileName << std::endl;
   }
   else
   {
      std::cerr << "ERROR: unreadable sound file = " 
         << soundFiles[ size ]->fileName 
         << ".  You may need to correct your param file." << std::endl;
      exit(1);
   }
}

void cfdReadParam::vrxprConfigFiles( std::ifstream &inFile )
{
   inFile >> isoScale;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " Warped Contour Scaling = " << isoScale
                          << std::endl << vprDEBUG_FLUSH;

   inFile >> delta;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " Navigation Step Size = " << delta
                          << std::endl << vprDEBUG_FLUSH;

   inFile >> diameter;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " Streamline Diameter Value = " << diameter
                          << std::endl << vprDEBUG_FLUSH;
}

void cfdReadParam::readScalarBar( std::ifstream &inFile )
{
   inFile >> this->scalarBarPos[0]
          >> this->scalarBarPos[1]
          >> this->scalarBarPos[2];
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " scalarBarPos = " 
      << this->scalarBarPos[0] << " : " << this->scalarBarPos[1] << " : "
      << this->scalarBarPos[2] << std::endl << vprDEBUG_FLUSH;

   inFile >> this->scalarBarZRot;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " scalarBar_Z-Rotation = " << this->scalarBarZRot
                          << std::endl << vprDEBUG_FLUSH;

   inFile >> this->scalarBarH >> this->scalarBarW;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " scalarBar_Height = " << this->scalarBarH
                          << ", scalarBar_Width = " << this->scalarBarW
                          << std::endl << vprDEBUG_FLUSH;
}

int cfdReadParam::convertDecimalToBinary( long number) 
{
   long int org_num=0;
   long org_base=0, pos_org_num;
   long int new_num, base10_num, remainder;
   int new_base=0;
   int pow_limit, pow_test;
   int cur_term=0;

   vprDEBUG(vprDBG_ALL,1) << " Number = " << number
                          << std::endl << vprDEBUG_FLUSH;
   new_base = 2;
   org_base = 10;
   pos_org_num = number;
   //Start of Conversion to Base Ten
   for(pow_test=0; pow(10.0, pow_test) <= pos_org_num; ++pow_test);
   {
      for(pow_limit=(pow_test - 1), remainder=pos_org_num, base10_num=0; pow_limit >= 0; --pow_limit)
      {
         cur_term = (int)(remainder / pow(10.0, pow_limit));
         if(cur_term >= org_base)
         {
            pow_limit=-1;
         }
         base10_num = (long int)(base10_num + (cur_term * pow((float)org_base, pow_limit)));
         remainder = (long int)(remainder - (pow(10.0, pow_limit) * cur_term));
      }
   }
   //End of Base Ten Conversion
   vprDEBUG(vprDBG_ALL,1)
      << "The conversion of "<<org_num<<" from base "
      << org_base<<" to base "<<new_base<<" reads as follows:"
      << std::endl << vprDEBUG_FLUSH;
   for(pow_test=0; pow((float)new_base, pow_test) <= base10_num; ++pow_test);
   {
      if(new_base < 11)
      {
         for(pow_limit=(pow_test-1), remainder=base10_num, new_num=0; pow_limit >= 0; --pow_limit)
         {
            cur_term = (int)pow((float)new_base, pow_limit);
            new_num = new_num + (long int)(pow(10.0, pow_limit) * (remainder / cur_term));
            remainder = (long int)(remainder - ((remainder / cur_term) * cur_term));
         }
         //if(org_num < 0)
         //   new_num = new_num * -1;
         vprDEBUG(vprDBG_ALL,1) <<new_num << std::endl << vprDEBUG_FLUSH;
      }
   }
   //display the user's final, highly desired conversion solution
   vprDEBUG(vprDBG_ALL,1) <<"new_num: "<< new_num 
                           << std::endl << vprDEBUG_FLUSH;
   return new_num;
}

/*
void cfdReadParam::convertBinaryToDecimal( int org_num ) 
{
   long int org_base=0, valid_org_val, new_num, base10_num, remainder;
   int new_base=0;
   int pow_limit, pow_test;
   int cur_term=0;
     
   new_base = 10;
   org_base = 2;

   //Start of Conversion to Base Ten
   for( pow_test=0; pow(10, pow_test) <= org_num; ++pow_test );
   for( pow_limit=(pow_test - 1), remainder= org_num, base10_num=0, valid_org_val=1; pow_limit >= 0; --pow_limit )
      {
      cur_term = (remainder / pow(10, pow_limit));
      if(cur_term >= org_base)
         {
         pow_limit=-1;
         valid_org_val=0;
         }
      base10_num = (base10_num + (cur_term * pow(org_base, pow_limit)));
      remainder = (remainder - (pow(10, pow_limit) * cur_term));
      }
   vprDEBUG(vprDBG_ALL,0) <<"The conversion of "<<org_num<<" from Binary to base decimal is: " << std::endl << vprDEBUG_FLUSH;
   for(pow_test=0; pow(new_base, pow_test) <= base10_num; ++pow_test);
      for(pow_limit=(pow_test-1), remainder=base10_num, new_num=0; pow_limit >= 0; --pow_limit)
         {
         cur_term = pow(new_base, pow_limit);
         new_num = new_num + (pow(10, pow_limit) * (remainder / cur_term));
         remainder = (remainder - ((remainder / cur_term) * cur_term));
         }
   //display the user's final, highly desired conversion solution
   vprDEBUG(vprDBG_ALL,0) << new_num << std::endl << vprDEBUG_FLUSH;
}
*/

void cfdReadParam::convertBinaryToArray( int gui, int size ) 
{
   int input;
   int i;
   int val;
   
   if ( guiVal != NULL )
   {
      delete [] this->guiVal;
      this->guiVal = NULL;
   }

   this->guiVal = new int[ size ];

   //int base[2] = {10,1};
   //int base[5] = {10000,1000,100,10,1};

   //std::cout << "Enter a binary number: " << std::endl;
   //std::cin >> input;
   input = gui;
   vprDEBUG(vprDBG_ALL,1) << " Input = " << input
                          << std::endl << vprDEBUG_FLUSH;

   for ( i = 0; i < size; i++ ) 
   {
      val = (int)pow(10.0,(size - 1) - i);
      guiVal[(size - 1) - i] = input/val; 
      input = input%val;
   }
}

void cfdReadParam::BMPLoader( std::ifstream &inFile )
{  
   this->bmpFile =1;
   inFile >> this->bmpFileName;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if (fileIO::isFileReadable( this->bmpFileName ) ) 
   {
      vprDEBUG(vprDBG_ALL,0) << " BMP file = " << this->bmpFileName
                             << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std::cerr << "ERROR: unreadable BMP File = " << this->bmpFileName 
           << ".  You may need to correct your param file." << std::endl;
      exit(1);
   }

   inFile >> this->bmpPosition[ 0 ] >> this->bmpPosition[ 1 ]
          >> this->bmpPosition[ 2 ];
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " BMP Position = " << this->bmpPosition[ 0 ]
      << "\t" << this->bmpPosition[ 1 ] << "\t" <<  this->bmpPosition[ 2 ]
      << std::endl << vprDEBUG_FLUSH;

   inFile >> this->bmpOrientation;
   inFile.getline( textLine, 256 );   //skip past remainder of line
   vprDEBUG(vprDBG_ALL,0) << " BMP Orientation = " << this->bmpOrientation
      << std::endl << vprDEBUG_FLUSH;

}
void cfdReadParam::IMGReader( std::ifstream &inFile )
{  
   char temp[256];
   inFile >> basename;
   inFile.getline( temp, 256 );   //skip past remainder of line

   inFile >> frames;
   inFile >> ex_x;
   inFile >> ex_y;
   inFile >> dim;
   inFile.getline( temp, 256 );
   inFile >> origin[0];
   inFile >> origin[1];
   inFile >> origin[2];
   inFile >> spacing[0];
   inFile >> spacing[1];
   inFile >> spacing[2];
   inFile.getline( temp, 256 );

   this->read_pf_DCS_parameters( inFile, 
                        this->imageScale, this->imageTrans, this->imageRot );
}

void cfdReadParam::readWorldDCS( std::ifstream &inFile )
{  
   vprDEBUG(vprDBG_ALL,0) << " World DCS parameters:"
                          << std::endl << vprDEBUG_FLUSH;
   this->read_pf_DCS_parameters( inFile, 
                        this->worldScale, this->worldTrans, this->worldRot );

}  

void cfdReadParam::read_pf_DCS_parameters( std::ifstream &inFile,
                             float scale[3], float trans[3], float rot[3] )
{  
   int i;
   char  text[ 256 ];
   
   for(i=0;i<3;i++)
   {
      inFile >> scale[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,0) << "\tScale factors:      "
      << "\t" << scale[0] << "\t" << scale[1] << "\t" << scale[2]
      << std::endl << vprDEBUG_FLUSH;

   for(i=0;i<3;i++)
   {
      inFile >> trans[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,0) << "\tTranslation factors:"
      << "\t" << trans[0] << "\t" << trans[1] << "\t" << trans[2]
      << std::endl << vprDEBUG_FLUSH;

   for(i=0;i<3;i++)
   {
      inFile >> rot[i];
   }
   inFile.getline( text, 256 );   //skip past remainder of line

   vprDEBUG(vprDBG_ALL,0) << "\tRotation factors:   "
      << "\t" << rot[0] << "\t" << rot[1] << "\t" << rot[2]
      << std::endl << vprDEBUG_FLUSH;
}  

void cfdReadParam::SkipModuleBlock( std::ifstream &inFile, int numLines )
{
   char text[ 256 ];
   for ( int i = 0; i < numLines; i++ )
   {
      inFile.getline( text, 256 );   //skip past remainder of line      
   }
}

void cfdReadParam::quatCamFile( std::ifstream & inFile)
{
   inFile >> quatCamFileName;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if (fileIO::isFileReadable( quatCamFileName ) ) 
   {
      vprDEBUG(vprDBG_ALL,0) << " QuatCam file = " << quatCamFileName 
                             << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std::cerr << "ERROR: unreadable QuatCam file = " << quatCamFileName 
           << ".  You may need to correct your param file."
           << std::endl;
      exit(1);
   }   
}
/*
void cfdReadParam::set1DText( std::ifstream &inFile )
{
   float scale[ 3 ];
   float trans[ 3 ];
   float rot[ 3 ];

   read_pf_DCS_parameters( inFile, scale, trans, rot);

   dashBoardDCS = new pfDCS();
   dashBoardDCS->setScale( scale[0], scale[1], scale[2] );
   dashBoardDCS->setTrans( trans[0], trans[1], trans[2] );
   dashBoardDCS->setRot( rot[0], rot[1], rot[2] );

   inFile >> dashboardFilename;
   inFile.getline( textLine, 256 );   //skip past remainder of line

   if ( fileIO::isFileReadable( (char *)dashboardFilename.c_str() ) ) 
   {
      vprDEBUG(vprDBG_ALL,0) << "\tdashboard file = " << dashboardFilename
                             << std::endl << vprDEBUG_FLUSH;
   }
   else
   {
      std::cerr << "ERROR: unreadable dashboard File = " << dashboardFilename 
           << ".  You may need to correct your param file." << std::endl;
      exit(1);
   }

   int numberOfStrings = 0;
   inFile >> numberOfStrings;
   std::string text;

   for ( int i = 0; i < numberOfStrings; i++ )
   {
      textInput.push_back( new cfd1DTextInput );
      read_pf_DCS_parameters( inFile, scale, trans, rot);
      inFile >> text;
      std::cout << text << std::endl;
      inFile.getline( textLine, 256 );   //skip past remainder of line
      textInput[ i ]->SetTransforms( scale, trans, rot );
      textInput[ i ]->SetFilename( text );
      textInput[ i ]->Update();
   }   
}
*/
