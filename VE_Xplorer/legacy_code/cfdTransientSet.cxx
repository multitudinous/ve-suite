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
#include <windows.h>
#include <direct.h>
#include <wchar.h>
#include <cstdio>
#include <cstdlib>
#endif

#include "cfdTransientSet.h"
#include "cfdTransientVizHandler.h"
#include "cfdDataSet.h"
#include "fileIO.h"
#include "readWriteVtkThings.h"
#include "cfdDCS.h"

#include <vtkDataSet.h>
#include <vtkPointData.h>

#include <iostream>
#include <vector>
#include <vpr/Util/Debug.h>

cfdTransientSet::cfdTransientSet( char * dir, int id, cfdDCS * dcs )
{
   this->directory = dir;
   this->id = id;
   this->dcs = dcs;

   this->numScalars = 0;
   this->numVectors = 0;
   this->scalarName = NULL;
   //this->vectorName = NULL;
   this->min_max = NULL;
   this->activeScalar = 0;
   this->param = NULL;
   this->numFiles = 0;
   this->order = NULL;
}

cfdTransientSet::~cfdTransientSet( )
{
   vprDEBUG(vprDBG_ALL,1) << "Deleting cfdTransientSet"
                          << std::endl << vprDEBUG_FLUSH;

   int num = this->frameFileNames.size();
   for ( int i = 0; i < num; i++ )
   {
      delete [] this->frameFileNames[ i ];

      delete [] this->order;
      this->order = NULL;
   }

   if ( num > 0 )
      this->frameFileNames.clear();

   if ( this->numScalars )
   {
      for ( int i=0; i<this->numScalars; i++ )
      {
         delete [] this->scalarName[ i ];
         delete [] this->min_max[ i ];

      }
      delete [] this->scalarName;
      this->scalarName = NULL;

      delete [] this->min_max;
      this->min_max = NULL;
   }

/*
   if ( this->numVectors )
   {
      for ( int i=0; i<this->numVectors; i++ )
      {
cout << "this->vectorName[ " << i << " ] = " << this->vectorName[ i ] << endl;
         delete [] this->vectorName[ i ];
      }
      delete [] this->vectorName;
      this->vectorName = NULL;
   }
*/

/*
   // cfdTransientSet dataSets are part of the global list and are 
   // deleted in cfdReadParam
   for (int i=0; i<(int)this->dataSets.size(); i++)
   {
      vprDEBUG(vprDBG_ALL,2) << "\tcfdTransientSet:Deleting dataSet " << i 
                             << std::endl << vprDEBUG_FLUSH;
      delete this->dataSets[ i ];
   }
   this->dataSets.clear();
*/

}

// get/set this dataset's directories
char * cfdTransientSet::GetDirectory()
{
   return this->directory;
}

/*
void cfdTransientSet::SetDirectory( char * dir )
{
   this->directory = dir;
}
*/

int cfdTransientSet::GetID()
{
   return this->id;
}

/*
void cfdTransientSet::SetID( int id )
{
   this->id = id;
}
*/

cfdDCS * cfdTransientSet::GetDCS()
{
   return this->dcs;
}

/*
void cfdTransientSet::SetDCS( pfDCS * myDCS )
{
   this->dcs = myDCS;
}
*/

// get/set minMax of any droplet scalar
double * cfdTransientSet::GetMinMax( int index )
{
   return this->min_max[ index ];
}

void cfdTransientSet::SetMinMax( int index, double * minMax )
{
   this->min_max[ index ][ 0 ] = minMax[ 0 ];
   this->min_max[ index ][ 1 ] = minMax[ 1 ];
}

int cfdTransientSet::GetNumberOfScalars()
{
   return this->numScalars;
}

int cfdTransientSet::GetNumberOfVectors()
{
   return this->numVectors;
}

void cfdTransientSet::ReadScalarRanges()
{
   // Read Scalar Ranges
   char *cwd;
#ifndef WIN32
   if ((cwd = getcwd(NULL, 100)) == NULL)
   {
      std::cerr << "Couldn't get the current working directory!" << std::endl;
      exit(1);
   }

   // open the directory
   DIR* dir = opendir( this->directory );
   direct* file = 0;

   // change into this directory so that vtk can find the files
   chdir( this->directory );
   
   // count the files and record the name of each file
   while( (file = readdir(dir)) != NULL )
   {
      //assume all vtk files in this directory are part of the sequence
      if(strstr(file->d_name, ".vtk"))
      {
         char * pathAndFileName = new char[
                     strlen(this->directory) + strlen(file->d_name) + 2 ];
         strcpy(pathAndFileName,this->directory);
         strcat(pathAndFileName,"/");
         strcat(pathAndFileName,file->d_name);

         this->frameFileNames.push_back( pathAndFileName );
         vprDEBUG(vprDBG_ALL, 1) << " pathAndFileName : " 
            << pathAndFileName << std::endl << vprDEBUG_FLUSH;
         //increment the number of frames found
         this->numFiles++;
      }
   };
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
      return;
   }

   // Get the proper directory path for transient files
   sprintf(directory, "%s\\*", this->directory);

   //get the first file
   hList = FindFirstFile(directory, &fileData);
  
   //check to see if directory is valid
   if(hList == INVALID_HANDLE_VALUE){ 
      cerr<<"No transient files found in: "<<this->directory<<endl;
      return;
   }else{
      // Traverse through the directory structure
      finished = FALSE;
      while (!finished){
         //add the file name to our data list
		 //assume all vtk files in this directory are part of the sequence
		 if(strstr(fileData.cFileName, ".vtk")){
            char* pathAndFileName = new char[
                  strlen(this->directory) + strlen(fileData.cFileName) + 2 ];
            strcpy(pathAndFileName,this->directory);
            strcat(pathAndFileName,"/");
            strcat(pathAndFileName,fileData.cFileName);

            this->frameFileNames.push_back( pathAndFileName );
            vprDEBUG(vprDBG_ALL, 1) << " pathAndFileName : " 
                                    << pathAndFileName << std::endl << vprDEBUG_FLUSH;
            //increment the number of frames found
            this->numFiles++;
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
   vprDEBUG(vprDBG_ALL,0) << " Number of files in directory \"" 
      << this->directory << "\" = " << this->numFiles
      << std::endl << vprDEBUG_FLUSH;
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
   this->order = new int [ this->numFiles ];
   for (int j = 0; j < this->numFiles; j++)
   {
      int number = fileIO::extractIntegerBeforeExtension( this->frameFileNames[ j ] );
      this->order[ number ] = j;
      vprDEBUG(vprDBG_ALL,2) << "\t" << j << "\t" << number << "\t" 
         << this->frameFileNames[ j ] << std::endl << vprDEBUG_FLUSH;
   }

   // open first file that has point data and count scalars and vectors
   double minMax [ 2 ];
   int numArrays = 0;
   int parentIndex = 0;

   if ( this->param == NULL )
   {
      std::cout << "need a param!!!!!" << std::endl;
      exit(1);
   }

   iii = this->param->GetNumberOfDataSets();
   //cout << " iii = " << iii << endl;

   for ( int j = 0; j < this->numFiles; j++ )
   {
      vprDEBUG(vprDBG_ALL,0) << " For \"" << this->frameFileNames[ this->order[ j ]  ]
                             << "\"..." << std::endl << vprDEBUG_FLUSH;

      // stub out the cfdDataset for this dataset
      this->param->CreateNewDataSet();
      this->param->GetDataSet( iii+j )->LoadData( this->frameFileNames[ this->order[ j ] ] );
      this->param->GetDataSet( iii+j )->SetDCS( this->GetDCS() );
      this->param->GetDataSet( iii+j )->SetAsPartOfTransientSeries();

      if ( this->param->GetDataSet( iii+j )->GetDataSet()->GetNumberOfPoints() == 0 )
      {
         vprDEBUG(vprDBG_ALL,1) << "\tNumberOfPoints() == 0"
                                << std::endl << vprDEBUG_FLUSH;
         continue;
      }

      this->CountScalarsAndVectors( this->param->GetDataSet( iii+j )->GetDataSet(), 
                                    this->numScalars, this->numVectors );

      vprDEBUG(vprDBG_ALL,0) << "\tnumScalars = " << this->numScalars 
         << ", numVectors = " << this->numVectors
         << std::endl << vprDEBUG_FLUSH;

      // allocate space for the scalar names and ranges ...
      this->scalarName = new char * [ this->numScalars ];
      this->min_max = new double * [ this->numScalars ];

      // copy scalar names and ranges from this dataset...
      int ii = 0;
      numArrays = this->param->GetDataSet( iii+j )->GetDataSet()->GetPointData()
                                                  ->GetNumberOfArrays();
      for (int i = 0; i < numArrays; i++)
      {
         vtkDataArray * array_i = this->param->GetDataSet( iii+j )->GetDataSet()
                                      ->GetPointData()->GetArray(i);
         if ( array_i->GetNumberOfComponents() != 1 ) 
            continue;

         this->scalarName[ ii ] = new char [strlen( array_i->GetName() )+1];
         strcpy( this->scalarName[ ii ], array_i->GetName() );

         // use this dataset to initialize min and max range values
         array_i->GetRange( minMax );
         vprDEBUG(vprDBG_ALL,1) << "\tarray " << i << " (scalar): " 
            << array_i->GetName() << "\t"  << minMax[0] << "\t" << minMax[1]
            << std::endl << vprDEBUG_FLUSH;

         this->min_max[ ii ] = new double [ 2 ];
         this->min_max[ ii ][ 0 ] = minMax[ 0 ];
         this->min_max[ ii ][ 1 ] = minMax[ 1 ];

         ii++;
      }
      parentIndex = j;
      break;
   }

   if ( this->numScalars == 0 )
   {
      vprDEBUG(vprDBG_ALL,0) << " this transient set has no scalar data"
                             << std::endl << vprDEBUG_FLUSH;
      this->frameFileNames.clear();
#ifndef WIN32
      closedir(dir);
#endif
      return;
   }

   vprDEBUG(vprDBG_ALL,1) << " parentIndex = " << parentIndex
                          << std::endl << vprDEBUG_FLUSH;

   // for the first datasets that have no points, set its "parent" dataset...
   for (int j = 0; j < parentIndex; j++)
   {
//cout << "for first datasets that have no points, set parent dataset" << endl;
      this->param->GetDataSet( iii+j )->SetParent( 
                                    this->param->GetDataSet( iii+parentIndex ) );
   }

   // look at remaining datasets to find global min and max range values
   for (int j = parentIndex+1; j < this->numFiles; j++)
   {
      vprDEBUG(vprDBG_ALL,0) << " For \"" << this->frameFileNames[ this->order[ j ]  ]
                             << "\"..." << std::endl << vprDEBUG_FLUSH;

      // stub out the cfdDatasets for these "children" datasets...
      this->param->CreateNewDataSet();
      this->param->GetDataSet( iii+j )->LoadData( this->frameFileNames[ this->order[ j ] ] );
      this->param->GetDataSet( iii+j )->SetDCS( this->GetDCS() );
      this->param->GetDataSet( iii+j )->SetAsPartOfTransientSeries();

      // set its "parent" dataset...
      this->param->GetDataSet( iii+j )->SetParent(
                                  this->param->GetDataSet( iii + parentIndex ) );

      int index = 0;
      for (int i = 0; i < numArrays; i++)
      {
         vtkDataArray * array_i = this->param->GetDataSet( iii + j )->GetDataSet()
                                      ->GetPointData()->GetArray( i );

         if ( array_i->GetNumberOfComponents() == 3 )
         {
            // ignore arrays of normals...
            if ( ! strcmp( array_i->GetName(), "normals" ) )
               continue; 

            // ignore other vectors...
            vprDEBUG(vprDBG_ALL,1) << "\tarray " << i << " (vector): " 
               << array_i->GetName() << std::endl << vprDEBUG_FLUSH;
            continue; 
         }
         else if ( array_i->GetNumberOfComponents() != 1 ) 
            continue;

         array_i->GetRange( minMax );
         vprDEBUG(vprDBG_ALL,1) << "\tarray " << i << " (scalar): " 
            << array_i->GetName() << "\t"  << minMax[0] << "\t" << minMax[1]
            << std::endl << vprDEBUG_FLUSH;

         if ( this->min_max[ index ][ 0 ] > minMax[ 0 ] )
            this->min_max[ index ][ 0 ] = minMax[ 0 ];

         if ( this->min_max[ index ][ 1 ] < minMax[ 1 ] )
            this->min_max[ index ][ 1 ] = minMax[ 1 ];

         index++;
      }
   }
   this->frameFileNames.clear();
   //close the directory
#ifndef WIN32
   closedir(dir);

   dir = 0;
   file = 0;
#endif
   for (int i = 0; i < this->numScalars; i++)
   {
      // overide scalar ranges in the "parent" dataset
      vprDEBUG(vprDBG_ALL,0) << " overide parent scalar " << i
         << " (" << this->scalarName[ i ] << ") range with " 
         << this->min_max[ i ][ 0 ] << " : " << this->min_max[ i ][ 1 ]
         << std::endl << vprDEBUG_FLUSH;

      this->param->GetDataSet( iii + parentIndex )
                 ->SetActualScalarRange( i, this->min_max[ i ] );
/*
      // double check follows...
      double range[2];
      this->param->GetDataSet( iii+parentIndex )->GetActualScalarRange( i, range );

      vprDEBUG(vprDBG_ALL,0) << " new parent scalar " << i << " range: " 
         << this->scalarName[ i ] << " :\t"  
         << range[ 0 ] << " : " << range[ 1 ]
         << std::endl << vprDEBUG_FLUSH;
*/
   }

   // now that all datasets are read in, doublecheck that type is correct...
   RecheckDatasetTypes();
}

int cfdTransientSet::GetNumberOfDataSets()
{
   return this->numFiles;
}

void cfdTransientSet::CountScalarsAndVectors( vtkDataSet * dataset, 
                                         int & numScalars, int & numVectors )
{
   numScalars = 0;
   numVectors = 0;

   //std::cout << "dataset->GetPointData() = " << dataset->GetPointData() << std::endl;
   int numArrays = dataset->GetPointData()->GetNumberOfArrays();
   //std::cout << "numArrays = " << numArrays << std::endl;
   for (int i = 0; i < numArrays; i++)
   {
      vtkDataArray * array_i = dataset->GetPointData()->GetArray(i);
      if ( array_i->GetNumberOfComponents() == 3 )
      {
         // ignore arrays of normals...
         if ( ! strcmp( array_i->GetName(), "normals" ) )
            continue; 

         numVectors++; 
         continue; 
      }
      else if ( array_i->GetNumberOfComponents() != 1 ) 
         continue;

      numScalars++; 
   }
   vprDEBUG(vprDBG_ALL,1)
      << "numScalars = " << numScalars << ", numVectors = " << numVectors
      << std::endl << vprDEBUG_FLUSH;
}

void cfdTransientSet::SetParameterFile( cfdTransientVizHandler *param )
{
   this->param = param;
}

void cfdTransientSet::RecheckDatasetTypes()
{
   // type 0 = unstructuredGrid
   // type 1 = polydata with only vertex data
   // type 2 = general polydata

   // We assume that all datasets in a transientSet should be of same type
   // But, a vertex file with no data will default to "general polydata"
   // count the number of datasets of each type
   int numDataSetsOfType[ 3 ];
   numDataSetsOfType[ 0 ] = 0;
   numDataSetsOfType[ 1 ] = 0;
   numDataSetsOfType[ 2 ] = 0;
   for ( int j = 0; j < this->numFiles; j++ )
   {  
      int type = this->param->GetDataSet( iii + j )->GetType();
      if ( 0 <= type && type < 3 )
         numDataSetsOfType[ type ]++;
   }
   vprDEBUG(vprDBG_ALL,0)
      << " numDataSetsOfType[ 0 ] = " << numDataSetsOfType[ 0 ]
      << ", numDataSetsOfType[ 1 ] = " << numDataSetsOfType[ 1 ]
      << ", numDataSetsOfType[ 2 ] = " << numDataSetsOfType[ 2 ]
      << std::endl << vprDEBUG_FLUSH;

   // if there are both type 1 and type 2 sets, see if the type 2 sets are empty
   if ( numDataSetsOfType[ 1 ] > 0 && numDataSetsOfType[ 2 ] > 0 )
   {
      for ( int j = 0; j < this->numFiles; j++ )
      {  
         if ( this->param->GetDataSet( iii + j )->GetType() == 2 &&
              this->param->GetDataSet( iii + j )->GetDataSet()
                                                ->GetNumberOfPoints() == 0 )
         {
            this->param->GetDataSet( iii + j )->SetType( 1 );
         }
      }
   }
}

