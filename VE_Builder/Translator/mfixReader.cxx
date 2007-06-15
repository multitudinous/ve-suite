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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <sstream>

#include <vtkUnstructuredGrid.h>
#include <vtkRectilinearGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h>  // this code requires VTK4
#include <vtkPointData.h>
#include <vtkTransform.h>
#include <vtkTransformFilter.h>

#include "VE_Xplorer/Utilities/fileIO.h"        // for "StripTrailingSpaces"
#include "VE_Builder/Translator/gridConversion.h"
#include "VE_Xplorer/Utilities/readWriteVtkThings.h"
using namespace VE_Util;

vtkUnstructuredGrid * mfixReader( std::string mfixFileName, int nx, int ny, int nz,
                                  int retainEveryNthFrame, vtkTransform * transform, int debug )
{
//debug = 1;
   vtkUnstructuredGrid * uGrid = NULL;

   if ( debug ) std::cout << "mfix input file is " << mfixFileName << std::endl;

   FILE *inFile;
   if((inFile=fopen(mfixFileName.c_str(),"r"))==NULL)
   {
      std::cout << "ERROR: can't open file \"" << mfixFileName << "\", so exiting" << std::endl;
      return uGrid;
   }

   std::string ext;// = NULL;
   ext = fileIO::getExtension( mfixFileName );
   if ( debug ) std::cout << "ext = " << ext << std::endl;

   int i, j, k, ii, kk;

   std::cout << "nx=" << nx << ", ny=" << ny << ", nz=" << nz << std::endl; 
   std::cout << "requested retainEveryNthFrame = " << retainEveryNthFrame << std::endl; 

   const int recordLength = 512;

   //read the first record, a text string containg version information
   const int versionLength = 512;
   char version[versionLength+1];
   fseek(inFile,0*recordLength,SEEK_SET);
   fread(version, sizeof(char), versionLength, inFile);
   version[versionLength] = '\0';
   //std::cout << "version = \"" << version << "\"" << std::endl;
   fileIO::StripTrailingSpaces( version );
   std::cout << "version = \"" << version << "\"" << std::endl;

   //read the second record, a text string containing name and six ints
   const int nameLength = 60;
   char name[nameLength+1];
   fseek(inFile,1*recordLength,SEEK_SET);
   fread(name, sizeof(char), nameLength, inFile);
   name[nameLength] = '\0';
   //std::cout << "name = \"" << name << "\"" << std::endl;
   fileIO::StripTrailingSpaces( name );

   // initially set endian flag to false
   bool endianFlip = 0;

   // read six ints: these are dependent on the endian flag
   int month, day, year, hour, minute, second;

   if (fileIO::readNByteBlockFromFile( &month, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &day, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &year, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &hour, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &minute, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &second, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }

   if ( ! ( ( 0 < month && month < 13 ) && ( 0 < day && day < 32 ) ) )
   {
      //re-read the second record, a text string containing name and six ints
      fseek(inFile,1*recordLength,SEEK_SET);
      fread(name, sizeof(char), nameLength, inFile);
      name[nameLength] = '\0';
      //std::cout << "name = \"" << name << "\"" << std::endl;
      fileIO::StripTrailingSpaces( name );

      endianFlip = 1;

      if (fileIO::readNByteBlockFromFile( &month, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      if (fileIO::readNByteBlockFromFile( &day, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      if (fileIO::readNByteBlockFromFile( &year, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      if (fileIO::readNByteBlockFromFile( &hour, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      if (fileIO::readNByteBlockFromFile( &minute, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      if (fileIO::readNByteBlockFromFile( &second, sizeof(int), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
   }

   if ( ! ( ( 0 < month && month < 13 ) && ( 0 < day && day < 32 ) ) )
   {
      std::cerr << "ERROR: Could not read valid month and day from line 2, so exiting" << std::endl;
      return uGrid;
   }

   std::cout << "name = \"" << name << "\"" << std::endl;

   std::cout << "endianFlip = " << endianFlip << std::endl;

   std::cout << "month/day/year hour:minute:second = ";
   std::cout.fill('0');
   std::cout.width(2); 
   std::cout << month << "/";
   std::cout.width(2); 
   std::cout << day << "/" << year << " ";
   std::cout.width(2); 
   std::cout << hour << ":";
   std::cout.width(2); 
   std::cout << minute << ":";
   std::cout.width(2); 
   std::cout << second << std::endl;

   //read the third record, two ints
   fseek(inFile,2*recordLength,SEEK_SET);
   int last_rec1, num_rec1;
   if (fileIO::readNByteBlockFromFile( &last_rec1, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   if (fileIO::readNByteBlockFromFile( &num_rec1, sizeof(int), 1, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   std::cout << "last_rec1 (the line number of last record) = " << last_rec1 << std::endl;           //1813
   std::cout << "num_rec1 (number of records between each separate start of array data) = " << num_rec1 << std::endl;      //9

   //compute the number of time steps by subtracting the three header lines and dividing by number of records per array
   int numTimeSteps = (last_rec1-3-1)/num_rec1;
   std::cout << "computed number of time steps = " << numTimeSteps << std::endl;

   if ( retainEveryNthFrame < 1 ) retainEveryNthFrame = 1;  // enforce a minimum of one

/*
   if ( debug )
   {
      // double-check length of file:
      inFile.seekg(0, ios::end);
      int fileLength = inFile.tellg();
      std::cout << "fileLength/512 = " << fileLength/512 << std::endl;
   }
*/

   //make room for the array containing values for all locations for a single component
   int arraySize;
   if (nz==1) arraySize = (nx+2)*(ny+2);
   else       arraySize = (nx+2)*(ny+2)*(nz+2);

   std::cout << "computed arraySize = " << arraySize << std::endl;
   float * rawData = new float [arraySize];   // rawData contains "garbage" values at the ghost cell vertices

   std::cout << "required number of records per timestep = " << ((float)arraySize)/128.0 << std::endl;   //7.17188
   int numExtraTerms = 128 - (arraySize%128);
   std::cout << "computed numExtraTerms = " << numExtraTerms << std::endl;                                        //106
   int numRecordsPerTimeStep = (arraySize+numExtraTerms)/128;
   std::cout << "computed numRecordsPerTimeStep = " << numRecordsPerTimeStep << std::endl;                        //8

   //subtract the record that stores time and compute the number of arrays at each time step
   float floatnumComponents = (float)(num_rec1-1)/numRecordsPerTimeStep;
   std::cout << "float numComponents = " << floatnumComponents << std::endl;

   vtkFloatArray **dataArray = NULL;
   int numArrays = 0;
   int numComponents = 0;
   if      ( !ext.compare("SP1") )//strcmp(ext,"SP1") )           // Void Fraction
   {
      numArrays = 1;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("void_fraction");
   }
   else if ( !ext.compare("SP2") )//strcmp(ext,"SP2") )           // Gas Pressure, solids pressure
   {
      numArrays = 2;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("gas_pressure");
      dataArray[1]->SetName("solids_pressure");
   }
   else if ( !ext.compare("SP3") )//strcmp(ext,"SP3") )          // Gas Velocity (U_g, V_g, W_g)
   {
      numArrays = 1;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("gas_velocity");
   }
   else if ( !ext.compare("SP4") )//strcmp(ext,"SP4") )          // Solid Velocity (U_s, V_s, W_s)
   {
      numArrays = 1;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("solid_velocity");
   }
   else if ( !ext.compare("SP5") )//strcmp(ext,"SP5") )           // Solid density
   {
      numArrays = 1;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("solid_density");
   }
   else if ( !ext.compare("SP6") )//strcmp(ext,"SP6") )          // Gas and Solids temperature (T_g, T_s1, T_s2)
   {
      numArrays = 3;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("gas_temp");
      dataArray[1]->SetName("solid_1_temp");
      dataArray[2]->SetName("solid_2_temp");
   }
   else if ( !ext.compare("SP7") )//strcmp(ext,"SP7") )                // Gas and Solids mass fractions (X_g, X_s)
   {
      numArrays = 2;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("gas_mass_fraction");
      dataArray[1]->SetName("solids_mass_fraction");
   }
   else if ( !ext.compare("SP8") )//strcmp(ext,"SP8") )         // Granular temperature
   {
      numArrays = 1;
      numComponents = (int)( floatnumComponents + 0.5 ) / numArrays;
      dataArray = new vtkFloatArray * [ numArrays ];
      for ( i=0; i<numArrays; i++ )
      {
         dataArray[i] = vtkFloatArray::New();
         dataArray[i]->SetNumberOfComponents( numComponents );
         dataArray[i]->SetNumberOfTuples( nx*ny*nz );
      }
      dataArray[0]->SetName("granular_temperature");
   }
//   else if ( !strcmp(ext,"SP9") )         // User defined scalars
//      numArrays = ???;
   else
   {
      std::cout <<"\n\nERROR - Cannot deal with MFIX file extension \"" << ext << "\"\n"<< std::endl;
      return uGrid;
   }

   std::cout << "computed numComponents per array = " << numComponents << std::endl;

   fseek(inFile,4*recordLength,SEEK_SET);
   if (fileIO::readNByteBlockFromFile( rawData, sizeof(float), arraySize, inFile, endianFlip ))
   {
      std::cerr << "ERROR: bad read of rawData in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
      return uGrid;
   }
   const float ghostValueReal = rawData[nx+2];
   std::cout << "ghostValueReal = " << ghostValueReal << std::endl;
   // The garbage values at the ghost cells are written as approximately 9.87654e+31.
   // If read in as an integer instead of a float, the value would be exactly 1956369162;
   //    const int ghostValue = 1956369162;
   //    int ghostValueInt = const_cast<int&> (ghostValue);
   //    const float ghostValueReal = reinterpret_cast<float&> (const_cast<int&> (ghostValue));

   float *scalarData = new float [nx*ny*nz];
   if ( scalarData==NULL )
   {
      std::cerr << "ERROR: can't get memory for scalarData, so exiting" << std::endl;
      exit(1);
   }

   //for lack of actual cell centers, assign cell center coordinates based on cell layout
   //this makes rectilinear grid information out of the structured points data
   vtkFloatArray *xCoords = vtkFloatArray::New();
   xCoords->SetNumberOfTuples( nx );
   for (i=0; i<nx; i++) xCoords->SetTuple1( i, (float)i );

   vtkFloatArray *yCoords = vtkFloatArray::New();
   yCoords->SetNumberOfTuples( ny );
   for (i=0; i<ny; i++) yCoords->SetTuple1( i, (float)i );

   vtkFloatArray *zCoords = vtkFloatArray::New();
   zCoords->SetNumberOfTuples( nz );
   for (i=0; i<nz; i++) zCoords->SetTuple1( i, (float)i );

   vtkRectilinearGrid *rgrid;

   float time1, time1Old = -99;
   int nstep1, nstep1Old = -99;

   // print selected files to vtk unstructured grid format
   int writtenTimeStep = 0;
   for (int tStep=0; tStep<numTimeSteps; tStep++)  // do for all timesteps
   {
      //if (tStep != 99 )
      if ( tStep % retainEveryNthFrame != 0 )
         continue;

      //read time step information
      fseek(inFile,tStep*num_rec1*recordLength + 3*recordLength,SEEK_SET);
      if (fileIO::readNByteBlockFromFile( &time1, sizeof(float), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read of time value in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      ///inFile.seekg(tStep*num_rec1*recordLength + 3*recordLength);
      //inFile.read((char *) &time1, sizeof(float));
      //if (inFile.eof()){ std::cerr << "Error reading time value" << std::endl; break; }

      if (fileIO::readNByteBlockFromFile( &nstep1, sizeof(float), 1, inFile, endianFlip ))
      {
         std::cerr << "ERROR: bad read of nstep value in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
         return uGrid;
      }
      std::cout << "tStep = " << tStep << "\ttime1 = " << time1 <<  "\tnstep1 = " << nstep1 << std::endl;
      if (time1 < time1Old  || nstep1 < nstep1Old)
      {
         std::cout << "ERROR: time1 < time1Old  || nstep1 < nstep1Old: WILL CONTINUE TO NEXT TIMESTEP" << std::endl;
         std::cerr << "ERROR: time1 < time1Old  || nstep1 < nstep1Old: WILL CONTINUE TO NEXT TIMESTEP" << std::endl;
         continue;
      }
      time1Old = time1;
      nstep1Old = nstep1;

      rgrid = vtkRectilinearGrid::New();
      rgrid->SetDimensions(nx,ny,nz);
      rgrid->SetXCoordinates(xCoords);
      rgrid->SetYCoordinates(yCoords);
      rgrid->SetZCoordinates(zCoords);

      for (int paramIndex=0; paramIndex<numArrays; paramIndex++)
      {
         for (kk=0; kk<numComponents; kk++)
         {
            //read the array for this time step
            fseek(inFile,tStep*num_rec1*recordLength + 
                         4*recordLength + 
                         (paramIndex*numComponents + kk)*numRecordsPerTimeStep*recordLength,SEEK_SET);
            if (fileIO::readNByteBlockFromFile( rawData, sizeof(float), arraySize, inFile, endianFlip ))
            {
               std::cerr << "ERROR: bad read of component " << kk 
                    << " of rawData in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
               return uGrid;
            }

            if ( debug )
            {
               std::cout << "Array " << kk << "..." << std::endl;
               for (ii=0; ii<30; ii++) 
                  std::cout << "     rawData[" << ii << "] = " << rawData[ii] << std::endl;
               std::cout << "                  ..." << std::endl;
               for (ii=arraySize-30; ii<arraySize; ii++) 
                  std::cout << "     rawData[" << ii << "] = " << rawData[ii] << std::endl;
            }

            //for (ii=0; ii<arraySize; ii++) std::cout << "     rawData[" << ii << "] = " << rawData[ii] << std::endl;

            if ( debug )
            {
               //put in as a check that nothing was in the remaining slots -- only zeroes
               float junk;
               for (ii=0; ii<numExtraTerms; ii++)
               {
                  if (fileIO::readNByteBlockFromFile( &junk, sizeof(float), 1, inFile, endianFlip ))
                  {
                     std::cerr << "ERROR: bad read of nstep value in fileIO::readNByteBlockFromFile, so exiting" << std::endl;
                     return uGrid;
                  }
                  std::cout << "junk= " << junk << std::endl;
               }
            }

            int index;
            if (nz==1) index = (nx+2);
            else       index = (nx+2)*(ny+2); //skip over ghostValues 

            int counter = 0;
            for (k=0; k<nz; k++)
            {
               if (nz==1) index += 0;
               else       index += (nx+2); //skip ones (bracketed with ghostValues)

               for (j=0; j<ny; j++)
               {
                  index++;    // skip over one line containing ghostValue

                  for (i=0; i<nx; i++) 
                     scalarData[counter++] = rawData[index++];

                  index++;    // skip over one line containing ghostValue
               }

               if (nz==1) index += 0;
               else       index += (nx+2); //skip last group of non-data (bracketed with ghostValues)

            }
            //std::cout << "ending counter = " << counter << std::endl;
            //std::cout << "ending index = " << index << std::endl;

            if ( debug )
            {
               std::cout << "scalarData for component " << kk << "..." << std::endl;
               for (ii=0; ii<30; ii++) 
                  std::cout << "scalarData[" << ii << "] = " << scalarData[ii] << std::endl;
               std::cout << "                  ..." << std::endl;
               for (ii=nx*ny*nz-30; ii<nx*ny*nz; ii++) 
                  std::cout << "scalarData[" << ii << "] = " << scalarData[ii] << std::endl;
            }

            for (ii=0; ii<nx*ny*nz; ii++) // for each tuple
            {
               if (numComponents == 1)
                  dataArray[paramIndex]->InsertTuple(ii, &scalarData[ii]);
               else
                  dataArray[paramIndex]->InsertComponent(ii, kk, scalarData[ii]);
            }
         }   //for (kk=0; kk<numComponents;

         (rgrid->GetPointData())->AddArray( dataArray[paramIndex] ); //attach data to mesh

      }   //for each parameter (sjk: not positive that this is correct looping order)

      // convertToUnstructuredGrid will supply a new unstructured grid
      uGrid = convertToUnstructuredGrid( rgrid );

      // Transformation 
      vtkTransformFilter *transFilter = vtkTransformFilter::New();
      transFilter->SetTransform( transform );
      transFilter->SetInput( uGrid );
      transFilter->Update();

      // write a vtk file
      std::ostringstream dirStringStream;
      dirStringStream << "ugrid_" << ext << "_" << writtenTimeStep << ".vtk";
      writeVtkThing( transFilter->GetOutput(), (std::string)dirStringStream.str().c_str(), 0 );   //0=ascii
      uGrid->Delete();

//      for ( i=0; i<numArrays; i++)
//         rgrid->GetPointData()->RemoveArray( i );

      writtenTimeStep++;
      transFilter->Delete();
      rgrid->Delete();
   }// for (tStep=0; tStep<numTimeSteps; tStep++)  // do for all timesteps

   // doublecheck that you are finished: should see "end of file found after reading 1 more floats"
   if (debug) fileIO::readToFileEnd( inFile );
   fclose( inFile );

   // Clean up
   xCoords->Delete();
   yCoords->Delete();
   zCoords->Delete();

   for ( i=0; i<numArrays; i++ )
      dataArray[i]->Delete();
   delete [] dataArray;    dataArray = NULL;

   delete [] rawData;      rawData = NULL;
   delete [] scalarData;   scalarData = NULL;
   //delete [] ext;

   std::cout << "returning uGrid = NULL" << std::endl;
   uGrid = NULL;
   return uGrid;
}

