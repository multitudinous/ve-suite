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
 * File:          $RCSfile: ansysReader.cpp,v $
 * Date modified: $Date: 2004-05-18 16:09:54 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 385 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include "ansysReader.h"
#include <iostream>
#include <iomanip>
#include <cmath>
#include <netinet/in.h>

#include "fileIO.h"
#include "vtkCleanUnstructuredGrid.h"

#include <vtkUnstructuredGrid.h>
#include <vtkGenericCell.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h>
#include <vtkDoubleArray.h>
#include <vtkIntArray.h>
#include <vtkPointData.h>
#include <vtkCellType.h>
#include <vtkMath.h>

// from http://www.codeproject.com/cpp/endianness.asp
#define ntohll(x) (((long long)(ntohl((int)((x << 32) >> 32))) << 32) | (unsigned int)ntohl(((int)(x >> 32)))) //By Runner

#define PRINT_WIDTH 36

#ifndef PRINT_HEADERS
#define PRINT(x)
#else
#define PRINT(x) \
   std::cout << std::setw(PRINT_WIDTH-3) << #x << " = " << x << std::endl;
#endif

ansysReader::ansysReader( char * input )
{
   ansysFileName = input;
   std::cout << "\nOpening file \"" << ansysFileName << "\"" << std::endl;

   // open file
   if((this->s1=fopen(ansysFileName,"r"))==NULL)
   {
      std::cerr << "ERROR: can't open file \"" << ansysFileName
           << "\", so exiting" << std::endl;
      exit( 1 );
   }
/* 
   double n1[3] = {0,1,0};
   double n2[3] = {1,0,0};
   double p1[3] = {1,2,0};
   double p2[3] = {2,1,0};
   double * junk = this->GetIntersectionPoint( n1, n2, p1, p2 );
   exit( 1 );
*/
   this->endian_flip = 1;

   this->integerPosition = 0;

   this->numNodes = 0;
   this->numExpandedNodes = 0;
   this->maxNumberDataSets = 0;
   this->numElems = 0;
   this->numDOF = 0;
   this->dofCode = NULL;
   this->nodeID = NULL;
   this->elemID = NULL;

   this->ptrNodalEquivalencyTable = 0;
   this->ptrElementEquivalencyTable = 0;
   this->ptrDataStepsIndexTable = 0;
   this->ptrTIM = 0;
   this->ptrLoadStepTable = 0;
   this->ptrGEO = 0;

   this->maxety = 0;
   this->ptrETY = 0;
   this->ptrREL = 0;
   this->ptrNOD = 0;
   this->ptrCSY = 0;
   this->ptrELM = 0;

   this->etysiz = 0;
   this->ptrToElemType = NULL;
   this->ptrToElemRealConstants = NULL;
   this->elemDescriptions = NULL;
   this->elemRealConstants = NULL;
   this->coordinateSystemDescriptions = NULL;
   this->nodalCoordinates = NULL;
   this->ptrElemDescriptions = NULL;
   this->ptrDataSetSolutions = NULL;
   this->ptrNSL = 0; // Nodal solutions
   this->ptrESL = 0; // Element solutions
   this->ptrEXT = 0;
   this->ptrENS = NULL;
   this->materialInElement = NULL;
   this->realConstantsForElement = NULL;
   this->coordSystemforElement = NULL;
   this->numCornerNodesInElement = NULL;
   this->cornerNodeNumbersForElement = NULL;
   this->summedFullGraphicsS1Stress = NULL;
   this->summedFullGraphicsS3Stress = NULL;
   this->summedFullGraphicsVonMisesStress = NULL;
   this->summedPowerGraphicsS1Stress = NULL;
   this->summedPowerGraphicsS3Stress = NULL;
   this->summedPowerGraphicsVonMisesStress = NULL;
   this->numContributingFullGraphicsElements = NULL;
   this->numContributingPowerGraphicsElements = NULL;

   this->ugrid = vtkUnstructuredGrid::New();
   this->pointerToMidPlaneNode = NULL;
  
   char tempText [ 256 ];
   std::cout << "\nWhat are the units for displacement (in,mm,m,etc.): " << std::endl;
   std::cin >> tempText;
   this->displacementUnits = new char [ strlen(tempText)+1 ];
   strcpy(this->displacementUnits,tempText);

   std::cout << "\nWhat are the units for stress (MPa,psi,etc): " << std::endl;
   std::cin >> tempText;
   this->stressUnits = new char [ strlen(tempText)+1 ];
   strcpy(this->stressUnits,tempText);

   this->ReadHeader();
   this->ReadRSTHeader();
   this->ReadDOFBlock();
   this->ReadNodalEquivalencyTable();
   this->ReadElementEquivalencyTable();
   this->ReadDataStepsIndexTable();
   this->ReadTimeTable();
   this->ReadGeometryTable();
   this->ReadElementTypeIndexTable();
   this->ReadRealConstantsIndexTable();
   //this->ReadCoordinateSystemsIndexTable();
   this->ReadNodalCoordinates();
   this->ReadElementDescriptionIndexTable();
}

ansysReader::~ansysReader()
{
   fclose( this->s1 );

   if ( this->dofCode )
   {
      delete [] this->dofCode;
      this->dofCode = NULL;
   }

   if ( this->nodeID )
   {
      this->nodeID->Delete();
      this->nodeID = NULL;
   }

   if ( this->elemID )
   {
      delete [] this->elemID;
      this->elemID = NULL;
   }

   if ( this->ptrDataSetSolutions )
   {
      delete [] this->ptrDataSetSolutions;
      this->ptrDataSetSolutions = NULL;
   }

   if ( this->ptrToElemType )
   {
      delete [] this->ptrToElemType;
      this->ptrToElemType = NULL;
   }

   if ( this->ptrToElemRealConstants )
   {
      delete [] this->ptrToElemRealConstants;
      this->ptrToElemRealConstants = NULL;
   }

   if ( this->elemDescriptions )
   {
      for ( int32 i = 0; i < this->maxety; i++ )
         delete [] this->elemDescriptions[ i ];

      delete [] this->elemDescriptions;
      this->elemDescriptions = NULL;
   }

   if ( this->elemRealConstants )
   {
      for ( int32 i = 0; i < this->maxrl; i++ )
         delete [] this->elemRealConstants[ i ];

      delete [] this->elemRealConstants;
      this->elemRealConstants = NULL;
   }

   if ( this->coordinateSystemDescriptions )
   {
      for ( int32 i = 0; i < this->maxcsy; i++ )
         delete [] this->coordinateSystemDescriptions[ i ];

      delete [] this->coordinateSystemDescriptions;
      this->coordinateSystemDescriptions = NULL;
   }

   if ( this->nodalCoordinates )
   {
      for ( int32 i = 0; i < this->numNodes; i++ )
         delete [] this->nodalCoordinates[ i ];

      delete [] this->nodalCoordinates;
      this->nodalCoordinates = NULL;
   }

   if ( this->ptrElemDescriptions )
   {
      delete [] this->ptrElemDescriptions;
      this->ptrElemDescriptions = NULL;
   }

   if ( this->ptrENS )
   {
      delete [] this->ptrENS;
      this->ptrENS = NULL;
   }

   if ( this->materialInElement )
   {
      delete [] this->materialInElement;
      this->materialInElement = NULL;
   }

   if ( this->realConstantsForElement )
   {
      delete [] this->realConstantsForElement;
      this->realConstantsForElement = NULL;
   }

   if ( this->coordSystemforElement )
   {
      delete [] this->coordSystemforElement;
      this->coordSystemforElement = NULL;
   }

   if ( this->numCornerNodesInElement )
   {
      delete [] this->numCornerNodesInElement;
      this->numCornerNodesInElement = NULL;
   }

   delete [] this->displacementUnits;
   delete [] this->stressUnits;

   this->pointerToMidPlaneNode->Delete();
   this->summedFullGraphicsS1Stress->Delete();
   this->summedFullGraphicsS3Stress->Delete();
   this->summedFullGraphicsVonMisesStress->Delete();
   this->summedPowerGraphicsS1Stress->Delete();
   this->summedPowerGraphicsS3Stress->Delete();
   this->summedPowerGraphicsVonMisesStress->Delete();
   this->numContributingFullGraphicsElements->Delete();
   this->numContributingPowerGraphicsElements->Delete();
}

void ansysReader::FlipEndian()
{
   if ( this->endian_flip == true )
      this->endian_flip = false;
   else
      this->endian_flip = true;
}

int32 ansysReader::ReadNthInteger( int32 & n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   int32 integer;
   if (fileIO::readNByteBlockFromFile( &integer, sizeof(int), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
      exit( 1 );
   }
   n++; //increment to next integer position
   return integer;
}

int64 ansysReader::ReadNthDoubleLong( int32 & n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   int64 value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(int64), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
      exit( 1 );
   }
   n+=2; //increment to next integer position
   return ntohll( value );
}

float ansysReader::ReadNthFloat( int32 & n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   float value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(float), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
      exit( 1 );
   }
   n++; //increment to next integer position
   return value;
}

double ansysReader::ReadNthDouble( int32 & n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   double value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(double), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
      exit( 1 );
   }
   n+=2; //increment to next integer position
   return value;
}

void ansysReader::ReadHeader()
{
#ifdef PRINT_HEADERS
   std::cout << "\nReading generic binary header" << std::endl;
#endif // PRINT_HEADERS

   // the very first number is the integer 404
   int32 itemNumber = 0;
   int32 headerSize = ReadNthInteger( itemNumber );
   if ( headerSize != 404 ) 
   {
#ifdef PRINT_HEADERS
      std::cout << "headerSize = " << headerSize 
           << " != 404, will flip endian flag" << std::endl;
#endif // PRINT_HEADERS
      this->FlipEndian();
      itemNumber = 0;
      headerSize = ReadNthInteger( itemNumber );
      if ( headerSize != 404 ) 
      {
         std::cerr << "headerSize = " << headerSize
                   << " != 404, will exit" << std::endl;
         exit( 1 );
      }
   }

   // the ANSYS header is 100 ints long
   int32 numValues = ReadNthInteger( itemNumber );
   if ( numValues != 100 ) 
   {
      std::cerr << "numValues = " << numValues << " != 100" << std::endl;
      exit( 1 );
   }

   //create and null terminate end of 4 character buffer
   char buffer4[ 5 ];
   buffer4[ 4 ] = '\0';

   int32 fileNumber = ReadNthInteger( itemNumber );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "fileNumber = " << fileNumber 
        << " where 12 = results files, 16 = db files" << std::endl;
#endif // PRINT_HEADERS

   // file format
   int32 fileFormat = ReadNthInteger( itemNumber );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "fileFormat = " << fileFormat
        << " (0=internal, 1=external)" << std::endl;
#endif // PRINT_HEADERS

   // time
   int32 time = ReadNthInteger( itemNumber );
   PRINT( time );

   // date
   int32 date = ReadNthInteger( itemNumber );
   PRINT( date );

   // units
   int32 units = ReadNthInteger( itemNumber );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "units = " << units 
        << " (0=user-defined, 1=SI, 2=CSG, 3=feet, 4=inches)" << std::endl;
#endif // PRINT_HEADERS

   long position = 0;

   itemNumber = 10;     // ANSYS release level
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "ANSYS release level = " << "\""
        << buffer4 << "\"" << std::endl;
#endif // PRINT_HEADERS

   strcpy( this->ansysVersion, buffer4 );
   if ( strcmp(this->ansysVersion," 8.1") )
   {
      std::cerr << "this->ansysVersion = " << this->ansysVersion << std::endl;
      std::cerr << "Error: This translator is only created for ansys version 8.1"/* and 9.0*/ << std::endl;
      exit( 1 );
   }

   itemNumber = 11;     // date of ANSYS release
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "date of ANSYS release = " << "\""
        << buffer4 << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 12-14 is machine identifier
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "machine identifier = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 12; itemNumber <= 14; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 15-16 is jobname
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "short form of jobname = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 15; itemNumber <= 16; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 17-18 is ANSYS product name
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "ANSYS product name = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 17; itemNumber <= 18; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 19 is ANSYS special version label
   itemNumber = 19;
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "ANSYS special version label = " << "\""
        << buffer4 << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 20-22 is username
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "username = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 20; itemNumber <= 22; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 23-25 is machine identifier
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "machine identifier = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 23; itemNumber <= 25; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   itemNumber = 27;
   // system record size
   int32 systemRecordSize = ReadNthInteger( itemNumber );
   PRINT( systemRecordSize );

   // maximum file length
   int32 maximumFileLength = ReadNthInteger( itemNumber );
   PRINT( maximumFileLength );
   //std::cout << std::setw( PRINT_WIDTH ) << "int34 maximumFileLength = " << maximumFileLength << std::endl;

   // maximum record size
   int32 maximumRecordSize = ReadNthInteger( itemNumber );
   PRINT( maximumRecordSize );

   // item number 31-38 is jobname
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "long form of jobname = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 31; itemNumber <= 38; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 41-60 is main analysis title 
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "main analysis title = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 41; itemNumber <= 60; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 61-80 is first subtitle
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "first subtitle = " << "\"";
#endif // PRINT_HEADERS
   for ( itemNumber = 61; itemNumber <= 80; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
#ifdef PRINT_HEADERS
      std::cout << buffer4;
#endif // PRINT_HEADERS
   }
#ifdef PRINT_HEADERS
   std::cout << "\"" << std::endl;
#endif // PRINT_HEADERS

   // item number 95 is split point of the file
   itemNumber = 96;
   int32 splitPoint = ReadNthInteger( itemNumber );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "split point of the file = " << splitPoint << std::endl;
#endif // PRINT_HEADERS

   // item number 97-98 is filesize at write
   itemNumber = 98;
   int64 filesize = ReadNthDoubleLong( itemNumber );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "int64 filesize at write = " << filesize << std::endl;
#endif // PRINT_HEADERS

   // the number at integer position 102 is 404
   itemNumber = 102;
   headerSize = ReadNthInteger( itemNumber );
   if ( headerSize != 404 ) 
   {
      std::cerr << "headerSize = " << headerSize << " != 404" << std::endl;
      exit( 1 );
   }

   // We have now will read past the header which includes integers
   // headerSize, numValues, headerSize
   this->integerPosition = numValues + 3;
}

void ansysReader::ReadRSTHeader()
{
   std::cout << "\nReading RST Header" << std::endl;

   // the number at the next integer position 164
   int32 blockSize_1 = ReadNthInteger( this->integerPosition );
   if ( blockSize_1 != 164 ) 
   {
      std::cerr << "blockSize = " << blockSize_1 << " != 16" << std::endl;
      exit( 1 );
   }

   // this block is 40 ints long
   int32 numValues = ReadNthInteger( this->integerPosition );
   if ( numValues != 40 ) 
   {
      std::cerr << "numValues = " << numValues << " != 40" << std::endl;
      exit( 1 );
   }

   // read all 40 integers
   int32 fun12 = ReadNthInteger( this->integerPosition );
   PRINT( fun12 );
   if ( fun12 != 12 )
   {
      std::cerr << "ERROR: fun12 != 12" << std::endl;
      exit( 1 );
   }

   int32 maxNodeNumber = ReadNthInteger( this->integerPosition );
   PRINT( maxNodeNumber );

   this->numNodes = ReadNthInteger( this->integerPosition );
   PRINT( this->numNodes );
   std::cout << "\tnumNodes = " << this->numNodes << std::endl;

   this->maxNumberDataSets = ReadNthInteger( this->integerPosition );
   PRINT( this->maxNumberDataSets );

   this->numDOF = ReadNthInteger( this->integerPosition );
   PRINT( this->numDOF );
   //std::cout << "\tnumDOF = " << this->numDOF << std::endl;

   int32 maxElemNumber = ReadNthInteger( this->integerPosition );
   PRINT( maxElemNumber );

   this->numElems = ReadNthInteger( this->integerPosition );
   PRINT( this->numElems );

   int32 analysisType = ReadNthInteger( this->integerPosition );
   PRINT( analysisType );

   int32 numDataSets = ReadNthInteger( this->integerPosition );
   PRINT( numDataSets );

   int32 ptrEndOfFile = ReadNthInteger( this->integerPosition );
   PRINT( ptrEndOfFile );

   this->ptrDataStepsIndexTable = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrDataStepsIndexTable );

   this->ptrTIM = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrTIM );

   this->ptrLoadStepTable = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrLoadStepTable );

   this->ptrElementEquivalencyTable = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrElementEquivalencyTable );

   this->ptrNodalEquivalencyTable = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrNodalEquivalencyTable );

   this->ptrGEO = ReadNthInteger( this->integerPosition );
   PRINT( this->ptrGEO );

   int32 ptrCYC = ReadNthInteger( this->integerPosition );
   PRINT( ptrCYC );

   int32 CMSflg = ReadNthInteger( this->integerPosition );
   PRINT( CMSflg );

   int32 units = ReadNthInteger( this->integerPosition );
   PRINT( units );

   int32 nSector = ReadNthInteger( this->integerPosition );
   PRINT( nSector );

   int32 csyCYC = ReadNthInteger( this->integerPosition );
   PRINT( csyCYC );

   int32 csEls = ReadNthInteger( this->integerPosition );
   PRINT( csEls );

   int64 ptrEnd8 = ReadNthDoubleLong( this->integerPosition );
   PRINT( ptrEnd8 );

   int32 fsiflag = ReadNthInteger( this->integerPosition );
   PRINT( fsiflag );

   int32 pmeth = ReadNthInteger( this->integerPosition );
   PRINT( pmeth );

   int32 nodeOffset = ReadNthInteger( this->integerPosition );
   PRINT( nodeOffset );

   int32 elemOffset = ReadNthInteger( this->integerPosition );
   PRINT( elemOffset );

   int32 nTrans = ReadNthInteger( this->integerPosition );
   PRINT( nTrans );

   int32 ptrTran = ReadNthInteger( this->integerPosition );
   PRINT( ptrTran );

   int32 kLong = ReadNthInteger( this->integerPosition );
   PRINT( kLong );

   // read  nine zeroes
   for ( int32 i=0; i < 9; i++ )
   {
      int32 zero = ReadNthInteger( this->integerPosition );
      PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( this->integerPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadDOFBlock()
{
   std::cout << "\nReading DOF block" << std::endl;

   // the number at the next integer position 16 
   int32 blockSize_1 = ReadNthInteger( this->integerPosition );
   
   // this block is numDOF ints long
   int32 numValues = ReadNthInteger( this->integerPosition );
   if ( numValues != this->numDOF ) 
   {
      std::cerr << "numValues = " << numValues << " != numDOF" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->dofCode = new int32 [ this->numDOF ];
   for ( int32 i=0; i < this->numDOF; i++ )
   {
      this->dofCode[ i ] = ReadNthInteger( this->integerPosition );
#ifdef PRINT_HEADERS
      std::cout << "\tdofCode[ " << i << " ]: " << this->dofCode[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( this->integerPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadNodalEquivalencyTable()
{
   if ( this->ptrNodalEquivalencyTable == 0 )
      return;

   std::cout << "\nReading Nodal Equivalency Table" << std::endl;

   int32 intPosition = this->ptrNodalEquivalencyTable;
   int32 blockSize_1 = ReadNthInteger( intPosition );

   // this block is numNodes ints long
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->numNodes ) 
   {
      std::cerr << "numValues = " << numValues << " != numNodes" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->nodeID = vtkIntArray::New();
   this->nodeID->SetName( "nodeIDs" );
   this->nodeID->SetNumberOfComponents( 1 );
   this->nodeID->SetNumberOfTuples( this->numNodes );

   for ( int32 i = 0; i < this->numNodes; i++ )
   {
      this->nodeID->SetTuple1( i, ReadNthInteger( intPosition ) );
#ifdef PRINT_HEADERS
      if ( i < 10 || i > (this->numNodes-10) )
      {
         std::cout << "\tnodeID[ " << i << " ]: "
                   //<< this->nodeID[ i ] << std::endl;
                   << this->nodeID->GetTuple1( i ) << std::endl;
      }
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadElementEquivalencyTable()
{
   if ( this->ptrElementEquivalencyTable == 0 )
      return;

   std::cout << "\nReading Element Equivalency Table" << std::endl;

   int32 intPosition = this->ptrElementEquivalencyTable;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   // this block is numElems ints long
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->elemID = new int32 [ this->numElems ];
   for ( int32 i = 0; i < this->numElems; i++ )
   {
      this->elemID[ i ] = ReadNthInteger( intPosition );
#ifdef PRINT_HEADERS
      if ( i < 10 || i > (this->numElems-10) )
      {
         std::cout << "\telemID[ " << i << " ]: "
                   << this->elemID[ i ] << std::endl;
      }
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadDataStepsIndexTable()
{
   if ( this->ptrDataStepsIndexTable == 0 )
      return;

   std::cout << "\nReading Data Steps Index Table" << std::endl;

   int32 intPosition = this->ptrDataStepsIndexTable;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != 2 * this->maxNumberDataSets ) 
   {
      std::cerr << "numValues = " << numValues << " != 2 * maxNumberDataSets " << std::endl;
      exit( 1 );
   }

   this->ptrDataSetSolutions = new int32 [ 2 * this->maxNumberDataSets ];
   // read all integers
   for ( int32 i = 0; i < 2 * this->maxNumberDataSets; i++ )
   {
      this->ptrDataSetSolutions [ i ] = ReadNthInteger( intPosition );

#ifdef PRINT_HEADERS
      if ( this->ptrDataSetSolutions [ i ] != 0 )
      {
         std::cout << "\tptrDataSetSolutions[ " << i << " ]: "
              << this->ptrDataSetSolutions [ i ] << std::endl;
      }
#endif // PRINT_HEADERS

   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadTimeTable()
{
   if ( this->ptrTIM == 0 )
      return;

   std::cout << "\nReading Time Table" << std::endl;

   int32 intPosition = this->ptrTIM;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != 0 ) 
   {
      std::cerr << "numValues = " << numValues << " != 0" << std::endl;
      exit( 1 );
   }

   // read all values 
   for ( int32 i = 0; i < this->maxNumberDataSets; i++ )
   {
      double value = ReadNthDouble( intPosition );

#ifdef PRINT_HEADERS
      if ( value )
         std::cout << "\tvalue[ " << i << " ]: " << value << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadGeometryTable()
{
   if ( this->ptrGEO == 0 )
      return;

   std::cout << "\nReading Geometry Table" << std::endl;

   int32 intPosition = this->ptrGEO;
   int32 blockSize_1 = ReadNthInteger( intPosition );
 
   // ansys 8.1 uses 20 values, ansys 9.0 uses 40 values
   int32 numValues = ReadNthInteger( intPosition );
   PRINT( numValues );
   if ( numValues != 20 && numValues != 40 ) 
   {
      std::cerr << "numValues = " << numValues << " must be 20 or 40" << std::endl;
      exit( 1 );
   }

   // read all integer values 
   int32 zero = ReadNthInteger( intPosition );
   PRINT( zero );
   if ( zero != 0 )
   {
      std::cerr << "ERROR: zero != 0" << std::endl;
      exit( 1 );
   }

   this->maxety = ReadNthInteger( intPosition );
   PRINT( this->maxety );

   this->maxrl = ReadNthInteger( intPosition );
   PRINT( this->maxrl );

   int32 ndnod = ReadNthInteger( intPosition );
   PRINT( ndnod );

   int32 nelm = ReadNthInteger( intPosition );
   PRINT( nelm );

   this->maxcsy = ReadNthInteger( intPosition );
   PRINT( this->maxcsy );

   this->ptrETY = ReadNthInteger( intPosition );
   PRINT( this->ptrETY );

   this->ptrREL = ReadNthInteger( intPosition );
   PRINT( this->ptrREL );

   this->ptrNOD = ReadNthInteger( intPosition );
   PRINT( this->ptrNOD );

   this->ptrCSY = ReadNthInteger( intPosition );
   PRINT( this->ptrCSY );

   this->ptrELM = ReadNthInteger( intPosition );
   PRINT( this->ptrELM );

   // read four zeroes
   for ( int32 i=0; i < 4; i++ )
   {
      int32 zero = ReadNthInteger( intPosition );
      PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }
   }

   int32 ptrMAS = ReadNthInteger( intPosition );
   PRINT( ptrMAS );

   this->csysiz = ReadNthInteger( intPosition );
   PRINT( this->csysiz );
   if ( this->csysiz != 24 )
      std::cerr << "WARNING: csysiz != 24" << std::endl;

   int32 elmsiz = ReadNthInteger( intPosition );
   PRINT( elmsiz );

   this->etysiz = ReadNthInteger( intPosition );
   PRINT( this->etysiz );

   int32 rlsiz = ReadNthInteger( intPosition );
   PRINT( rlsiz );

   if ( numValues == 40 ) 
   {
      int64 ptrETYPEL = ReadNthDoubleLong( intPosition );
      PRINT( ptrETYPEL );

      int64 ptrRELL = ReadNthDoubleLong( intPosition );
      PRINT( ptrRELL );

      int64 ptrCSYL = ReadNthDoubleLong( intPosition );
      PRINT( ptrCSYL );

      int64 ptrNODL = ReadNthDoubleLong( intPosition );
      PRINT( ptrNODL );

      int64 ptrELML = ReadNthDoubleLong( intPosition );
      PRINT( ptrELML );

      // read ten zeroedouble s
      for ( int32 i=0; i < 10; i++ )
      {
         int32 zero = ReadNthInteger( intPosition );
         PRINT( zero );
         if ( zero != 0 )
         {
            std::cerr << "ERROR: zero != 0" << std::endl;
            exit( 1 );
         }
      }
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadElementTypeIndexTable()
{
   if ( this->ptrETY == 0 )
   {
      std::cerr << "\nthis->ptrETY == 0" << std::endl;
      return;
   }

   std::cout << "\nReading Element Type Index Table" << std::endl;

   int32 intPosition = this->ptrETY;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->maxety ) 
   {
      std::cerr << "numValues = " << numValues << " != maxety" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->ptrToElemType = new int32 [ this->maxety ];
   for ( int32 i = 0; i < this->maxety; i++ )
   {
      this->ptrToElemType[ i ] = ReadNthInteger( intPosition );
#ifdef PRINT_HEADERS
      std::cout << "\tptrToElemType[ " << i << " ]: "
           << this->ptrToElemType[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   // allocate space for the element description arrays
   this->elemDescriptions = new int32 * [ this->maxety ];
   for ( int32 i = 0; i < this->maxety; i++ )
      this->elemDescriptions[ i ] = this->ReadElementTypeDescription(
                                                   this->ptrToElemType[ i ] );
}

void ansysReader::ReadRealConstantsIndexTable()
{
   if ( this->ptrREL == 0 )
   {
      std::cerr << "\nthis->ptrREL == 0" << std::endl;
      return;
   }

   std::cout << "\nReading Real Constants Index Table" << std::endl;

   int32 intPosition = this->ptrREL;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->maxrl ) 
   {
      std::cerr << "numValues = " << numValues << " != maxrl" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->ptrToElemRealConstants = new int32 [ this->maxrl ];
   for ( int32 i = 0; i < this->maxrl; i++ )
   {
      this->ptrToElemRealConstants[ i ] = ReadNthInteger( intPosition );
#ifdef PRINT_HEADERS
      std::cout << "\tptrToElemRealConstants[ " << i << " ]: "
           << this->ptrToElemRealConstants[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   // allocate space for the element description arrays
   this->elemRealConstants = new double * [ this->maxrl ];
   for ( int32 i = 0; i < this->maxrl; i++ )
   {
      this->elemRealConstants[ i ] = this->ReadElementRealConstants(
                                          this->ptrToElemRealConstants[ i ] );
   }
}

void ansysReader::ReadCoordinateSystemsIndexTable()
{
   if ( this->ptrCSY == 0 )
   {
      std::cerr << "\nthis->ptrCSY == 0" << std::endl;
      return;
   }

   std::cout << "\nReading Coordinate Systems Index Table" << std::endl;

   int32 intPosition = this->ptrCSY;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->maxcsy ) 
   {
      std::cerr << "numValues = " << numValues << " != maxcsy" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->ptrToCoordinateSystems = new int32 [ this->maxcsy ];
   for ( int32 i = 0; i < this->maxcsy; i++ )
   {
      this->ptrToCoordinateSystems[ i ] = ReadNthInteger( intPosition );
#ifdef PRINT_HEADERS
      std::cout << "\tptrToCoordinateSystems[ " << i << " ]: "
           << this->ptrToCoordinateSystems[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   // allocate space for the coord system description arrays
   this->coordinateSystemDescriptions = new double * [ this->maxcsy ];
   for ( int32 i = 0; i < this->maxcsy; i++ )
   {
      this->coordinateSystemDescriptions[ i ] = this->ReadCoordinateSystemDescription( 
                                                this->ptrToCoordinateSystems[ i ] );
   }
}

int32 * ansysReader::ReadElementTypeDescription( int32 pointer )
{
#ifdef PRINT_HEADERS
   std::cout << "\nReading Element Type Description" << std::endl;
#endif // PRINT_HEADERS

   if ( pointer == 0 ) 
   {
#ifdef PRINT_HEADERS
      std::cout << "\treturning NULL because pointer == 0" << std::endl;
#endif // PRINT_HEADERS
      return NULL;
   }

   int32 intPosition = pointer;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->etysiz ) 
   {
      std::cerr << "numValues = " << numValues << " != etysiz" << std::endl;
      exit( 1 );
   }

   // read all integers
   int32 * elemDescription = new int32 [ this->etysiz ];
   for ( int32 i = 0; i < this->etysiz; i++ )
   {
      elemDescription[ i ] = ReadNthInteger( intPosition );
      //std::cout << "\telemDescriptions[ " << i << " ]: " << elemDescriptions[ i ] << std::endl;
   }

   /* descriptions of key parameters (in one-based notation)
   item 1    : element type reference number
   item 2    : element routine number
   item 3-14 : element type option keys
   item 34   : dof
   item 61   : number of nodes
   item 94   : number of corner nodes
   */

   int32 numNodesInElement = elemDescription[ 61-1 ];
   int32 numCornerNodes = elemDescription[ 94-1 ];

#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "element type reference number = "
             << elemDescription[ 1-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "element routine number = "
             << elemDescription[ 2-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of dof/node = "
             << elemDescription[ 34-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of nodes = "
             << numNodesInElement << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of corner nodes = "
             << numCornerNodes << std::endl;
#endif // PRINT_HEADERS

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   return elemDescription;
}

double * ansysReader::ReadElementRealConstants( int32 pointer )
{
#ifdef PRINT_HEADERS
   std::cout << "\nReading Element Real Constants" << std::endl;
#endif // PRINT_HEADERS

   if ( pointer == 0 ) 
   {
#ifdef PRINT_HEADERS
      std::cout << "\treturning NULL because pointer == 0" << std::endl;
#endif // PRINT_HEADERS
      return NULL;
   }

   int32 intPosition = pointer;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = this->VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   // read all constants
   double * elemRealConstants = new double [ numValues ];
   for ( int32 i = 0; i < numValues; i++ )
   {
      elemRealConstants[ i ] = ReadNthDouble( intPosition );
#ifdef PRINT_HEADERS
      std::cout << "\telemRealConstants[ " << i << " ]: "
                << elemRealConstants[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   return elemRealConstants;
}

double * ansysReader::ReadCoordinateSystemDescription( int32 pointer )
{
#ifdef PRINT_HEADERS
   std::cout << "\nReading Coordinate System Description" << std::endl;
#endif // PRINT_HEADERS

   if ( pointer == 0 ) 
   {
#ifdef PRINT_HEADERS
      std::cout << "\treturning NULL because pointer == 0" << std::endl;
#endif // PRINT_HEADERS
      return NULL;
   }

   int32 intPosition = pointer;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = this->VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   if ( numValues != this->csysiz )
   {
      std::cerr << "Error: numValues != this->csysiz" << std::endl;
      exit( 1 );
   }

   // read all data
   double * coordinateSystemDescription = new double [ this->csysiz ];
   for ( int32 i = 0; i < this->csysiz; i++ )
   {
      coordinateSystemDescription[ i ] = ReadNthDouble( intPosition );
#ifdef PRINT_HEADERS
      std::cout << "\tcoordinateSystemDescription[ " << i << " ]: "
                << coordinateSystemDescription[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   return coordinateSystemDescription;
}
void ansysReader::ReadNodalCoordinates()
{
   if ( this->ptrNOD == 0 )
      return;

   std::cout << "\nReading Nodal Coordinates and Storing Vertices" << std::endl;

   int32 intPosition = this->ptrNOD;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   
   if ( numValues != 7 * this->numNodes ) 
   {
      std::cout << "Note: numValues = " << numValues
                << " != 7 * numNodes = " << 7 * this->numNodes<< std::endl;
      exit( 1 );
   }

   vtkPoints *vertex = vtkPoints::New();
   // allocate space for the nodal coordinate arrays
   this->nodalCoordinates = new double * [ this->numNodes ];
   for ( int32 i = 0; i < this->numNodes; i++ )
   {
      this->nodalCoordinates[ i ] =  new double [ 7 ];

      // read all values
      if ( fileIO::readNByteBlockFromFile( this->nodalCoordinates[ i ],
                             sizeof(double), 7, this->s1, this->endian_flip ) )
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << std::endl;
         exit(1);
      }

#ifdef PRINT_HEADERS
      if ( i < 10 || i > (this->numNodes-10) )
      {
         std::cout << "for i = " << i << std::endl;
         for ( int32 j = 0; j < 7; j++ )
         {  
            std::cout << "\t" << this->nodalCoordinates[ i ][ j ] << std::endl;
         }
      }
#endif // PRINT_HEADERS

      vertex->InsertPoint( (int)this->nodalCoordinates[ i ][ 0 ],
                                this->nodalCoordinates[ i ][ 1 ],
                                this->nodalCoordinates[ i ][ 2 ],
                                this->nodalCoordinates[ i ][ 3 ] );
   }
   this->ugrid->SetPoints( vertex );
   vertex->Delete();
#ifdef PRINT_HEADERS
   std::cout << "\tAfter reading in ansys-defined points, vertex array size = "
             << this->ugrid->GetNumberOfPoints() << std::endl;
#endif // PRINT_HEADERS

   // the last number is blockSize again
   int32 blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   VerifyBlock( blockSize_1, blockSize_2 );

///////////////////////////////////////////////////////////////////////
// Shell elements require the creation of extra nodes. Create an array to keep
// track of displacements of top and bottom shell nodes relative to midplane nodes
// when i is top shell node number pointerToMidPlaneNode->GetTuple1( i ) returns
// midPlane node number

   this->pointerToMidPlaneNode = vtkIntArray::New();
   // Because the ansys vertices are one-based, increase the array size by one
   this->pointerToMidPlaneNode->SetName( "pointerToMidPlaneNode" );
   this->pointerToMidPlaneNode->SetNumberOfComponents( 1 );
   this->pointerToMidPlaneNode->SetNumberOfTuples( this->numNodes + 1 );   // note: +1
   for ( int32 i = 0; i < this->numNodes+1; i++ )
      this->pointerToMidPlaneNode->SetTuple1( i, i );
}

void ansysReader::ReadElementDescriptionIndexTable()
{
   if ( this->ptrELM == 0 )
      return;

#ifdef PRINT_HEADERS
   std::cout << "\nReading Element Description Index Table" << std::endl;
#endif // PRINT_HEADERS

   int32 intPosition = this->ptrELM;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   // allocate space for the element-specific arrays
   this->ptrElemDescriptions = new int32 [ this->numElems ];
   this->materialInElement = new int32 [ this->numElems ];
   this->realConstantsForElement = new int32 [ this->numElems ];
   this->coordSystemforElement = new int32 [ this->numElems ];
   this->numCornerNodesInElement = new int32 [ this->numElems ];
   this->cornerNodeNumbersForElement = new int32 * [ this->numElems ];

   // read all values
   if ( fileIO::readNByteBlockFromFile( this->ptrElemDescriptions,
                  sizeof(int), this->numElems, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit(1);
   }

/*
#ifdef PRINT_HEADERS
   for ( int32 i = 0; i < this->numElems; i++ )
   {  
      std::cout << "\tptrElemDescriptions[ " << i << " ] = " 
           << this->ptrElemDescriptions[ i ] << std::endl;
   }
#endif // PRINT_HEADERS
*/

   // the last number is blockSize again
   int32 blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   VerifyBlock( blockSize_1, blockSize_2 );

   // now we are ready to construct the mesh
   std::cout << "\nReading Element Descriptions and Constructing the Mesh" << std::endl;
   this->ugrid->Allocate(this->numElems,this->numElems);
   for ( int32 i = 0; i < this->numElems; i++ )
   {
      this->ReadElementDescription( i, this->ptrElemDescriptions[ i ] );
   }

#ifdef PRINT_HEADERS
   for ( int32 i = 0; i < 10; i++ )
   {
      std::cout << "Elem[ " << i << " ]: material = " 
                << this->materialInElement[ i ] << std::endl;
   }
   std::cout << "..." << std::endl;
   for ( int32 i = this->numElems-10; i < this->numElems; i++ )
   {
      std::cout << "Elem[ " << i << " ]: material = " 
                << this->materialInElement[ i ] << std::endl;
   }
#endif // PRINT_HEADERS

   // number of nodes may have increased with creation of shell elements...
   this->numExpandedNodes = this->pointerToMidPlaneNode->GetNumberOfTuples() - 1;
   PRINT( this->numNodes );
   PRINT( this->numExpandedNodes );

   // Because the ansys vertices are one-based, increase the array size by one
   this->summedFullGraphicsS1Stress = vtkDoubleArray::New();
   this->summedFullGraphicsS1Stress->SetName( "summedFullGraphicsS1Stress" );
   this->summedFullGraphicsS1Stress->SetNumberOfComponents( 1 );
   this->summedFullGraphicsS1Stress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->summedFullGraphicsS3Stress = vtkDoubleArray::New();
   this->summedFullGraphicsS3Stress->SetName( "summedFullGraphicsS3Stress" );
   this->summedFullGraphicsS3Stress->SetNumberOfComponents( 1 );
   this->summedFullGraphicsS3Stress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->summedFullGraphicsVonMisesStress = vtkDoubleArray::New();
   this->summedFullGraphicsVonMisesStress->SetName( "summedFullGraphicsVonMisesStress" );
   this->summedFullGraphicsVonMisesStress->SetNumberOfComponents( 1 );
   this->summedFullGraphicsVonMisesStress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->summedPowerGraphicsS1Stress = vtkDoubleArray::New();
   this->summedPowerGraphicsS1Stress->SetName( "summedPowerGraphicsS1Stress" );
   this->summedPowerGraphicsS1Stress->SetNumberOfComponents( 1 );
   this->summedPowerGraphicsS1Stress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->summedPowerGraphicsS3Stress = vtkDoubleArray::New();
   this->summedPowerGraphicsS3Stress->SetName( "summedPowerGraphicsS3Stress" );
   this->summedPowerGraphicsS3Stress->SetNumberOfComponents( 1 );
   this->summedPowerGraphicsS3Stress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->summedPowerGraphicsVonMisesStress = vtkDoubleArray::New();
   this->summedPowerGraphicsVonMisesStress->SetName( "summedPowerGraphicsVonMisesStress" );
   this->summedPowerGraphicsVonMisesStress->SetNumberOfComponents( 1 );
   this->summedPowerGraphicsVonMisesStress->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->numContributingFullGraphicsElements = vtkIntArray::New();
   this->numContributingFullGraphicsElements->SetName( "numContributingFullGraphicsElements" );
   this->numContributingFullGraphicsElements->SetNumberOfComponents( 1 );
   this->numContributingFullGraphicsElements->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->numContributingPowerGraphicsElements = vtkIntArray::New();
   this->numContributingPowerGraphicsElements->SetName( "numContributingPowerGraphicsElements" );
   this->numContributingPowerGraphicsElements->SetNumberOfComponents( 1 );
   this->numContributingPowerGraphicsElements->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1

   this->currentDataSetSolution = 0;
   for ( int32 i = 0; i < 2 * this->maxNumberDataSets; i++ )
   {
      if ( this->ptrDataSetSolutions[ i ] == 0 )
         continue;

      this->currentDataSetSolution = i;
      std::cout << "\nWorking on Load Case "
                << this->currentDataSetSolution << std::endl;

      this->summedFullGraphicsS1Stress->FillComponent( 0, 0.0 );
      this->summedFullGraphicsS3Stress->FillComponent( 0, 0.0 );
      this->summedFullGraphicsVonMisesStress->FillComponent( 0, 0.0 );
      this->summedPowerGraphicsS1Stress->FillComponent( 0, 0.0 );
      this->summedPowerGraphicsS3Stress->FillComponent( 0, 0.0 );
      this->summedPowerGraphicsVonMisesStress->FillComponent( 0, 0.0 );
      this->numContributingFullGraphicsElements->FillComponent( 0, 0 );
      this->numContributingPowerGraphicsElements->FillComponent( 0, 0 );

      this->ReadSolutionDataHeader( this->ptrDataSetSolutions[ i ] );
      this->ReadNodalSolutions( this->ptrDataSetSolutions[ i ] );
      this->ReadElementSolutions( this->ptrDataSetSolutions[ i ] );
   }
}

void ansysReader::ReadElementDescription( int32 elemIndex, int32 pointer )
{
   if ( pointer == 0 )
      return;

   //std::cout << "\nReading Element Description" << std::endl;

   int32 intPosition = pointer;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   //PRINT( numValues );

   int32 mat = ReadNthInteger( intPosition );
   //PRINT( mat );
   this->materialInElement[ elemIndex ] = mat;

   int32 type = ReadNthInteger( intPosition );  //important
   //PRINT( type );

   int32 real = ReadNthInteger( intPosition );
   //PRINT( real );
   this->realConstantsForElement[ elemIndex ] = real;

   int32 secnum = ReadNthInteger( intPosition );
   //PRINT( secnum );

   int32 esys = ReadNthInteger( intPosition );
   //PRINT( esys );
   this->coordSystemforElement[ elemIndex ] = esys;

   int32 death = ReadNthInteger( intPosition );
   //PRINT( death );

   int32 solidm = ReadNthInteger( intPosition );
   //PRINT( solidm );

   int32 shape = ReadNthInteger( intPosition );
   //PRINT( shape );

   int32 elnum = ReadNthInteger( intPosition );
   //PRINT( elnum );

   int32 zero = ReadNthInteger( intPosition );
   //PRINT( zero );
   if ( zero != 0 )
   {
      std::cerr << "ERROR: zero != 0" << std::endl;
      exit( 1 );
   }

   int32 numNodesInElement = this->elemDescriptions[ type - 1 ][ 61-1 ];
   //PRINT( numNodesInElement );

   int32 numCornerNodes = this->elemDescriptions[ type - 1 ][ 94-1 ];
   //PRINT( numCornerNodes );

   this->numCornerNodesInElement[ elemIndex ] = numCornerNodes;
   this->cornerNodeNumbersForElement[ elemIndex ] = new int32 [ numCornerNodes ];

   // allocate space for the node IDs that define the corners of the element
   int32 * nodes = new int32 [ numNodesInElement ];

   // read the node IDs that define the element
   if ( fileIO::readNByteBlockFromFile( nodes,
                  sizeof(int), numNodesInElement, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit( 1 );
   }

   this->cornerNodeNumbersForElement[ elemIndex ] = nodes;

#ifdef PRINT_HEADERS
      if ( elemIndex == 35893 )
      {
         std::cout << "elemIndex " << elemIndex << ", nodes: ";
         for ( int i = 0; i < numCornerNodes; i++ )
         {
            std::cout << "\t" << nodes[ i ];
         }
         std::cout << std::endl;
      }
#endif // PRINT_HEADERS

   // read rest of values: usually but not always zero
   intPosition += numNodesInElement;
   for ( int32 i = 0; i < numValues - (10 + numNodesInElement); i++ )
   {  
      int32 zero = ReadNthInteger( intPosition );
      /*PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }*/
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

/////////////////////////////////////////////////////////////////////////

   vtkPoints * vertices = this->ugrid->GetPoints();

   int32 elementRoutineNumber = this->elemDescriptions[ type - 1 ][ 2-1 ];

   if ( elementRoutineNumber == 186 )     //numNodesInElement == 20 && numCornerNodes == 8 )
   {
      this->ugrid->InsertNextCell( VTK_HEXAHEDRON, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 187 )//numNodesInElement == 10 && numCornerNodes == 4 )
   {
      this->ugrid->InsertNextCell( VTK_TETRA, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 154 &&
             numNodesInElement == 8 && numCornerNodes == 4 )
   {
      //std::cout << "Inserting QUAD for elementRoutineNumber = " << elementRoutineNumber << std::endl;
      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 170 &&
             numNodesInElement == 8 && numCornerNodes == 4 )
   {
      //std::cout << "Inserting QUAD for elementRoutineNumber = " << elementRoutineNumber << std::endl;
      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 174 &&
             numNodesInElement == 8 && numCornerNodes == 4 )
   {
      //std::cout << "Inserting QUAD for elementRoutineNumber = " << elementRoutineNumber << std::endl;
      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 175 && 
             numNodesInElement == 1 && numCornerNodes == 1 )
   {
      //std::cout << "Inserting VERTEX for elementRoutineNumber = " << elementRoutineNumber << std::endl;
      this->ugrid->InsertNextCell( VTK_VERTEX, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 179 &&
             numNodesInElement == 3 && numCornerNodes == 2 )
   {
      //std::cout << "Inserting LINE for elementRoutineNumber = " << elementRoutineNumber << std::endl;
      this->ugrid->InsertNextCell( VTK_LINE, numCornerNodes, nodes );
   }
   else if ( elementRoutineNumber == 181 &&
             numNodesInElement == 4 && numCornerNodes == 4 )
   {
//#define SJK_USE_QUADS
#ifdef SJK_USE_QUADS

      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
#else
      //std::cout << "Inserting shell element (" << elementRoutineNumber << ") for elemIndex " << elemIndex << std::endl;

      double iNode[ 3 ], jNode[ 3 ], kNode[ 3 ];
      this->ugrid->GetPoints()->GetPoint( nodes[ 0 ], iNode );
      this->ugrid->GetPoints()->GetPoint( nodes[ 1 ], jNode );
      this->ugrid->GetPoints()->GetPoint( nodes[ 2 ], kNode );

      double x[ 3 ], y[ 3 ];
      for ( int i = 0; i < 3; i++ )
      {
         x[ i ] = jNode[ i ] - iNode[ i ];

         y[ i ] = kNode[ i ] - jNode[ i ];
      }

      double z[ 3 ];
      vtkMath::Cross( x, y, z );
      vtkMath::Normalize( z );

      //std::cout << "z = " << z[0] << "\t" << z[1] << "\t" << z[2] << std::endl;

      //int coordSystem = this->coordSystemforElement[ elemIndex ];

      //std::cout << "coordSystem[ " << coordSystem << " ] = "
      //          << this->coordinateSystemDescriptions[ coordSystem ][ 6+0 ] << "\t"
      //          << this->coordinateSystemDescriptions[ coordSystem ][ 6+1 ] << "\t"
      //          << this->coordinateSystemDescriptions[ coordSystem ][ 6+2 ] << std::endl;

      double thickness[ 4 ];
      int realSet = this->realConstantsForElement[ elemIndex ];
      //PRINT( realSet );
      if ( realSet < 1 || realSet > this->maxrl )
      {
         std::cerr << "Error: real constant set pointer is out of bounds"
                   << std::endl;
      }
      realSet--;  // convert to c++ zero-based array counting

      // ansys says that if shell has uniform thickness
      // it will be defined in first slot, with others zeroed out. 
      thickness[ 0 ] = this->elemRealConstants[ realSet ][ 0 ];
      if ( this->elemRealConstants[ realSet ][ 1 ] == 0. &&
           this->elemRealConstants[ realSet ][ 2 ] == 0. &&
           this->elemRealConstants[ realSet ][ 3 ] == 0. )
      {
         thickness[ 1 ] = this->elemRealConstants[ realSet ][ 0 ];
         thickness[ 2 ] = this->elemRealConstants[ realSet ][ 0 ];
         thickness[ 3 ] = this->elemRealConstants[ realSet ][ 0 ];
      }
      else
      {
         thickness[ 1 ] = this->elemRealConstants[ realSet ][ 1 ];
         thickness[ 2 ] = this->elemRealConstants[ realSet ][ 2 ];
         thickness[ 3 ] = this->elemRealConstants[ realSet ][ 3 ];
      }

#ifdef PRINT_HEADERS
      if ( thickness[ 0 ] != thickness[ 1 ] ||
           thickness[ 0 ] != thickness[ 2 ] ||
           thickness[ 0 ] != thickness[ 3 ] )
      {
         std::cout << "thickness = " << thickness[ 0 ] << "\t"
                   << thickness[ 1 ] << "\t" << thickness[ 2 ] << "\t"
                   << thickness[ 3 ] << std::endl;
      }
#endif // PRINT_HEADERS

      int * expandedNodes = new int [ 2 * numCornerNodes ];
      for ( int i = 0; i < numCornerNodes; i++ )
      {
         double midPlaneCoordinates [ 3 ];
         this->ugrid->GetPoints()->GetPoint( nodes[ i ], midPlaneCoordinates );
         double topPlaneCoordinates [ 3 ];
         for ( int j = 0; j < 3; j++ )
         {
            //if ( elemIndex == 0 ) std::cout << "midPlaneCoordinates[ " << j << " ] = " << midPlaneCoordinates[ j ] << std::endl;
            topPlaneCoordinates[ j ] = midPlaneCoordinates[ j ] + 0.5 * thickness[ i ] * z[ j ];
                  //this->coordinateSystemDescriptions[ coordSystem ][ 6+j ];
            //if ( elemIndex == 0 ) std::cout << "topPlaneCoordinates[ " << j << " ] = " << topPlaneCoordinates[ j ] << std::endl;
         }
         expandedNodes[ i ] = vertices->InsertNextPoint( topPlaneCoordinates );
         //if ( elemIndex == 0 ) std::cout << "new vertex = " << expandedNodes[ i ] << std::endl;

         this->nodeID->InsertNextTuple1( expandedNodes[ i ] ); //+ 1 );//sjk nope
         //this->nodeID->InsertTuple1( expandedNodes[ i ]-1, expandedNodes[ i ] );//nope
         //this->nodeID->InsertTuple1( expandedNodes[ i ], expandedNodes[ i ] );

         this->pointerToMidPlaneNode->InsertNextTuple1( nodes[ i ] );
      }

      for ( int i = 0; i < numCornerNodes; i++ )
      {
         double midPlaneCoordinates [ 3 ];
         this->ugrid->GetPoints()->GetPoint( nodes[ i ], midPlaneCoordinates );
         double bottomPlaneCoordinates [ 3 ];
         for ( int j = 0; j < 3; j++ )
         {
            //if ( elemIndex == 0 ) std::cout << "midPlaneCoordinates[ " << j << " ] = " << midPlaneCoordinates[ j ] << std::endl;
            bottomPlaneCoordinates[ j ] = midPlaneCoordinates[ j ] - 0.5 * thickness[ i ] * z[ j ];
                  //this->coordinateSystemDescriptions[ coordSystem ][ 6+j ];
            //if ( elemIndex == 0 ) std::cout << "bottomPlaneCoordinates[ " << j << " ] = " << bottomPlaneCoordinates[ j ] << std::endl;
         }
         expandedNodes[ numCornerNodes+i ] = vertices->InsertNextPoint( bottomPlaneCoordinates );
         //if ( elemIndex == 0 ) std::cout << "new vertex = " << expandedNodes[ numCornerNodes+i ] << std::endl;

         this->nodeID->InsertNextTuple1( expandedNodes[ numCornerNodes+i ] );//- 1 );//nope
         //this->nodeID->InsertTuple1( expandedNodes[ numCornerNodes+i ]+1, expandedNodes[ numCornerNodes+i ] );//nope
         //this->nodeID->InsertTuple1( expandedNodes[ numCornerNodes+i ], expandedNodes[ numCornerNodes+i ] );

         this->pointerToMidPlaneNode->InsertNextTuple1( nodes[ i ] );
      }

#ifdef PRINT_HEADERS
      if ( elemIndex == 0 )
      {
         std::cout << "elemIndex " << elemIndex << ", expandedNodes: ";
         for ( int i = 0; i < 2*numCornerNodes; i++ )
         {
            std::cout << "\t" << expandedNodes[ i ];
         }
         std::cout << std::endl;
      }
#endif // PRINT_HEADERS

      this->ugrid->InsertNextCell( VTK_HEXAHEDRON, 2*numCornerNodes, expandedNodes );

      // stress is expecting stress information at newly created top and bottom plane nodes
      delete [] nodes;
      nodes = NULL;

      this->cornerNodeNumbersForElement[ elemIndex ] = expandedNodes;

      this->numCornerNodesInElement[ elemIndex ] = 2 * numCornerNodes;
#endif //SJK_USE_QUADS

   }
/*
   else if ( numNodesInElement == 1 && numCornerNodes == 1 )
   {
      std::cout << "Note: Inserting VERTEX for non-implemented element type "
                << elementRoutineNumber
                << ", numNodesInElement = " << numNodesInElement
                << ", numCornerNodes = " << numCornerNodes
                << std::endl;
      this->ugrid->InsertNextCell( VTK_VERTEX, numCornerNodes, nodes );
   }
   else if ( numNodesInElement == 3 && numCornerNodes == 2 )
   {
      std::cout << "Note: Inserting LINE for non-implemented element type "
                << elementRoutineNumber
                << ", numNodesInElement = " << numNodesInElement
                << ", numCornerNodes = " << numCornerNodes
                << std::endl;
      this->ugrid->InsertNextCell( VTK_LINE, numCornerNodes, nodes );
   }
   else if ( numNodesInElement == 8 && numCornerNodes == 4 )
   {
      std::cout << "Note: Inserting QUAD for non-implemented element type "
                << elementRoutineNumber
                << ", numNodesInElement = " << numNodesInElement
                << ", numCornerNodes = " << numCornerNodes
                << std::endl;
      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
   }
*/
   else
   {
      std::cout << "Note: Can not yet handle element type "
                << elementRoutineNumber
                << ", numNodesInElement = " << numNodesInElement
                << ", numCornerNodes = " << numCornerNodes << std::endl;
      this->ugrid->InsertNextCell( VTK_EMPTY_CELL, 0, NULL );
   }

   //delete [] nodes;
}

void ansysReader::ReadSolutionDataHeader( int32 ptrDataSetSolution )
{
   std::cout << "\tReading Solution Data Header" << std::endl;

   int32 intPosition = ptrDataSetSolution;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   
   int32 numValues = ReadNthInteger( intPosition );
   if ( numValues != 100 ) 
   {
      std::cerr << "numValues = " << numValues << " != 100" << std::endl;
      exit( 1 );
   }

   int32 solnSetNumber = ReadNthInteger( intPosition );
   PRINT( solnSetNumber );
   if ( solnSetNumber != 0 ) 
   {
      std::cerr << "solnSetNumber = " << solnSetNumber << " != 0" << std::endl;
      exit( 1 );
   }

   int32 numberOfElements = ReadNthInteger( intPosition );
   PRINT( numberOfElements );
   if ( numberOfElements != this->numElems ) 
   {
      std::cerr << "numberOfElements = " << numberOfElements
           << " != numElems" << std::endl;
      exit( 1 );
   }

   int32 numberOfNodes = ReadNthInteger( intPosition );
   PRINT( numberOfNodes );
   if ( numberOfNodes != this->numNodes ) 
   {
      std::cerr << "numberOfNodes = " << numberOfNodes
                << " != numNodes" << std::endl;
      exit( 1 );
   }

   int32 bitmask = ReadNthInteger( intPosition );
   PRINT( bitmask );

   int32 itime = ReadNthInteger( intPosition );
   PRINT( itime );

   int32 iter = ReadNthInteger( intPosition );
   PRINT( iter );
   if ( iter != 1 ) 
   {
      std::cerr << "iter = " << iter << " != 1" << std::endl;
      exit( 1 );
   }

   int32 ncumit = ReadNthInteger( intPosition );
   PRINT( ncumit );

   int32 numReactionForces = ReadNthInteger( intPosition );
   PRINT( numReactionForces );

   int32 cs_LSC = ReadNthInteger( intPosition );
   PRINT( cs_LSC );
   if ( cs_LSC != 0 ) 
   {
      std::cerr << "cs_LSC = " << cs_LSC << " != 0" << std::endl;
      exit( 1 );
   }

   int32 nmast = ReadNthInteger( intPosition );
   PRINT( nmast );
   if ( nmast != 0 ) 
   {
      std::cerr << "nmast = " << nmast << " != 0" << std::endl;
      exit( 1 );
   }

   // the following pointers are relative to the pointer of this header
   //NSL = NodalSolution
   this->ptrNSL = ReadNthInteger( intPosition );
   PRINT( this->ptrNSL );

   this->ptrESL = ReadNthInteger( intPosition );
   PRINT( this->ptrESL );

   int32 ptrRF  = ReadNthInteger( intPosition );
   PRINT( ptrRF );

   int32 ptrMST = ReadNthInteger( intPosition );
   PRINT( ptrMST );

   int32 ptrBC = ReadNthInteger( intPosition );
   PRINT( ptrBC );

   int32 rxtrap = ReadNthInteger( intPosition );
   PRINT( rxtrap );

   int32 mode = ReadNthInteger( intPosition );
   PRINT( mode );

   int32 isym = ReadNthInteger( intPosition );
   PRINT( isym );

   int32 kcmplx = ReadNthInteger( intPosition );
   PRINT( kcmplx );

   int32 numdof = ReadNthInteger( intPosition );
   PRINT( numdof );
   if ( numdof != this->numDOF ) 
   {
      std::cerr << "numdof = " << numdof << " != this->numDOF" << std::endl;
      exit( 1 );
   }

   for ( int32 i = 0; i < numdof; i++ )
   {
      int32 dofRefNumber = ReadNthInteger( intPosition );
      PRINT( dofRefNumber );
   }

   for ( int32 i = 0; i < 30 - numdof; i++ )
   {
      int32 zero = ReadNthInteger( intPosition );
      if ( zero != 0 ) 
      {
         std::cerr << "zero = " << zero << " != 0" << std::endl;
         exit( 1 );
      }
   }

   for ( int32 i = 0; i < 20; i++ )
   {
      int32 title = ReadNthInteger( intPosition );
      //PRINT( title );
   }

   for ( int32 i = 0; i < 20; i++ )
   {
      int32 stitle1 = ReadNthInteger( intPosition );
      //PRINT( stitle1 );
   }

   int32 dbmtim = ReadNthInteger( intPosition );
   PRINT( dbmtim );

   int32 dbmdat = ReadNthInteger( intPosition );
   PRINT( dbmdat );

   int32 dbfncl = ReadNthInteger( intPosition );
   PRINT( dbfncl );

   int32 soltim = ReadNthInteger( intPosition );
   PRINT( soltim );

   int32 soldat = ReadNthInteger( intPosition );
   PRINT( soldat );

   int32 ptrOND = ReadNthInteger( intPosition );
   PRINT( ptrOND );

   int32 ptrOEL = ReadNthInteger( intPosition );
   PRINT( ptrOEL );

   int32 nfldof = ReadNthInteger( intPosition );
   PRINT( nfldof );

   int32 ptrEXA = ReadNthInteger( intPosition );
   PRINT( ptrEXA );

   this->ptrEXT = ReadNthInteger( intPosition );
   PRINT( this->ptrEXT );

   // the last number is blockSize again
   int32 blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadNodalSolutions( int32 ptrDataSetSolution )
{
   if ( this->ptrNSL == 0 )
      return;

   std::cout << "\tReading Nodal Solutions" << std::endl;

   int32 intPosition = ptrDataSetSolution + this->ptrNSL;
   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   
   if ( numValues != this->numDOF * this->numNodes ) 
   {
      std::cerr << "numValues = " << numValues
                << " != numDOF * numNodes" << std::endl;
      exit( 1 );
   }

   // set up arrays to store scalar and vector data over entire mesh...
   int32 numParameters = 2;
   vtkFloatArray ** parameterData = new vtkFloatArray * [ numParameters ];
   for ( int32 i=0; i < numParameters; i++ )
   {
      parameterData[ i ] = vtkFloatArray::New();
   }

   // Because the ansys vertices are one-based, increase the arrays by one
   char arrayName[ 256 ];
   sprintf( arrayName, "displacement %i %s", this->currentDataSetSolution, this->displacementUnits );
   parameterData[ 0 ]->SetName( arrayName );
   parameterData[ 0 ]->SetNumberOfComponents( 3 );
   parameterData[ 0 ]->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   sprintf( arrayName, "displacement mag %i %s", this->currentDataSetSolution, this->displacementUnits );
   parameterData[ 1 ]->SetName( arrayName );
   parameterData[ 1 ]->SetNumberOfComponents( 1 );
   parameterData[ 1 ]->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   // Read the solutions and populate the floatArrays.
   // Because the ansys vertices are one-based, up the loop by one
   double * nodalSolution = new double [ this->numDOF ];
   for ( int32 i = 0; i < this->numNodes; i++ )
   {
      if ( fileIO::readNByteBlockFromFile( nodalSolution,
                 sizeof(double), this->numDOF, this->s1, this->endian_flip ) )
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << std::endl;
         exit( 1 );
      }

      int ansysNode = this->nodeID->GetTuple1( i );
      parameterData[ 0 ]->SetTuple( ansysNode, nodalSolution );
/*
#ifdef PRINT_HEADERS
      if ( ansysNode == 2108 )    //tets
      {
         std::cout <<  "nodalSolution[ " << ansysNode <<" ] = ";
         for ( int32 j = 0; j < this->numDOF; j++ )
            std::cout << "\t" << nodalSolution[ j ];
         std::cout << std::endl;
      }
#endif // PRINT_HEADERS
*/
      // parameterData[ 1 ] is displacement magnitude scalar
      parameterData[ 1 ]->SetTuple1( ansysNode, vtkMath::Norm(nodalSolution) );

#ifdef PRINT_HEADERS
      if ( vtkMath::Norm(nodalSolution) > 1e+10 )
      {
         std::cout <<  "nodalSolution[ " << ansysNode <<" ] = "
                   << vtkMath::Norm(nodalSolution) << std::endl;
      }
#endif // PRINT_HEADERS
   }

   // Now take care of shell elements
   for ( int32 i = this->numNodes+1; i < this->numExpandedNodes+1; i++ )
   {
      int32 midPlaneNode = this->pointerToMidPlaneNode->GetTuple1( i );
      for ( int32 j = 0; j < numParameters; j++ )
      {
         parameterData[ j ]->InsertNextTuple( parameterData[ j ]->GetTuple( midPlaneNode ) );
      }
   }

   for ( int32 i=0; i < numParameters; i++ )
   {
      this->ugrid->GetPointData()->AddArray( parameterData [ i ] );
      parameterData[ i ]->Delete();
   }

   delete [] parameterData;
   
   delete [] nodalSolution;

   // the last number is blockSize again
   int32 blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadElementSolutions( int32 ptrDataSetSolution )
{
   if ( this->ptrESL == 0 )
      return;

   std::cout << "\tReading Element Solutions" << std::endl;

   int32 intPosition = ptrDataSetSolution + this->ptrESL;

   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   this->ptrENS = new int32 [ this->numElems ];

   for ( int32 elemIndex = 0; elemIndex < this->numElems; elemIndex++ )
   {
      int32 ptrElement_i = ReadNthInteger( intPosition );

      //PRINT( ptrElement_i );

      int32 ptrPosition = ptrDataSetSolution + ptrElement_i;
      ReadElementIndexTable( elemIndex, ptrPosition );
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );

   this->AttachStressToGrid();
}

void ansysReader::AttachStressToGrid()
{
   std::cout << "\tAttaching Stress To Grid for Load Case " << this->currentDataSetSolution << std::endl;

   // set up arrays to store stresses over entire mesh...
   int32 numParameters = 6;
   vtkFloatArray ** parameterData = new vtkFloatArray * [ numParameters ];
   for ( int32 i=0; i < numParameters; i++ )
   {
      parameterData[ i ] = vtkFloatArray::New();
      parameterData[ i ]->SetNumberOfComponents( 1 );
      // Because the ansys vertices are one-based, increase the arrays by one
      parameterData[ i ]->SetNumberOfTuples( this->numExpandedNodes + 1 );   // note: +1
   }

   char arrayName[ 256 ];
   sprintf( arrayName, "fg max prin stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 0 ]->SetName( arrayName );
   sprintf( arrayName, "fg min prin stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 1 ]->SetName( arrayName );
   sprintf( arrayName, "fg von Mises stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 2 ]->SetName( arrayName );
   sprintf( arrayName, "pg max prin stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 3 ]->SetName( arrayName );
   sprintf( arrayName, "pg min prin stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 4 ]->SetName( arrayName );
   sprintf( arrayName, "pg von Mises stress %i %s", this->currentDataSetSolution, this->stressUnits );
   parameterData[ 5 ]->SetName( arrayName );

   // first do full graphics calculations...
   //for ( int32 nodeIndex = 0; nodeIndex < this->numNodes; nodeIndex++ )
   for ( int32 nodeIndex = 0; nodeIndex < this->numExpandedNodes; nodeIndex++ )
   {
      int ansysNode = this->nodeID->GetTuple1( nodeIndex );

      if ( ansysNode < 1 || ansysNode > this->numExpandedNodes ) 
      {
         std::cerr << "AttachStressToGrid: ansysNode = " << ansysNode << " is out of range" << std::endl;
         exit( 1 );
      }

      double avgFullGraphicsS1Stress = 0.0;
      double avgFullGraphicsS3Stress = 0.0;
      double avgFullGraphicsVonMisesStress = 0.0;

      if ( numContributingFullGraphicsElements->GetTuple1( ansysNode ) > 0 )
      {
         avgFullGraphicsS1Stress = summedFullGraphicsS1Stress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
         avgFullGraphicsS3Stress = summedFullGraphicsS3Stress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
         avgFullGraphicsVonMisesStress = summedFullGraphicsVonMisesStress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
/*
#ifdef PRINT_HEADERS
         //if ( ansysNode == 2108 )  //tets
         //if ( ansysNode == 63637 )  //prod
         {
            std::cout << "FullGraphics: Node " << ansysNode
                      << " has average S1 Stress = " << avgFullGraphicsS1Stress
                      << " has average S3 Stress = " << avgFullGraphicsS3Stress
                      << " has average vonMisesStress = " << avgFullGraphicsVonMisesStress
                      << std::endl;
         }
#endif // PRINT_HEADERS
*/
      }
      parameterData[ 0 ]->SetTuple1( ansysNode, avgFullGraphicsS1Stress );
      parameterData[ 1 ]->SetTuple1( ansysNode, avgFullGraphicsS3Stress );
      parameterData[ 2 ]->SetTuple1( ansysNode, avgFullGraphicsVonMisesStress );
   }
/*
   // shell: reference non-unique new nodes back to midPlane nodes
   for ( int32 nodeIndex1 = this->numNodes; nodeIndex1 < this->numExpandedNodes; nodeIndex1++ )
   {
      int ansysNode1 = this->nodeID->GetTuple1( nodeIndex1 );
      if ( ansysNode1 < 1 || ansysNode1 > this->numExpandedNodes ) 
      {
         std::cerr << "sjk1: ansysNode = " << ansysNode1 << " is out of range" << std::endl;
         exit( 1 );
      }
      double vertex1[ 3 ];
      this->ugrid->GetPoints()->GetPoint( ansysNode1, vertex1 );

      for ( int32 nodeIndex2 = this->numNodes; nodeIndex2 < this->numExpandedNodes; nodeIndex2++ )
      {
         int ansysNode2 = this->nodeID->GetTuple1( nodeIndex2 );
         if ( ansysNode2 < 1 || ansysNode2 > this->numExpandedNodes ) 
         {
            std::cerr << "sjk1: ansysNode = " << ansysNode2 << " is out of range" << std::endl;
            exit( 1 );
         }
         double vertex2[ 3 ], diff[ 3 ];
         this->ugrid->GetPoints()->GetPoint( ansysNode2, vertex2 );
         for ( int i = 0; i < 3; i++ )
         {
            diff[ i ] = vertex2[ i ] - vertex1[ i ];
         }
         if ( vtkMath::Norm( diff ) > 1e-5 )
         {
         }


      }

      double avgFullGraphicsS1Stress = 0.0;
      double avgFullGraphicsS3Stress = 0.0;
      double avgFullGraphicsVonMisesStress = 0.0;

      if ( numContributingFullGraphicsElements->GetTuple1( ansysNode ) > 0 )
      {
         avgFullGraphicsS1Stress = summedFullGraphicsS1Stress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
         avgFullGraphicsS3Stress = summedFullGraphicsS3Stress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
         avgFullGraphicsVonMisesStress = summedFullGraphicsVonMisesStress->GetTuple1( ansysNode )
            / numContributingFullGraphicsElements->GetTuple1( ansysNode );
      }
      parameterData[ 0 ]->SetTuple1( ansysNode, avgFullGraphicsS1Stress );
      parameterData[ 1 ]->SetTuple1( ansysNode, avgFullGraphicsS3Stress );
      parameterData[ 2 ]->SetTuple1( ansysNode, avgFullGraphicsVonMisesStress );
   }
*/

   // now do power graphics calculations...
   for ( int32 nodeIndex = 0; nodeIndex < this->numExpandedNodes; nodeIndex++ )
   {
      int ansysNode = this->nodeID->GetTuple1( nodeIndex );

      if ( ansysNode < 1 || ansysNode > this->numExpandedNodes ) 
      {
         std::cerr << "AttachStressToGrid: ansysNode = " << ansysNode << " is out of range" << std::endl;
         exit( 1 );
      }

      double avgPowerGraphicsS1Stress = 0.0;
      double avgPowerGraphicsS3Stress = 0.0;
      double avgPowerGraphicsVonMisesStress = 0.0;

      if ( numContributingPowerGraphicsElements->GetTuple1( ansysNode ) > 0 )
      {
         avgPowerGraphicsS1Stress = summedPowerGraphicsS1Stress->GetTuple1( ansysNode )
            / numContributingPowerGraphicsElements->GetTuple1( ansysNode );
         avgPowerGraphicsS3Stress = summedPowerGraphicsS3Stress->GetTuple1( ansysNode )
            / numContributingPowerGraphicsElements->GetTuple1( ansysNode );
         avgPowerGraphicsVonMisesStress = summedPowerGraphicsVonMisesStress->GetTuple1( ansysNode )
            / numContributingPowerGraphicsElements->GetTuple1( ansysNode );
/*
#ifdef PRINT_HEADERS
         //if ( ansysNode == 2108 )  //tets
         //if ( ansysNode == 63637 )  //prod
         {
            std::cout << "PowerGraphics: Node " << ansysNode
                      << " has average S1 Stress = " << avgPowerGraphicsS1Stress
                      << " has average S3 Stress = " << avgPowerGraphicsS3Stress
                      << " has average vonMisesStress = " << avgPowerGraphicsVonMisesStress
                      << std::endl;
         }
#endif // PRINT_HEADERS
*/
      }
      parameterData[ 3 ]->SetTuple1( ansysNode, avgPowerGraphicsS1Stress );
      parameterData[ 4 ]->SetTuple1( ansysNode, avgPowerGraphicsS3Stress );
      parameterData[ 5 ]->SetTuple1( ansysNode, avgPowerGraphicsVonMisesStress );
   }

   for ( int32 i=0; i < numParameters; i++ )
   {
      this->ugrid->GetPointData()->AddArray( parameterData [ i ] );
      parameterData[ i ]->Delete();
   }
   delete [] parameterData;
}

void ansysReader::ReadElementIndexTable( int32 elemIndex, int32 intPosition )
{
   if ( intPosition == 0 )
      return;
/*
#ifdef PRINT_HEADERS
   std::cout << "\nReading Element Index Table at intPosition = "
             << intPosition << " for elementIndex " << elemIndex << std::endl;
#endif // PRINT_HEADERS
*/

   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   if ( numValues != 25 ) 
   {
      std::cerr << "numValues = " << numValues << " != 25" << std::endl;
      exit( 1 );
   }

   int32 ptrEMS = ReadNthInteger( intPosition );
   //PRINT( ptrEMS );

   int32 ptrENF = ReadNthInteger( intPosition );
   //PRINT( ptrENF );

   // pointer to Nodal Stresses
   this->ptrENS[ elemIndex ] = ReadNthInteger( intPosition );
   //PRINT( this->ptrENS[ elemIndex ] );
   //std::cout << "ptrENS[ " << elemIndex << " ] = " << this->ptrENS[ elemIndex ] << std::endl;

   this->StoreNodalStessesForThisElement( elemIndex );

/*
   // read remainder of the values...
   for ( int32 i = 0; i < 22; i++ )
   {
      int32 ptrElement_i = ReadNthInteger( intPosition );
      PRINT( ptrElement_i );
   }

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
*/
}

void ansysReader::StoreNodalStessesForThisElement( int32 elemIndex )
{
   if ( this->ptrENS[ elemIndex ] == 0 )
      return;

   if ( elemIndex >= this->ugrid->GetNumberOfCells() )
   {
      std::cerr << "elemIndex >= this->ugrid->GetNumberOfCells()" << std::endl;
      return;
   }

   // The more accurate and conservative computations use only exterior cells
   vtkGenericCell *cell = vtkGenericCell::New();
   this->ugrid->GetCell( elemIndex, cell );

   int32 thisIsExteriorCell = 0;
   vtkIdList *cellIds = vtkIdList::New();
   for ( int32 j=0; j < cell->GetNumberOfFaces(); j++ )
   {
      vtkCell * face = cell->GetFace( j );
      this->ugrid->GetCellNeighbors( elemIndex, face->PointIds, cellIds );

      if ( cellIds->GetNumberOfIds() == 0 )  // exterior faces have a zero here
      {
         thisIsExteriorCell = 1;
         break;
      }
   }

   cell->Delete();
   cellIds->Delete();

   int32 intPosition = this->ptrDataSetSolutions[ this->currentDataSetSolution ]
                     + this->ptrENS[ elemIndex ];
   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   if ( reportedNumValues != 0 )
   {
      std::cerr << "expected doubles" << std::endl;
      exit( 1 );
   }

   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
/*
   if ( this->numCornerNodesInElement[ elemIndex ] * 11 != numValues )
   {
      std::cerr << "numValues = " << numValues
                << " != numCornerNodesInElement[ i ] * 11 ="
                << (this->numCornerNodesInElement[ elemIndex ] * 11) << std::endl;
      exit( 1 );
   }
*/

   for ( int32 j = 0; j < this->numCornerNodesInElement[ elemIndex ]; j++ )
   {
      double * stresses = new double [ 11 ];
      // SX, SY, SZ, SXY, SYZ, SXZ, S1, S2, S3, SI, SIGE
      // The first 6 are the component stresses, then the 3 principal stresses
      // SI is maximum shear stress and SIGE is the von Mises stress

      if ( fileIO::readNByteBlockFromFile( stresses,
                     sizeof(double), 11, this->s1, this->endian_flip ) )
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << std::endl;
         exit( 1 );
      }

      int32 node = this->cornerNodeNumbersForElement[ elemIndex ][ j ];

/*
      if ( elemIndex == 35893 )
      {
         double vertex[ 3 ];
         this->ugrid->GetPoints()->GetPoint( node, vertex );
         std::cout << "elemIndex = " << elemIndex << ", node = " << node
                   << " at " << vertex[0] << " " << vertex[1] << " "
                   << vertex[2] << ", S1 = " << stresses [ 6 ] << std::endl;
      }
*/
/*
#ifdef PRINT_HEADERS
      //if ( node == 2108 )   //tets
      //if ( node == 63637 )  //prod
      {
         std::cout << "Ansys node " << node
                   << " on element " << this->elemID[ elemIndex ]
                   << " has stress:" << std::endl;
         for ( int32 ii = 0; ii < 11; ii++ )
         {
            std::cout << "\t" << stresses[ ii ];
         }
         std::cout << std::endl;
      }
#endif // PRINT_HEADERS
*/

      // ansys node numbering goes from 1 to numNodes
      if ( node < 1 || node > this->numExpandedNodes ) 
      {
         std::cerr << "StoreNodalStessesForThisElement: node = " << node << " is out of range" << std::endl;
         exit( 1 );
      }

      this->summedFullGraphicsS1Stress->SetTuple1( node, this->summedFullGraphicsS1Stress->GetTuple1( node ) + stresses [ 6 ] );
      this->summedFullGraphicsS3Stress->SetTuple1( node, this->summedFullGraphicsS3Stress->GetTuple1( node ) + stresses [ 8 ] );
      this->summedFullGraphicsVonMisesStress->SetTuple1( node, this->summedFullGraphicsVonMisesStress->GetTuple1( node ) + stresses [ 10 ] ); //vonMisesStress;
      this->numContributingFullGraphicsElements->SetTuple1( node, this->numContributingFullGraphicsElements->GetTuple1( node ) + 1 );

      // ansys powergraphic values
      if ( thisIsExteriorCell )
      {
         this->summedPowerGraphicsS1Stress->SetTuple1( node, this->summedPowerGraphicsS1Stress->GetTuple1( node ) + stresses [ 6 ] );
         this->summedPowerGraphicsS3Stress->SetTuple1( node, this->summedPowerGraphicsS3Stress->GetTuple1( node ) + stresses [ 8 ] );
         this->summedPowerGraphicsVonMisesStress->SetTuple1( node, this->summedPowerGraphicsVonMisesStress->GetTuple1( node ) + stresses [ 10 ] ); //vonMisesStress;
         this->numContributingPowerGraphicsElements->SetTuple1( node, this->numContributingPowerGraphicsElements->GetTuple1( node ) + 1 );
      }

      delete [] stresses;
   }
}

/*
void ansysReader::ReadHeaderExtension()
{
   if ( this->ptrEXT == 0 )
      return;

   std::cout << "\nReading Header Extension" << std::endl;

   int32 intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrEXT;

   int32 blockSize_1 = ReadNthInteger( intPosition );
   int32 reportedNumValues = ReadNthInteger( intPosition );
   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   if ( numValues != 200 )
   {
      std::cerr << "numValues = " << numValues << " != 200" << std::endl;
      exit( 1 );
   }

   // read the data
   for ( int32 i = 0; i < this->numDOF; i++ )
   {
      int32 dofRefNumber = ReadNthInteger( intPosition );
      PRINT( dofRefNumber );
   }

   for ( int32 i = 0; i < 32 - this->numDOF; i++ )
   {
      int32 zero = ReadNthInteger( intPosition );
      if ( zero != 0 ) 
      {
         std::cerr << "zero = " << zero << " != 0" << std::endl;
         exit( 1 );
      }
   }

   char dofLabel[ 32 ][ 5 ];
   for ( int32 i = 0; i < this->numDOF; i++ )
   {
      dofLabel[ i ][ 4 ] = '\0';
      fileIO::readNByteBlockFromFile( dofLabel[ i ], sizeof(char), 4,
                                      this->s1, this->endian_flip );
      PRINT( dofLabel[ i ] );
   }

   if ( strcmp(dofLabel[ 0 ],"UX  ") ||
        strcmp(dofLabel[ 1 ],"UY  ") ||
        strcmp(dofLabel[ 2 ],"UZ  ") )
   {
      std::cerr << "ERROR: unexpected dofLabels" << std::endl;
      for ( int32 i = 0; i < this->numDOF; i++ )
         std::cerr << "\tdofLabel[ " << i << " ] = \""
              << dofLabel[ i ] << "\"" << std::endl;
      exit( 1 );
   }

   for ( int32 i = 0; i < 32 - this->numDOF; i++ )
   {
      int32 zero;// = ReadNthInteger( intPosition );
      fileIO::readNByteBlockFromFile( &zero,
                  sizeof(int), 1, this->s1, this->endian_flip );
      //PRINT( zero );
      if ( zero != 0 ) 
      {
         std::cerr << "zero = " << zero << " != 0" << std::endl;
         exit( 1 );
      }
   }

   // TODO: there is more stuff here, but return for now
   return;

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   VerifyBlock( blockSize_1, blockSize_2 );
}
*/

void ansysReader::ReadGenericBlock( int32 intPosition )
{
   //std::cout << "\nReading block at intPosition = " << intPosition << std::endl;

   int32 blockSize_1 = ReadNthInteger( intPosition );
   //std::cout << "blockSize_1 = " << blockSize_1 << std::endl;

   int32 reportedNumValues = ReadNthInteger( intPosition );

   int32 numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   //std::cout << "numValues = " << numValues << std::endl;

   // read the data
   for ( int32 i = 0; i < numValues; i++ )
   {
      if ( reportedNumValues == 0 )
      {
         double value = ReadNthDouble( intPosition );
#ifdef PRINT_HEADERS
         std::cout << "\tvalue[ " << i << " ]: " << value << std::endl;
#endif // PRINT_HEADERS
      }
      else
      {
         int32 integer = ReadNthInteger( intPosition );
#ifdef PRINT_HEADERS
         std::cout << "\tinteger[ " << i << " ]: " << integer << std::endl;
#endif // PRINT_HEADERS
      }
   }
   //std::cout << "after loop, intPosition = " << intPosition << std::endl;

   // the last number is blockSize again
   int32 blockSize_2 = ReadNthInteger( intPosition );
   //std::cout << "blockSize_2 = " << blockSize_2 << std::endl;
   VerifyBlock( blockSize_1, blockSize_2 );
}

vtkUnstructuredGrid * ansysReader::GetUGrid()
{
   //return this->ugrid;

   std::cout << "\nMerging coincident points in the unstructured grid..." << std::endl;
   vtkCleanUnstructuredGrid * clean = vtkCleanUnstructuredGrid::New();
   clean->SetInput( this->ugrid );
   clean->Update();
   this->ugrid->Delete();

   vtkUnstructuredGrid * cleanedGrid = vtkUnstructuredGrid::New();
   cleanedGrid->ShallowCopy( clean->GetOutput() );
   clean->Delete();
   return cleanedGrid;
}

int32 ansysReader::VerifyNumberOfValues( int32 reportedNumValues, int32 blockSize_1 )
{
   int32 expectedNumValues;
   if ( reportedNumValues == 0 )
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   else
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(int);

   //std::cout << "reportedNumValues = " << reportedNumValues << std::endl;
   //std::cout << "expectedNumValues = " << expectedNumValues << std::endl;

   if ( reportedNumValues != 0 && reportedNumValues != expectedNumValues ) 
   {
      std::cerr << "reportedNumValues = " << reportedNumValues
                << "!= expectedNumValues = " << expectedNumValues << std::endl;
      exit( 1 );
   }
   return expectedNumValues;
}

void ansysReader::VerifyBlock( int32 blockSize_1, int32 blockSize_2 )
{
   //std::cout << "VerifyBlock: blockSize_1 = " << blockSize_1 << std::endl;
   //std::cout << "VerifyBlock: blockSize_2 = " << blockSize_2 << std::endl;
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "terminal blockSize = " << blockSize_2
                << " != expected block size = " << blockSize_1 << std::endl;
      exit( 1 );
   }
}

double * ansysReader::GetIntersectionPoint( double n1[ 3 ], double n2[ 3 ],
                                            double p1[ 3 ], double p2[ 3 ] )
{
   double d[ 3 ];
   vtkMath::Cross( n1, n2, d );

   double ** A = new double * [ 3 ];
   /*for ( int i = 0; i < 3; i++ )
      double * A[ i ] = new double [ 3 ];*/

   A[ 0 ] = n1;
   A[ 1 ] = n2;
   A[ 2 ] = d;

   double x[ 3 ];
   x[ 0 ] = p1[ 0 ]*n1[ 0 ] +  p1[ 1 ]*n1[ 1 ] + p1[ 2 ]*n1[ 2 ];
   x[ 1 ] = p2[ 0 ]*n2[ 0 ] +  p2[ 1 ]*n2[ 1 ] + p2[ 2 ]*n2[ 2 ];
   x[ 2 ] = p1[ 0 ]*d[ 0 ] +  p1[ 1 ]*d[ 1 ] + p1[ 2 ]*d[ 2 ];

   int error = vtkMath::SolveLinearSystem( A, x, 3 );
   std::cout << "intersection point = " << x[ 0 ] << "\t" << x[ 1 ] << "\t"
             << x[ 2 ] << std::endl;
   
   return x;
}

