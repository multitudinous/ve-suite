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
#include "fileIO.h"
#include "converter.h"      // for "letUsersAddParamsToField"

#include <vtkUnstructuredGrid.h>
#include <vtkPoints.h>
#include <vtkFloatArray.h>  // this code requires VTK4
#include <vtkPointData.h>
#include <vtkCellType.h>

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
   
   this->endian_flip = 1;

   this->integerPosition = 0;

   this->numNodes = 0;
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
   this->ptrMAS = 0;

   this->etysiz = 0;
   this->ptrToElemType = NULL;
   this->elemDescriptions = NULL;
   this->nodalCoordinates = NULL;
   this->ptrElemDescriptions = NULL;
   this->ptrDataSetSolutions = NULL;
   this->ptrEXT = 0;

   this->ugrid = vtkUnstructuredGrid::New();
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
      delete [] this->nodeID;
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

   if ( this->elemDescriptions )
   {
      for ( int i = 0; i < this->maxety; i++ )
         delete [] this->elemDescriptions[ i ];

      delete [] this->elemDescriptions;
      this->elemDescriptions = NULL;
   }

   if ( this->nodalCoordinates )
   {
      for ( int i = 0; i < this->numNodes; i++ )
         delete [] this->nodalCoordinates[ i ];

      delete [] this->nodalCoordinates;
      this->nodalCoordinates = NULL;
   }

   if ( this->ptrElemDescriptions )
   {
      delete [] this->ptrElemDescriptions;
      this->ptrElemDescriptions = NULL;
   }

/*
   // can't do this with translateToVtk
   if ( this->ugrid )
   {
      this->ugrid->Delete();
      this->ugrid = NULL;
   }
*/
}

void ansysReader::FlipEndian()
{
   if ( this->endian_flip == true )
      this->endian_flip = false;
   else
      this->endian_flip = true;
}

int ansysReader::ReadNthInteger( int n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   int integer;
   if (fileIO::readNByteBlockFromFile( &integer, sizeof(int), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << std::endl;
      exit( 1 );
   }
   return integer;
}

long ansysReader::ReadNthLong( int n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   long value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(long), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << std::endl;
      exit( 1 );
   }
   return value;
}

float ansysReader::ReadNthFloat( int n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   float value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(float), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << std::endl;
      exit( 1 );
   }
   return value;
}

double ansysReader::ReadNthDouble( int n )
{
   long currentPosition = n * sizeof(int);
   fseek(this->s1,currentPosition,SEEK_SET);
   double value;
   if (fileIO::readNByteBlockFromFile( &value, sizeof(double), 1,
                                       this->s1, this->endian_flip ))
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << std::endl;
      exit( 1 );
   }
   return value;
}

void ansysReader::ReadHeader()
{
   std::cout << "\nReading generic binary header" << std::endl;

   // the very first number is the integer 404
   int headerSize = ReadNthInteger( 0 );
   if ( headerSize != 404 ) 
   {
      std::cerr << "headerSize = " << headerSize 
           << " != 404, will flip endian flag" << std::endl;
      this->FlipEndian();
      headerSize = ReadNthInteger( 0 );
      if ( headerSize != 404 ) 
      {
         std::cerr << "headerSize = " << headerSize << " != 404, will exit" << std::endl;
         exit( 1 );
      }
   }

   // the ANSYS header is 100 ints long
   int numValues = ReadNthInteger( 1 );
   if ( numValues != 100 ) 
   {
      std::cerr << "numValues = " << numValues << " != 100" << std::endl;
      exit( 1 );
   }

   //create and null terminate end of 4 character buffer
   char buffer4[ 5 ];
   buffer4[ 4 ] = '\0';

   int itemNumber = 1;   // get ready to get the first item: fileNumber
   int fileNumber = ReadNthInteger( itemNumber+1 );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "fileNumber = " << fileNumber 
        << " where 12 = results files, 16 = db files" << std::endl;
#endif // PRINT_HEADERS

   itemNumber = 2;      // file format
   int fileFormat = ReadNthInteger( itemNumber+1 );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "fileFormat = " << fileFormat
        << " (0=internal, 1=external)" << std::endl;
#endif // PRINT_HEADERS

   itemNumber = 3;      // time
   int time = ReadNthInteger( itemNumber+1 );
   PRINT( time );

   itemNumber = 4;      // date
   int date = ReadNthInteger( itemNumber+1 );
   PRINT( date );

   itemNumber = 5;      // units
   int units = ReadNthInteger( itemNumber+1 );
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

   itemNumber = 26;     // system record size
   int systemRecordSize = ReadNthInteger( itemNumber+1 );
   PRINT( systemRecordSize );

   itemNumber = 27;     // maximum file length
   int maximumFileLength = ReadNthInteger( itemNumber+1 );
   PRINT( maximumFileLength );

   itemNumber = 28;     // maximum record size
   int maximumRecordSize = ReadNthInteger( itemNumber+1 );
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
   itemNumber = 95;
   int splitPoint = ReadNthInteger( itemNumber+1 );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "split point of the file = " << splitPoint << std::endl;
#endif // PRINT_HEADERS

   // item number 97-98 is filesize at write
   itemNumber = 97;
   long filesize = ReadNthLong( itemNumber+1 );
#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "filesize at write = " << filesize << std::endl;
#endif // PRINT_HEADERS

   // the number at integer position 102 is 404
   headerSize = ReadNthInteger( 102 );
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
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_1 != 164 ) 
   {
      std::cerr << "blockSize = " << blockSize_1 << " != 16" << std::endl;
      exit( 1 );
   }

   // this block is 40 ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != 40 ) 
   {
      std::cerr << "numValues = " << numValues << " != 40" << std::endl;
      exit( 1 );
   }

   // read all 40 integers
   int fun12 = ReadNthInteger( this->integerPosition++ );
   PRINT( fun12 );
   if ( fun12 != 12 )
   {
      std::cerr << "ERROR: fun12 != 12" << std::endl;
      exit( 1 );
   }

   int maxNodeNumber = ReadNthInteger( this->integerPosition++ );
   PRINT( maxNodeNumber );

   this->numNodes = ReadNthInteger( this->integerPosition++ );
   PRINT( this->numNodes );

   this->maxNumberDataSets = ReadNthInteger( this->integerPosition++ );
   PRINT( this->maxNumberDataSets );

   this->numDOF = ReadNthInteger( this->integerPosition++ );
   PRINT( this->numDOF );

   int maxElemNumber = ReadNthInteger( this->integerPosition++ );
   PRINT( maxElemNumber );

   this->numElems = ReadNthInteger( this->integerPosition++ );
   PRINT( this->numElems );

   int analysisType = ReadNthInteger( this->integerPosition++ );
   PRINT( analysisType );

   int numDataSets = ReadNthInteger( this->integerPosition++ );
   PRINT( numDataSets );

   int ptrEndOfFile = ReadNthInteger( this->integerPosition++ );
   PRINT( ptrEndOfFile );

   this->ptrDataStepsIndexTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrDataStepsIndexTable );

   this->ptrTIM = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrTIM );

   this->ptrLoadStepTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrLoadStepTable );

   this->ptrElementEquivalencyTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrElementEquivalencyTable );

   this->ptrNodalEquivalencyTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrNodalEquivalencyTable );

   this->ptrGEO = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrGEO );

   int ptrCYC = ReadNthInteger( this->integerPosition++ );
   PRINT( ptrCYC );

   int CMSflg = ReadNthInteger( this->integerPosition++ );
   PRINT( CMSflg );

   int units = ReadNthInteger( this->integerPosition++ );
   PRINT( units );

   int nSector = ReadNthInteger( this->integerPosition++ );
   PRINT( nSector );

   int csyCYC = ReadNthInteger( this->integerPosition++ );
   PRINT( csyCYC );

   int csEls = ReadNthInteger( this->integerPosition++ );
   PRINT( csEls );

   int ptrEnd8 = ReadNthLong( this->integerPosition++ );
   PRINT( ptrEnd8 );
   this->integerPosition++;   // advance as long = 2 ints

   int fsiflag = ReadNthInteger( this->integerPosition++ );
   PRINT( fsiflag );

   int pmeth = ReadNthInteger( this->integerPosition++ );
   PRINT( pmeth );

   int nodeOffset = ReadNthInteger( this->integerPosition++ );
   PRINT( nodeOffset );

   int elemOffset = ReadNthInteger( this->integerPosition++ );
   PRINT( elemOffset );

   int nTrans = ReadNthInteger( this->integerPosition++ );
   PRINT( nTrans );

   int ptrTran = ReadNthInteger( this->integerPosition++ );
   PRINT( ptrTran );

   int kLong = ReadNthInteger( this->integerPosition++ );
   PRINT( kLong );

   // read  nine zeroes
   for ( int i=0; i < 9; i++ )
   {
      int zero = ReadNthInteger( this->integerPosition++ );
      PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadDOFBlock()
{
   std::cout << "\nReading DOF block" << std::endl;

   // the number at the next integer position 16 
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   
   // this block is numDOF ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != this->numDOF ) 
   {
      std::cerr << "numValues = " << numValues << " != numDOF" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->dofCode = new int [ this->numDOF ];
   for ( int i=0; i < numValues; i++ )
   {
      this->dofCode[ i ] = ReadNthInteger( this->integerPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tdofCode[ " << i << " ]: " << this->dofCode[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadNodalEquivalencyTable()
{
   if ( this->ptrNodalEquivalencyTable == 0 )
      return;

   std::cout << "\nReading Nodal Equivalency Table" << std::endl;

   int intPosition = this->ptrNodalEquivalencyTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );

   // this block is numNodes ints long
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numNodes ) 
   {
      std::cerr << "numValues = " << numValues << " != numNodes" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->nodeID = new int [ this->numNodes ];
   for ( int i = 0; i < this->numNodes; i++ )
   {
      this->nodeID[ i ] = ReadNthInteger( intPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tnodeID[ " << i << " ]: " << this->nodeID[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementEquivalencyTable()
{
   if ( this->ptrElementEquivalencyTable == 0 )
      return;

   std::cout << "\nReading Element Equivalency Table" << std::endl;

   int intPosition = this->ptrElementEquivalencyTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   // this block is numElems ints long
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->elemID = new int [ this->numElems ];
   for ( int i = 0; i < this->numElems; i++ )
   {
      this->elemID[ i ] = ReadNthInteger( intPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\telemID[ " << i << " ]: " << this->elemID[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadDataStepsIndexTable()
{
   if ( this->ptrDataStepsIndexTable == 0 )
      return;

   std::cout << "\nReading Data Steps Index Table" << std::endl;

   int intPosition = this->ptrDataStepsIndexTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 2 * this->maxNumberDataSets ) 
   {
      std::cerr << "numValues = " << numValues << " != 2 * maxNumberDataSets " << std::endl;
      exit( 1 );
   }

   this->ptrDataSetSolutions = new int [ 2 * this->maxNumberDataSets ];
   // read all integers
   for ( int i = 0; i < 2 * this->maxNumberDataSets; i++ )
   {
      this->ptrDataSetSolutions [ i ] = ReadNthInteger( intPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tptrDataSetSolutions[ " << i << " ]: "
           << this->ptrDataSetSolutions [ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadTimeTable()
{
   if ( this->ptrTIM == 0 )
      return;

   std::cout << "\nReading Time Table" << std::endl;

   int intPosition = this->ptrTIM;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 0 ) 
   {
      std::cerr << "numValues = " << numValues << " != 0" << std::endl;
      exit( 1 );
   }

   // read all values 
   for ( int i = 0; i < this->maxNumberDataSets; i++ )
   {
      double value = ReadNthDouble( intPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tvalue[ " << i << " ]: " << value << std::endl;
#endif // PRINT_HEADERS
      intPosition++;   // increase increment for double
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadGeometryTable()
{
   if ( this->ptrGEO == 0 )
      return;

   std::cout << "\nReading Geometry Table" << std::endl;

   int intPosition = this->ptrGEO;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   PRINT( numValues );
   if ( numValues != 20 && numValues != 40 ) 
   {
      std::cerr << "numValues = " << numValues << " must be 20 or 40" << std::endl;
      exit( 1 );
   }

   // read all integer values 
   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 )
   {
      std::cerr << "ERROR: zero != 0" << std::endl;
      exit( 1 );
   }

   this->maxety = ReadNthInteger( intPosition++ );
   PRINT( this->maxety );

   int maxrl = ReadNthInteger( intPosition++ );
   PRINT( maxrl );

   int ndnod = ReadNthInteger( intPosition++ );
   PRINT( ndnod );

   int nelm = ReadNthInteger( intPosition++ );
   PRINT( nelm );

   int maxcsy = ReadNthInteger( intPosition++ );
   PRINT( maxcsy );

   this->ptrETY = ReadNthInteger( intPosition++ );
   PRINT( this->ptrETY );

   this->ptrREL = ReadNthInteger( intPosition++ );
   PRINT( this->ptrREL );

   this->ptrNOD = ReadNthInteger( intPosition++ );
   PRINT( this->ptrNOD );

   this->ptrCSY = ReadNthInteger( intPosition++ );
   PRINT( this->ptrCSY );

   this->ptrELM = ReadNthInteger( intPosition++ );
   PRINT( this->ptrELM );

   // read four zeroes
   for ( int i=0; i < 4; i++ )
   {
      int zero = ReadNthInteger( intPosition++ );
      PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }
   }

   this->ptrMAS = ReadNthInteger( intPosition++ );
   PRINT( this->ptrMAS );

   int csysiz = ReadNthInteger( intPosition++ );
   PRINT( csysiz );
   if ( csysiz != 24 )
      std::cerr << "WARNING: csysiz != 24" << std::endl;

   int elmsiz = ReadNthInteger( intPosition++ );
   PRINT( elmsiz );

   this->etysiz = ReadNthInteger( intPosition++ );
   PRINT( this->etysiz );

   int rlsiz = ReadNthInteger( intPosition++ );
   PRINT( rlsiz );

   if ( numValues == 40 ) 
   {
      long ptrETYPEL = ReadNthLong( intPosition++ );
      PRINT( ptrETYPEL );
      intPosition++; // increment for long int

      long ptrRELL = ReadNthLong( intPosition++ );
      PRINT( ptrRELL );
      intPosition++; // increment for long int

      long ptrCSYL = ReadNthLong( intPosition++ );
      PRINT( ptrCSYL );
      intPosition++; // increment for long int

      long ptrNODL = ReadNthLong( intPosition++ );
      PRINT( ptrNODL );
      intPosition++; // increment for long int

      long ptrELML = ReadNthLong( intPosition++ );
      PRINT( ptrELML );
      intPosition++; // increment for long int

      // read ten zeroes
      for ( int i=0; i < 10; i++ )
      {
         int zero = ReadNthInteger( intPosition++ );
         PRINT( zero );
         if ( zero != 0 )
         {
            std::cerr << "ERROR: zero != 0" << std::endl;
            exit( 1 );
         }
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementTypeIndexTable()
{
   if ( this->ptrETY == 0 )
      return;

   std::cout << "\nReading Element Type Index Table" << std::endl;

   int intPosition = this->ptrETY;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->maxety ) 
   {
      std::cerr << "numValues = " << numValues << " != maxety" << std::endl;
      exit( 1 );
   }

   // read all integers
   this->ptrToElemType = new int [ this->maxety ];
   for ( int i = 0; i < this->maxety; i++ )
   {
      this->ptrToElemType[ i ] = ReadNthInteger( intPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tptrToElemType[ " << i << " ]: "
           << this->ptrToElemType[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }

   // allocate space for the element description arrays
   this->elemDescriptions = new int * [ this->maxety ];
   for ( int i = 0; i < this->maxety; i++ )
      this->elemDescriptions[ i ] = this->ReadElementTypeDescription(
                                                   this->ptrToElemType[ i ] );
}

int * ansysReader::ReadElementTypeDescription( int pointer )
{
   std::cout << "\nReading Element Type Description" << std::endl;

   if ( pointer == 0 ) 
   {
#ifdef PRINT
      std::cout << "\treturning NULL because pointer == 0" << std::endl;
#endif // PRINT_HEADERS
      return NULL;
   }

   int intPosition = pointer;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->etysiz ) 
   {
      std::cerr << "numValues = " << numValues << " != etysiz" << std::endl;
      exit( 1 );
   }

   // read all integers
   int * elemDescription = new int [ this->etysiz ];
   for ( int i = 0; i < this->etysiz; i++ )
   {
      elemDescription[ i ] = ReadNthInteger( intPosition++ );
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

   int numNodesInElement = elemDescription[ 61-1 ];
   int numCornerNodes = elemDescription[ 94-1 ];

#ifdef PRINT_HEADERS
   std::cout << std::setw( PRINT_WIDTH ) << "element type reference number = " << elemDescription[ 1-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "element routine number = " << elemDescription[ 2-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of dof/node = " << elemDescription[ 34-1 ] << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of nodes = " << numNodesInElement << std::endl;
   std::cout << std::setw( PRINT_WIDTH ) << "number of corner nodes = " << numCornerNodes << std::endl;
#endif // PRINT_HEADERS

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
   return elemDescription;
}

void ansysReader::ReadNodalCoordinates()
{
   if ( this->ptrNOD == 0 )
      return;

   std::cout << "\nReading Nodal Coordinates" << std::endl;

   int intPosition = this->ptrNOD;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   if ( expectedNumValues != 7 * this->numNodes ) 
   {
      std::cerr << "expectedNumValues = " << expectedNumValues
           << " != 7 * numNodes" << std::endl;
      exit( 1 );
   }

   int zero = ReadNthInteger( intPosition++ );
   if ( zero != 0 ) 
   {
      std::cerr << "zero = " << zero << " != 0" << std::endl;
      exit( 1 );
   }

   vtkPoints *vertex = vtkPoints::New();
   // allocate space for the nodal coordinate arrays
   this->nodalCoordinates = new double * [ this->numNodes ];
   for ( int i = 0; i < this->numNodes; i++ )
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
      std::cout << "for i = " << i << std::endl;
      for ( int j = 0; j < 7; j++ )
      {  
         std::cout << "\t" << this->nodalCoordinates[ i ][ j ] << std::endl;
      }
#endif // PRINT_HEADERS

      vertex->InsertPoint( (int)this->nodalCoordinates[ i ][ 0 ],
                           this->nodalCoordinates[ i ][ 1 ],
                           this->nodalCoordinates[ i ][ 2 ],
                           this->nodalCoordinates[ i ][ 3 ] );
   }
   this->ugrid->SetPoints( vertex );
   vertex->Delete();

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementDescriptionIndexTable()
{
   if ( this->ptrELM == 0 )
      return;

   std::cout << "\nReading Element Description Index Table" << std::endl;

   int intPosition = this->ptrELM;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   // allocate space for the nodal coordinate arrays
   this->ptrElemDescriptions = new int [ this->numElems ];

   // read all values
   if ( fileIO::readNByteBlockFromFile( this->ptrElemDescriptions,
                  sizeof(int), this->numElems, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit(1);
   }

#ifdef PRINT_HEADERS
   for ( int i = 0; i < this->numElems; i++ )
   {  
      std::cout << "\tptrElemDescriptions[ " << i << " ] = " 
           << this->ptrElemDescriptions[ i ] << std::endl;
   }
#endif // PRINT_HEADERS

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }

   // now we are ready to construct the mesh
   std::cout << "\nConstructing the mesh" << std::endl;
   this->ugrid->Allocate(this->numElems,this->numElems);
   for ( int i = 0; i < this->numElems; i++ )
   {  
      this->ReadElementDescription( this->ptrElemDescriptions[ i ] );
   }
   //std::cout << "done constructing the mesh" << std::endl;
}

void ansysReader::ReadElementDescription( int pointer )
{
   if ( pointer == 0 )
      return;

   //std::cout << "\nReading Element Description" << std::endl;

   int intPosition = pointer;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   PRINT( numValues );

   int mat = ReadNthInteger( intPosition++ );
   PRINT( mat );

   int type = ReadNthInteger( intPosition++ );  //important
   PRINT( type );

   int real = ReadNthInteger( intPosition++ );
   PRINT( real );

   int secnum = ReadNthInteger( intPosition++ );
   PRINT( secnum );

   int esys = ReadNthInteger( intPosition++ );
   PRINT( esys );

   int death = ReadNthInteger( intPosition++ );
   PRINT( death );

   int solidm = ReadNthInteger( intPosition++ );
   PRINT( solidm );

   int shape = ReadNthInteger( intPosition++ );
   PRINT( shape );

   int elnum = ReadNthInteger( intPosition++ );
   PRINT( elnum );

   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 )
   {
      std::cerr << "ERROR: zero != 0" << std::endl;
      exit( 1 );
   }

   int numNodesInElement = this->elemDescriptions[ type - 1 ][ 61-1 ];
   PRINT( numNodesInElement );

   int numCornerNodes = this->elemDescriptions[ type - 1 ][ 94-1 ];
   PRINT( numCornerNodes );

   // allocate space for the node IDs that define the corners of the hex element
   int * nodes = new int [ numNodesInElement ];

   // read the node IDs that define the element
   if ( fileIO::readNByteBlockFromFile( nodes,
                  sizeof(int), numNodesInElement, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit(1);
   }

#ifdef PRINT_HEADERS
   for ( int i = 0; i < numCornerNodes; i++ )
   {  
      std::cout << "\tcornerNodes[ " << i << " ] = " << nodes[ i ] << std::endl;
   }
#endif // PRINT_HEADERS

   // read rest of values: usually but not always zero
   intPosition += numNodesInElement;
   for ( int i = 0; i < numValues - (10 + numNodesInElement); i++ )
   {  
      int zero = ReadNthInteger( intPosition++ );
      /*PRINT( zero );
      if ( zero != 0 )
      {
         std::cerr << "ERROR: zero != 0" << std::endl;
         exit( 1 );
      }*/
   }

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }

   if ( numCornerNodes == 8 )
      this->ugrid->InsertNextCell( VTK_HEXAHEDRON, numCornerNodes, nodes );
   else if ( numCornerNodes == 4 )
      this->ugrid->InsertNextCell( VTK_TETRA, numCornerNodes, nodes );
   else
   {
      std::cerr << "Error: Can not yet handle an element with numCornerNodes = "
           << numCornerNodes << std::endl;
      exit( 1 );
   }

   delete [] nodes;
}

void ansysReader::ReadSolutionDataHeader()
{
   if ( this->ptrDataSetSolutions[ 0 ] == 0 )
      return;

   std::cout << "\nReading Solution Data Header" << std::endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 100 ) 
   {
      std::cerr << "numValues = " << numValues << " != 100" << std::endl;
      exit( 1 );
   }

   int solnSetNumber = ReadNthInteger( intPosition++ );
   PRINT( solnSetNumber );
   if ( solnSetNumber != 0 ) 
   {
      std::cerr << "solnSetNumber = " << solnSetNumber << " != 0" << std::endl;
      exit( 1 );
   }

   int numberOfElements = ReadNthInteger( intPosition++ );
   PRINT( numberOfElements );
   if ( numberOfElements != this->numElems ) 
   {
      std::cerr << "numberOfElements = " << numberOfElements
           << " != numElems" << std::endl;
      exit( 1 );
   }

   int numberOfNodes = ReadNthInteger( intPosition++ );
   PRINT( numberOfNodes );
   if ( numberOfNodes != this->numNodes ) 
   {
      std::cerr << "numberOfNodes = " << numberOfNodes << " != numNodes" << std::endl;
      exit( 1 );
   }

   int bitmask = ReadNthInteger( intPosition++ );
   PRINT( bitmask );

   int itime = ReadNthInteger( intPosition++ );
   PRINT( itime );
   if ( itime != 1 ) 
   {
      std::cerr << "itime = " << itime << " != 1" << std::endl;
      exit( 1 );
   }

   int iter = ReadNthInteger( intPosition++ );
   PRINT( iter );
   if ( iter != 1 ) 
   {
      std::cerr << "iter = " << iter << " != 1" << std::endl;
      exit( 1 );
   }

   int ncumit = ReadNthInteger( intPosition++ );
   PRINT( ncumit );

   int numReactionForces = ReadNthInteger( intPosition++ );
   PRINT( numReactionForces );

   int cs_LSC = ReadNthInteger( intPosition++ );
   PRINT( cs_LSC );
   if ( cs_LSC != 0 ) 
   {
      std::cerr << "cs_LSC = " << cs_LSC << " != 0" << std::endl;
      exit( 1 );
   }

   int nmast = ReadNthInteger( intPosition++ );
   PRINT( nmast );
   if ( nmast != 0 ) 
   {
      std::cerr << "nmast = " << nmast << " != 0" << std::endl;
      exit( 1 );
   }

   // the following pointers are relative to the pointer of this header
   // (this->ptrDataSetSolutions[ 0 ])
   this->ptrNodalSolution = ReadNthInteger( intPosition++ );
   PRINT( this->ptrNodalSolution );

   int ptrESl = ReadNthInteger( intPosition++ );
   PRINT( ptrESl );

   int ptrRF  = ReadNthInteger( intPosition++ );
   PRINT( ptrRF );

   int ptrMST = ReadNthInteger( intPosition++ );
   PRINT( ptrMST );

   int ptrBC = ReadNthInteger( intPosition++ );
   PRINT( ptrBC );

   int rxtrap = ReadNthInteger( intPosition++ );
   PRINT( rxtrap );

   int mode = ReadNthInteger( intPosition++ );
   PRINT( mode );

   int isym = ReadNthInteger( intPosition++ );
   PRINT( isym );

   int kcmplx = ReadNthInteger( intPosition++ );
   PRINT( kcmplx );

   int numdof = ReadNthInteger( intPosition++ );
   PRINT( numdof );
   if ( numdof != this->numDOF ) 
   {
      std::cerr << "numdof = " << numdof << " != this->numDOF" << std::endl;
      exit( 1 );
   }

   for ( int i = 0; i < numdof; i++ )
   {
      int dofRefNumber = ReadNthInteger( intPosition++ );
      PRINT( dofRefNumber );
   }

   for ( int i = 0; i < 30 - numdof; i++ )
   {
      int zero = ReadNthInteger( intPosition++ );
      //PRINT( zero );
      if ( zero != 0 ) 
      {
         std::cerr << "zero = " << zero << " != 0" << std::endl;
         exit( 1 );
      }
   }

   for ( int i = 0; i < 20; i++ )
   {
      int title = ReadNthInteger( intPosition++ );
      //PRINT( title );
   }

   for ( int i = 0; i < 20; i++ )
   {
      int stitle1 = ReadNthInteger( intPosition++ );
      //PRINT( stitle1 );
   }

   int dbmtim = ReadNthInteger( intPosition++ );
   PRINT( dbmtim );

   int dbmdat = ReadNthInteger( intPosition++ );
   PRINT( dbmdat );

   int dbfncl = ReadNthInteger( intPosition++ );
   PRINT( dbfncl );

   int soltim = ReadNthInteger( intPosition++ );
   PRINT( soltim );

   int soldat = ReadNthInteger( intPosition++ );
   PRINT( soldat );

   int ptrOND = ReadNthInteger( intPosition++ );
   PRINT( ptrOND );

   int ptrOEL = ReadNthInteger( intPosition++ );
   PRINT( ptrOEL );

   int nfldof = ReadNthInteger( intPosition++ );
   PRINT( nfldof );

   int ptrEXA = ReadNthInteger( intPosition++ );
   PRINT( ptrEXA );

   this->ptrEXT = ReadNthInteger( intPosition++ );
   PRINT( this->ptrEXT );

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadNodalSolutions()
{
   if ( this->ptrNodalSolution == 0 )
      return;

   std::cout << "\nReading Nodal Solutions" << std::endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrNodalSolution;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   PRINT( expectedNumValues );
   if ( expectedNumValues != this->numDOF * this->numNodes ) 
   {
      std::cerr << "expectedNumValues = " << expectedNumValues
           << " != numDOF * numNodes" << std::endl;
      exit( 1 );
   }

   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 ) 
   {
      std::cerr << "zero = " << zero << " != 0" << std::endl;
      exit( 1 );
   }

   // set up arrays to store scalar and vector data over entire mesh...
   // TODO: hardcoded for one scalar and one vector
   int numParameters = 2;
   vtkFloatArray ** parameterData = new vtkFloatArray * [ numParameters ];
   for (int i=0; i < numParameters; i++)
   {
      parameterData[ i ] = vtkFloatArray::New();
   }

   // Because the ansys vertices are one-based, increase the arrays by one
   // TODO: hardcoded for one vector and one scalar
   parameterData[ 0 ]->SetName( "displacement vector" );
   parameterData[ 0 ]->SetNumberOfComponents( 3 );
   parameterData[ 0 ]->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   parameterData[ 1 ]->SetName( "displacement magnitude" );
   parameterData[ 1 ]->SetNumberOfComponents( 1 );
   parameterData[ 1 ]->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   // Read the solutions and populate the floatArrays.
   // Because the ansys vertices are one-based, up the loop by one
   double * nodalSolution = new double [ this->numDOF ];
   for ( int i = 0; i < this->numNodes; i++ )
   {
      if ( fileIO::readNByteBlockFromFile( nodalSolution,
                 sizeof(double), this->numDOF, this->s1, this->endian_flip ) )
      {
         std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << std::endl;
         exit(1);
      }

      parameterData[ 0 ]->SetTuple( this->nodeID[ i ], nodalSolution );

      double magnitude = nodalSolution[ 0 ] * nodalSolution[ 0 ] +
                         nodalSolution[ 1 ] * nodalSolution[ 1 ] +
                         nodalSolution[ 2 ] * nodalSolution[ 2 ];
      magnitude = sqrt( magnitude );
      parameterData[ 1 ]->SetTuple1( this->nodeID[ i ], magnitude );
   }

   // Set selected scalar and vector quantities to be written to pointdata array
   letUsersAddParamsToField( numParameters, parameterData,
                             this->ugrid->GetPointData() );

   for (int i=0; i < numParameters; i++) parameterData[i]->Delete();
   delete [] parameterData;      parameterData = NULL;
   
   delete [] nodalSolution;

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

void ansysReader::ReadHeaderExtension()
{
   if ( this->ptrEXT == 0 )
      return;

   std::cout << "\nReading Header Extension" << std::endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrEXT;

   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   if ( reportedNumValues != 200 )
   {
      std::cerr << "reportedNumValues = " << reportedNumValues << " != 200" << std::endl;
      exit( 1 );
   }

   // read the data
   for ( int i = 0; i < this->numDOF; i++ )
   {
      int dofRefNumber = ReadNthInteger( intPosition++ );
      PRINT( dofRefNumber );
   }

   for ( int i = 0; i < 32 - this->numDOF; i++ )
   {
      int zero = ReadNthInteger( intPosition++ );
      //PRINT( zero );
      if ( zero != 0 ) 
      {
         std::cerr << "zero = " << zero << " != 0" << std::endl;
         exit( 1 );
      }
   }

   char dofLabel[ 32 ][ 5 ];
   for ( int i = 0; i < this->numDOF; i++ )
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
      for ( int i = 0; i < this->numDOF; i++ )
         std::cerr << "\tdofLabel[ " << i << " ] = \""
              << dofLabel[ i ] << "\"" << std::endl;
      exit( 1 );
   }

   for ( int i = 0; i < 32 - this->numDOF; i++ )
   {
      int zero;// = ReadNthInteger( intPosition++ );
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
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}

/*
void ansysReader::ReadGenericIntBlock()
{
   //std::cout << "\nReading another block" << std::endl;

   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );

   int expectedNumValues;
   if ( reportedNumValues == 0 )
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   else
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(int);
   std::cout << "reportedNumValues = " << reportedNumValues << endl;
   std::cout << "expectedNumValues = " << expectedNumValues << std::endl;

   // read the data
   for ( int i = 0; i < expectedNumValues; i++ )
   {
      if ( reportedNumValues == 0 )
      {
         double value = ReadNthDouble( intPosition++ );
         std::cout << "\tvalue[ " << i << " ]: " << value << std::endl;
         intPosition++;   // increase again for doubles only
      }
      else
      {
         int integer = ReadNthInteger( intPosition++ );
         std::cout << "\tinteger[ " << i << " ]: " << integer << std::endl;
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      std::cerr << "blockSize = " << blockSize_2
           << " != expected block size" << std::endl;
      exit( 1 );
   }
}
*/

/*
int ansysReader::GetPtrNodalEquivalencyTable()
{
   return this->ptrNodalEquivalencyTable;
}

int ansysReader::GetPtrElementEquivalencyTable()
{
   return this->ptrElementEquivalencyTable;
}

int ansysReader::GetPtrDataStepsIndexTable()
{
   return this->ptrDataStepsIndexTable;
}
int ansysReader::GetPtrTIM()
{
   return this->ptrTIM;
}

int ansysReader::GetPtrLoadStepTable()
{
   return this->ptrLoadStepTable;
}

int ansysReader::GetPtrGEO()
{
   return this->ptrGEO;
}

int ansysReader::GetElemTypePtr( int i )
{
   if ( 0 <= i && i < this->maxety )
      return this->ptrToElemType[ i ];
   else
      return 0;
}

int ansysReader::GetPtrETY()
{
   return this->ptrETY;
}

int ansysReader::GetPtrNOD()
{
   return this->ptrNOD;
}
*/

vtkUnstructuredGrid * ansysReader::GetUGrid()
{
   return this->ugrid;
}

