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
#include <cstdio>
#include "fileIO.h"
#include "converter.h"      // for "letUsersAddParamsToField"

#include "vtkUnstructuredGrid.h"
#include "vtkPoints.h"
#include "vtkFloatArray.h"  // this code requires VTK4
#include "vtkPointData.h"
#include "vtkCellType.h"

#define PRINT_WIDTH 36

#ifndef PRINT_HEADERS
#define PRINT(x)
#else
#define PRINT(x) \
   cout << setw(PRINT_WIDTH-3) << #x << " = " << x << endl;
#endif

using namespace std;

ansysReader::ansysReader( char * input )
{
   ansysFileName = input;
   cout << "\nOpening file \"" << ansysFileName << "\"" << endl;

   // open file
   if((this->s1=fopen(ansysFileName,"r"))==NULL)
   {
      cerr << "ERROR: can't open file \"" << ansysFileName
           << "\", so exiting" << endl;
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

   this->ugrid = vtkUnstructuredGrid::New();
}

ansysReader::~ansysReader()
{
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

   if ( this->ptrToElemType )
   {
      delete [] this->ptrToElemType;
      this->ptrToElemType = NULL;
   }

   if ( this->ptrDataSetSolutions )
   {
      delete [] this->ptrDataSetSolutions;
      this->ptrDataSetSolutions = NULL;
   }

   if ( this->ugrid )
   {
      this->ugrid->Delete();
      this->ugrid = NULL;
   }
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
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
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
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
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
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
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
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
      exit( 1 );
   }
   return value;
}

void ansysReader::ReadHeader()
{
   cout << "\nReading header" << endl;

/*
   // read all integers
   int intArray[ 100 ];
   for ( int i=0; i < 100; i++ )
   {
      intArray[ i ] = ReadNthInteger( i );
      cout << "\tintArray[ " << i << " ]: " << intArray[ i ] << endl;
   }
*/

   // the very first number is the integer 404
   int headerSize = ReadNthInteger( 0 );
   if ( headerSize != 404 ) 
   {
      cerr << "headerSize = " << headerSize 
           << " != 404, will flip endian flag" << endl;
      this->FlipEndian();
      headerSize = ReadNthInteger( 0 );
      if ( headerSize != 404 ) 
      {
         cerr << "headerSize = " << headerSize << " != 404, will exit" << endl;
         exit( 1 );
      }
   }

   // the ANSYS header is 100 ints long
   int numValues = ReadNthInteger( 1 );
   if ( numValues != 100 ) 
   {
      cerr << "numValues = " << numValues << " != 100" << endl;
      exit( 1 );
   }

   //create and null terminate end of 4 character buffer
   char buffer4[ 5 ];
   buffer4[ 4 ] = '\0';

   int itemNumber = 1;   // get ready to get the first item: fileNumber
   int fileNumber = ReadNthInteger( itemNumber+1 );
   cout << setw( PRINT_WIDTH ) << "fileNumber = " << fileNumber 
        << " where 12 = results files, 16 = db files" << endl;

   itemNumber = 2;      // file format
   int fileFormat = ReadNthInteger( itemNumber+1 );
   cout << setw( PRINT_WIDTH ) << "fileFormat = " << fileFormat
        << " (0=internal, 1=external)" << endl;

   itemNumber = 3;      // time
   int time = ReadNthInteger( itemNumber+1 );
   PRINT( time );

   itemNumber = 4;      // date
   int date = ReadNthInteger( itemNumber+1 );
   PRINT( date );

   itemNumber = 5;      // units
   int units = ReadNthInteger( itemNumber+1 );
   cout << setw( PRINT_WIDTH ) << "units = " << units 
        << " (0=user-defined, 1=SI, 2=CSG, 3=feet, 4=inches)" << endl;

   long position = 0;

   itemNumber = 10;     // ANSYS release level
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw( PRINT_WIDTH ) << "ANSYS release level = " << "\""
        << buffer4 << "\"" << endl;

   itemNumber = 11;     // date of ANSYS release
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw( PRINT_WIDTH ) << "date of ANSYS release = " << "\""
        << buffer4 << "\"" << endl;

   // item number 12-14 is machine identifier
   cout << setw( PRINT_WIDTH ) << "machine identifier = " << "\"";
   for ( itemNumber = 12; itemNumber <= 14; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 15-16 is jobname
   cout << setw( PRINT_WIDTH ) << "short form of jobname = " << "\"";
   for ( itemNumber = 15; itemNumber <= 16; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 17-18 is ANSYS product name
   cout << setw( PRINT_WIDTH ) << "ANSYS product name = " << "\"";
   for ( itemNumber = 17; itemNumber <= 18; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 19 is ANSYS special version label
   itemNumber = 19;
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw( PRINT_WIDTH ) << "ANSYS special version label = " << "\""
        << buffer4 << "\"" << endl;

   // item number 20-22 is username
   cout << setw( PRINT_WIDTH ) << "username = " << "\"";
   for ( itemNumber = 20; itemNumber <= 22; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 23-25 is machine identifier
   cout << setw( PRINT_WIDTH ) << "machine identifier = " << "\"";
   for ( itemNumber = 23; itemNumber <= 25; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

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
   cout << setw( PRINT_WIDTH ) << "long form of jobname = " << "\"";
   for ( itemNumber = 31; itemNumber <= 38; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 41-60 is main analysis title 
   cout << setw( PRINT_WIDTH ) << "main analysis title = " << "\"";
   for ( itemNumber = 41; itemNumber <= 60; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 61-80 is first subtitle
   cout << setw( PRINT_WIDTH ) << "first subtitle = " << "\"";
   for ( itemNumber = 61; itemNumber <= 80; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 95 is split point of the file
   itemNumber = 95;
   int splitPoint = ReadNthInteger( itemNumber+1 );
   cout << setw( PRINT_WIDTH ) << "split point of the file = " << splitPoint << endl;

   // item number 97-98 is filesize at write
   itemNumber = 97;
   long filesize = ReadNthLong( itemNumber+1 );
   cout << setw( PRINT_WIDTH ) << "filesize at write = " << filesize << endl;

   // the number at integer position 102 is 404
   headerSize = ReadNthInteger( 102 );
   if ( headerSize != 404 ) 
   {
      cerr << "headerSize = " << headerSize << " != 404" << endl;
      exit( 1 );
   }

   // We have now will read past the header which includes integers
   // headerSize, numValues, headerSize
   this->integerPosition = numValues + 3;
}

void ansysReader::ReadRSTHeader()
{
   cout << "\nReading RST Header" << endl;

   // the number at the next integer position 164
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_1 != 164 ) 
   {
      cerr << "blockSize = " << blockSize_1 << " != 16" << endl;
      exit( 1 );
   }

   // this block is 40 ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != 40 ) 
   {
      cerr << "numValues = " << numValues << " != 40" << endl;
      exit( 1 );
   }

   // read all 40 integers
   int fun12 = ReadNthInteger( this->integerPosition++ );
   PRINT( fun12 );
   if ( fun12 != 12 )
   {
      cerr << "ERROR: fun12 != 12" << endl;
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
         cerr << "ERROR: zero != 0" << endl;
         exit( 1 );
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadDOFBlock()
{
   cout << "\nReading DOF block" << endl;

   // the number at the next integer position 16 
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   
   // this block is numDOF ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != this->numDOF ) 
   {
      cerr << "numValues = " << numValues << " != numDOF" << endl;
      exit( 1 );
   }

   // read all integers
   this->dofCode = new int [ this->numDOF ];
   for ( int i=0; i < numValues; i++ )
   {
      this->dofCode[ i ] = ReadNthInteger( this->integerPosition++ );
      cout << "\tdofCode[ " << i << " ]: " << this->dofCode[ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( this->integerPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadNodalEquivalencyTable()
{
   if ( this->ptrNodalEquivalencyTable == 0 )
      return;

   cout << "\nReading Nodal Equivalency Table" << endl;

   int intPosition = this->ptrNodalEquivalencyTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );

   // this block is numNodes ints long
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numNodes ) 
   {
      cerr << "numValues = " << numValues << " != numNodes" << endl;
      exit( 1 );
   }

   // read all integers
   this->nodeID = new int [ this->numNodes ];
   for ( int i = 0; i < this->numNodes; i++ )
   {
      this->nodeID[ i ] = ReadNthInteger( intPosition++ );
      cout << "\tnodeID[ " << i << " ]: " << this->nodeID[ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementEquivalencyTable()
{
   if ( this->ptrElementEquivalencyTable == 0 )
      return;

   cout << "\nReading Element Equivalency Table" << endl;

   int intPosition = this->ptrElementEquivalencyTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   // this block is numElems ints long
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numElems ) 
   {
      cerr << "numValues = " << numValues << " != numElems" << endl;
      exit( 1 );
   }

   // read all integers
   this->elemID = new int [ this->numElems ];
   for ( int i = 0; i < this->numElems; i++ )
   {
      this->elemID[ i ] = ReadNthInteger( intPosition++ );
      cout << "\telemID[ " << i << " ]: " << this->elemID[ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadDataStepsIndexTable()
{
   if ( this->ptrDataStepsIndexTable == 0 )
      return;

   cout << "\nReading Data Steps Index Table" << endl;

   int intPosition = this->ptrDataStepsIndexTable;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 2 * this->maxNumberDataSets ) 
   {
      cerr << "numValues = " << numValues << " != 2 * maxNumberDataSets " << endl;
      exit( 1 );
   }

   this->ptrDataSetSolutions = new int [ 2 * this->maxNumberDataSets ];
   // read all integers
   for ( int i = 0; i < 2 * this->maxNumberDataSets; i++ )
   {
      this->ptrDataSetSolutions [ i ] = ReadNthInteger( intPosition++ );
      cout << "\tptrDataSetSolutions[ " << i << " ]: " << this->ptrDataSetSolutions [ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadTimeTable()
{
   if ( this->ptrTIM == 0 )
      return;

   cout << "\nReading Time Table" << endl;

   int intPosition = this->ptrTIM;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 0 ) 
   {
      cerr << "numValues = " << numValues << " != 0" << endl;
      exit( 1 );
   }

   // read all values 
   for ( int i = 0; i < this->maxNumberDataSets; i++ )
   {
      double value = ReadNthDouble( intPosition++ );
      cout << "\tvalue[ " << i << " ]: " << value << endl;
      intPosition++;   // increase increment for double
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadGeometryTable()
{
   if ( this->ptrGEO == 0 )
      return;

   cout << "\nReading Geometry Table" << endl;

   int intPosition = this->ptrGEO;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   PRINT( numValues );
   if ( numValues != 20 && numValues != 40 ) 
   {
      cerr << "numValues = " << numValues << " must be 20 or 40" << endl;
      exit( 1 );
   }

   // read all integer values 
   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 )
   {
      cerr << "ERROR: zero != 0" << endl;
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
         cerr << "ERROR: zero != 0" << endl;
         exit( 1 );
      }
   }

   this->ptrMAS = ReadNthInteger( intPosition++ );
   PRINT( this->ptrMAS );

   int csysiz = ReadNthInteger( intPosition++ );
   PRINT( csysiz );
   if ( csysiz != 24 )
      cerr << "WARNING: csysiz != 24" << endl;

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
            cerr << "ERROR: zero != 0" << endl;
            exit( 1 );
         }
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementTypeIndexTable()
{
   if ( this->ptrETY == 0 )
      return;

   cout << "\nReading Element Type Index Table" << endl;

   int intPosition = this->ptrETY;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->maxety ) 
   {
      cerr << "numValues = " << numValues << " != maxety" << endl;
      exit( 1 );
   }

   // read all integers
   this->ptrToElemType = new int [ this->maxety ];
   for ( int i = 0; i < this->maxety; i++ )
   {
      this->ptrToElemType[ i ] = ReadNthInteger( intPosition++ );
      cout << "\tptrToElemType[ " << i << " ]: " << this->ptrToElemType[ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
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
   cout << "\nReading Element Type Description" << endl;

   int intPosition = pointer;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->etysiz ) 
   {
      cerr << "numValues = " << numValues << " != etysiz" << endl;
      exit( 1 );
   }
   /* descriptions of key paramters (in one-based notation)
   item 1    : element type reference number
   item 2    : element routine number
   item 3-14 : element type option keys
   item 34   : dof
   item 61   : number of nodes
   item 94   : number of corner nodes
   */

   // read all integers
   int * elemDescriptions = new int [ this->etysiz ];
   for ( int i = 0; i < this->etysiz; i++ )
   {
      elemDescriptions[ i ] = ReadNthInteger( intPosition++ );
      //cout << "\telemDescriptions[ " << i << " ]: " << elemDescriptions[ i ] << endl;
   }
   cout << setw( PRINT_WIDTH ) << "element type reference number = " << elemDescriptions[ 1-1 ] << endl;
   cout << setw( PRINT_WIDTH ) << "element routine number = " << elemDescriptions[ 2-1 ] << endl;
   cout << setw( PRINT_WIDTH ) << "number of dof/node = " << elemDescriptions[ 34-1 ] << endl;
   cout << setw( PRINT_WIDTH ) << "number of nodes = " << elemDescriptions[ 61-1 ] << endl;
   cout << setw( PRINT_WIDTH ) << "number of corner nodes = " << elemDescriptions[ 94-1 ] << endl;

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
   return elemDescriptions;
}

void ansysReader::ReadNodalCoordinates()
{
   if ( this->ptrNOD == 0 )
      return;

   cout << "\nReading Nodal Coordinates" << endl;

   int intPosition = this->ptrNOD;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   if ( expectedNumValues != 7 * this->numNodes ) 
   {
      cerr << "expectedNumValues = " << expectedNumValues
           << " != 7 * numNodes" << endl;
      exit( 1 );
   }

   int zero = ReadNthInteger( intPosition++ );
   if ( zero != 0 ) 
   {
      cerr << "zero = " << zero << " != 0" << endl;
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
         cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << endl;
         exit(1);
      }

      cout << "for i = " << i << endl;
      for ( int j = 0; j < 7; j++ )
      {  
         cout << "\t" << this->nodalCoordinates[ i ][ j ] << endl;
      }

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
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadElementDescriptionIndexTable()
{
   if ( this->ptrELM == 0 )
      return;

   cout << "\nReading Element Description Index Table" << endl;

   int intPosition = this->ptrELM;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != this->numElems ) 
   {
      cerr << "numValues = " << numValues << " != numElems" << endl;
      exit( 1 );
   }

   // allocate space for the nodal coordinate arrays
   this->ptrElemDescriptions = new int [ this->numElems ];

   // read all values
   if ( fileIO::readNByteBlockFromFile( this->ptrElemDescriptions,
                  sizeof(int), this->numElems, this->s1, this->endian_flip ) )
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << endl;
      exit(1);
   }

   for ( int i = 0; i < this->numElems; i++ )
   {  
      cout << "\tptrElemDescriptions[ " << i << " ] = " 
           << this->ptrElemDescriptions[ i ] << endl;
   }

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }

   // now we are ready to construct the mesh
   cout << "\nconstructing the mesh" << endl;
   this->ugrid->Allocate(this->numElems,this->numElems);
   for ( int i = 0; i < this->numElems; i++ )
   {  
      int * cornerNodes = this->ReadElementDescription( this->ptrElemDescriptions[ i ] );
      this->ugrid->InsertNextCell( VTK_HEXAHEDRON, 8, cornerNodes );
      //delete [] cornerNodes;
   }
   cout << "done constructing the mesh" << endl;
}

int * ansysReader::ReadElementDescription( int pointer )
{
   if ( pointer == 0 )
      return NULL;

   //cout << "\nReading Element Description" << endl;

   int intPosition = pointer;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 30 ) 
   {
      cerr << "numValues = " << numValues << " != 30" << endl;
      exit( 1 );
   }

   // allocate space for the node IDs that define the corners of the hex element
   int junk [ 10 ];

   // read the preface information
   if ( fileIO::readNByteBlockFromFile( &junk,
                  sizeof(int), 10, this->s1, this->endian_flip ) )
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << endl;
      exit(1);
   }

   cout << "\nReading Element Description for element " << junk[ 8 ] << endl;

   // allocate space for the node IDs that define the corners of the hex element
   int * cornerNodes = new int [ 8 ];

   // read the node IDs that define the corners of the hex element
   if ( fileIO::readNByteBlockFromFile( cornerNodes,
                  sizeof(int), 8, this->s1, this->endian_flip ) )
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << endl;
      exit(1);
   }

   for ( int i = 0; i < 8; i++ )
   {  
      cout << "\tcornerNodes[ " << i << " ] = " << cornerNodes[ i ] << endl;
   }

   // allocate space for the other node IDs
   int nonCornerNodes [ 12 ];

   // read the preface information
   if ( fileIO::readNByteBlockFromFile( &nonCornerNodes,
                  sizeof(int), 12, this->s1, this->endian_flip ) )
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << endl;
      exit(1);
   }

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
   return cornerNodes;
}

void ansysReader::ReadSolutionDataHeader()
{
   if ( this->ptrDataSetSolutions[ 0 ] == 0 )
      return;

   cout << "\nReading Solution Data Header" << endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );
   if ( numValues != 100 ) 
   {
      cerr << "numValues = " << numValues << " != 100" << endl;
      exit( 1 );
   }

   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 ) 
   {
      cerr << "zero = " << zero << " != 0" << endl;
      exit( 1 );
   }

   int numberOfElements = ReadNthInteger( intPosition++ );
   PRINT( numberOfElements );
   if ( numberOfElements != this->numElems ) 
   {
      cerr << "numberOfElements = " << numberOfElements << " != numElems" << endl;
      exit( 1 );
   }

   int numberOfNodes = ReadNthInteger( intPosition++ );
   PRINT( numberOfNodes );
   if ( numberOfNodes != this->numNodes ) 
   {
      cerr << "numberOfNodes = " << numberOfNodes << " != numNodes" << endl;
      exit( 1 );
   }

   int bitmask = ReadNthInteger( intPosition++ );
   PRINT( bitmask );

   int itime = ReadNthInteger( intPosition++ );
   PRINT( itime );
   if ( itime != 1 ) 
   {
      cerr << "itime = " << itime << " != 1" << endl;
      exit( 1 );
   }

   int iter = ReadNthInteger( intPosition++ );
   PRINT( iter );
   if ( iter != 1 ) 
   {
      cerr << "iter = " << iter << " != 1" << endl;
      exit( 1 );
   }

   int ncumit = ReadNthInteger( intPosition++ );
   PRINT( ncumit );
   if ( ncumit != 1 ) 
   {
      cerr << "ncumit = " << ncumit << " != 1" << endl;
      exit( 1 );
   }

   int numReactionForces = ReadNthInteger( intPosition++ );
   PRINT( numReactionForces );

   int cs_LSC = ReadNthInteger( intPosition++ );
   PRINT( cs_LSC );
   if ( cs_LSC != 0 ) 
   {
      cerr << "cs_LSC = " << cs_LSC << " != 0" << endl;
      exit( 1 );
   }

   int nmast = ReadNthInteger( intPosition++ );
   PRINT( nmast );
   if ( nmast != 0 ) 
   {
      cerr << "nmast = " << nmast << " != 0" << endl;
      exit( 1 );
   }

   this->ptrNodalSolution = ReadNthInteger( intPosition++ );
   PRINT( this->ptrNodalSolution );

   // TODO: there is more to read....
   // but return for now
   return;

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadNodalSolutions()
{
   if ( this->ptrNodalSolution == 0 )
      return;

   cout << "\nReading Nodal Solutions" << endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrNodalSolution;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   PRINT( expectedNumValues );
   if ( expectedNumValues != this->numDOF * this->numNodes ) 
   {
      cerr << "expectedNumValues = " << expectedNumValues
           << " != numDOF * numNodes" << endl;
      exit( 1 );
   }

   int zero = ReadNthInteger( intPosition++ );
   PRINT( zero );
   if ( zero != 0 ) 
   {
      cerr << "zero = " << zero << " != 0" << endl;
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
   parameterData[ 0 ]->SetNumberOfTuples( this->numNodes + 1 );   // note +1

   parameterData[ 1 ]->SetName( "displacement magnitude" );
   parameterData[ 1 ]->SetNumberOfComponents( 1 );
   parameterData[ 1 ]->SetNumberOfTuples( this->numNodes + 1 );   // note +1

   // Read the solutions and populate the floatArrays.
   // Because the ansys vertices are one-based, up the loop by one
   double * nodalSolution = new double [ this->numDOF ];
   for ( int i = 1; i < this->numNodes + 1; i++ )
   {
      if ( fileIO::readNByteBlockFromFile( nodalSolution,
                 sizeof(double), this->numDOF, this->s1, this->endian_flip ) )
      {
         cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
              << endl;
         exit(1);
      }

      parameterData[ 0 ]->SetTuple( i, nodalSolution );

      double magnitude = nodalSolution[ 0 ] * nodalSolution[ 0 ] +
                         nodalSolution[ 1 ] * nodalSolution[ 1 ] +
                         nodalSolution[ 2 ] * nodalSolution[ 2 ];
      magnitude = sqrt( magnitude );
      parameterData[ 1 ]->SetTuple1( i, magnitude );
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
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
      exit( 1 );
   }
}

/*
void ansysReader::ReadGenericIntBlock()
{
   //cout << "\nReading another block" << endl;

   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );

   int expectedNumValues;
   if ( reportedNumValues == 0 )
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(double);
   else
      expectedNumValues = ( blockSize_1 - sizeof(int) ) / sizeof(int);
   cout << "reportedNumValues = " << reportedNumValues << endl;
   cout << "expectedNumValues = " << expectedNumValues << endl;

   // read the data
   for ( int i = 0; i < expectedNumValues; i++ )
   {
      if ( reportedNumValues == 0 )
      {
         double value = ReadNthDouble( intPosition++ );
         cout << "\tvalue[ " << i << " ]: " << value << endl;
         intPosition++;   // increase again for doubles only
      }
      else
      {
         int integer = ReadNthInteger( intPosition++ );
         cout << "\tinteger[ " << i << " ]: " << integer << endl;
      }
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   if ( blockSize_2 != blockSize_1 ) 
   {
      cerr << "blockSize = " << blockSize_2
           << " != expected block size" << endl;
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

