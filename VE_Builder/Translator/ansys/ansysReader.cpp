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

#define PRINT_HEADERS

#ifndef PRINT_HEADERS
#define PRINT(x)
#else
#define PRINT(x) \
   cout << setw(30-3) << #x << " = " << x << endl;
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

   this->position = 0;
   this->integerPosition = 0;

   this->numNodes = 0;
   this->numElems = 0;
}

ansysReader::~ansysReader()
{
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

   int width = 30;

   int itemNumber = 1;   // get ready to get the first item: fileNumber
   int fileNumber = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "fileNumber = " << fileNumber 
        << " where 12 = results files, 16 = db files" << endl;

   itemNumber = 2;      // file format
   int fileFormat = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "fileFormat = " << fileFormat
        << " (0=internal, 1=external)" << endl;

   itemNumber = 3;      // time
   int time = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "time = " << time << endl;

   itemNumber = 4;      // date
   int date = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "date = " << date << endl;

   itemNumber = 5;      // units
   int units = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "units = " << units 
        << " (0=user-defined, 1=SI, 2=CSG, 3=feet, 4=inches)" << endl;

   itemNumber = 10;     // ANSYS release level
   this->position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,this->position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw(width) << "ANSYS release level = " << "\""
        << buffer4 << "\"" << endl;

   itemNumber = 11;     // date of ANSYS release
   this->position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,this->position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw(width) << "date of ANSYS release = " << "\""
        << buffer4 << "\"" << endl;

   // item number 12-14 is machine identifier
   cout << setw(width) << "machine identifier = " << "\"";
   for ( itemNumber = 12; itemNumber <= 14; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 15-16 is jobname
   cout << setw(width) << "short form of jobname = " << "\"";
   for ( itemNumber = 15; itemNumber <= 16; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 17-18 is ANSYS product name
   cout << setw(width) << "ANSYS product name = " << "\"";
   for ( itemNumber = 17; itemNumber <= 18; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 19 is ANSYS special version label
   itemNumber = 19;
   this->position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,this->position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw(width) << "ANSYS special version label = " << "\""
        << buffer4 << "\"" << endl;

   // item number 20-22 is username
   cout << setw(width) << "username = " << "\"";
   for ( itemNumber = 20; itemNumber <= 22; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 23-25 is machine identifier
   cout << setw(width) << "machine identifier = " << "\"";
   for ( itemNumber = 23; itemNumber <= 25; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   itemNumber = 26;     // system record size
   int systemRecordSize = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "system record size = " << systemRecordSize << endl;

   itemNumber = 27;     // maximum file length
   int maximumFileLength = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "maximum file length = " << maximumFileLength << endl;

   itemNumber = 28;     // maximum record size
   int maximumRecordSize = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "maximum record size = " << maximumRecordSize << endl;

   // item number 31-38 is jobname
   cout << setw(width) << "long form of jobname = " << "\"";
   for ( itemNumber = 31; itemNumber <= 38; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 41-60 is main analysis title 
   cout << setw(width) << "main analysis title = " << "\"";
   for ( itemNumber = 41; itemNumber <= 60; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 61-80 is first subtitle
   cout << setw(width) << "first subtitle = " << "\"";
   for ( itemNumber = 61; itemNumber <= 80; itemNumber++ )
   {
      this->position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,this->position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 95 is split point of the file
   itemNumber = 95;
   int splitPoint = ReadNthInteger( itemNumber+1 );
   cout << setw(width) << "split point of the file = " << splitPoint << endl;

   // item number 97-98 is filesize at write
   itemNumber = 97;
   long filesize = ReadNthLong( itemNumber+1 );
   cout << setw(width) << "filesize at write = " << filesize << endl;

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

   int ptrLoadStep = ReadNthInteger( this->integerPosition++ );
   PRINT( ptrLoadStep );

   this->ptrElementEquivalencyTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrElementEquivalencyTable );

   this->ptrNodalEquivalencyTable = ReadNthInteger( this->integerPosition++ );
   PRINT( this->ptrNodalEquivalencyTable );

   int ptrGEO = ReadNthInteger( this->integerPosition++ );
   PRINT( ptrGEO );

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
   cout << "\nReading Nodal Equivalency Table" << endl;

   //cout << "integerPosition = " << this->integerPosition << endl;
   integerPosition = this->ptrNodalEquivalencyTable;
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );

   // this block is numNodes ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != this->numNodes ) 
   {
      cerr << "numValues = " << numValues << " != numNodes" << endl;
      exit( 1 );
   }

   // read all integers
   this->nodeID = new int [ this->numNodes ];
   for ( int i = 0; i < this->numNodes; i++ )
   {
      this->nodeID[ i ] = ReadNthInteger( this->integerPosition++ );
      cout << "\tnodeID[ " << i << " ]: " << this->nodeID[ i ] << endl;
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

void ansysReader::ReadElementEquivalencyTable()
{
   cout << "\nReading Element Equivalency Table" << endl;

   integerPosition = this->ptrElementEquivalencyTable;
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   
   // this block is numElems ints long
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != this->numElems ) 
   {
      cerr << "numValues = " << numValues << " != numElems" << endl;
      exit( 1 );
   }

   // read all integers
   this->elemID = new int [ this->numElems ];
   for ( int i = 0; i < this->numElems; i++ )
   {
      this->elemID[ i ] = ReadNthInteger( this->integerPosition++ );
      cout << "\telemID[ " << i << " ]: " << this->elemID[ i ] << endl;
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

void ansysReader::ReadDataStepsIndexTable()
{
   cout << "\nReading Data Steps Index Table" << endl;

   integerPosition = this->ptrDataStepsIndexTable;
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != 2 * this->maxNumberDataSets ) 
   {
      cerr << "numValues = " << numValues << " != 2 * maxNumberDataSets " << endl;
      exit( 1 );
   }

   // read all integers
   for ( int i = 0; i < 2 * this->maxNumberDataSets; i++ )
   {
      int integer = ReadNthInteger( this->integerPosition++ );
      cout << "\tinteger[ " << i << " ]: " << integer << endl;
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

void ansysReader::ReadTimeTable()
{
   cout << "\nReading Time Table" << endl;

   integerPosition = this->ptrTIM;
   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   
   int numValues = ReadNthInteger( this->integerPosition++ );
   if ( numValues != 0 ) 
   {
      cerr << "numValues = " << numValues << " != 0" << endl;
      exit( 1 );
   }

   // read all values 
   for ( int i = 0; i < this->maxNumberDataSets; i++ )
   {
      double value = ReadNthDouble( this->integerPosition++ );
      cout << "\tvalue[ " << i << " ]: " << value << endl;
      this->integerPosition++;   // increase increment for double
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

void ansysReader::ReadGenericIntBlock()
{
   //cout << "\nReading another block" << endl;

   int blockSize_1 = ReadNthInteger( this->integerPosition++ );
   int reportedNumValues = ReadNthInteger( this->integerPosition++ );

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
         //float value = ReadNthFloat( this->integerPosition++ );
         double value = ReadNthDouble( this->integerPosition++ );
         cout << "\tvalue[ " << i << " ]: " << value << endl;
         this->integerPosition++;   // increase again for doubles only
      }
      else
      {
         int integer = ReadNthInteger( this->integerPosition++ );
         cout << "\tinteger[ " << i << " ]: " << integer << endl;
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


