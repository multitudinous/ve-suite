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
   this->ptrNSL = 0; // Nodal solutions
   this->ptrESL = 0; // Element solutions
   this->ptrEXT = 0;
   this->ptrENS = NULL;
   this->numCornerNodesInElement = NULL;
   this->cornerNodeNumbersForElement = NULL;
   this->summedVonMisesStress = NULL;
   this->numContributingElements = NULL;

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

   if ( this->ptrENS )
   {
      delete [] this->ptrENS;
      this->ptrENS = NULL;
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
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
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
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
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
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
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
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile"
                << std::endl;
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
         std::cerr << "headerSize = " << headerSize
                   << " != 404, will exit" << std::endl;
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   for ( int i=0; i < this->numDOF; i++ )
   {
      this->dofCode[ i ] = ReadNthInteger( this->integerPosition++ );
#ifdef PRINT_HEADERS
      std::cout << "\tdofCode[ " << i << " ]: " << this->dofCode[ i ] << std::endl;
#endif // PRINT_HEADERS
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( this->integerPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );
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
   VerifyBlock( blockSize_1, blockSize_2 );

   this->summedVonMisesStress = new double [ this->numNodes ];
   this->numContributingElements = new int [ this->numNodes ];
   for ( int i = 0; i < this->numNodes; i++ )
   {
      this->summedVonMisesStress [ i ] = 0.0;
      this->numContributingElements [ i ] = 0;
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   VerifyBlock( blockSize_1, blockSize_2 );

   // allocate space for the element description arrays
   this->elemDescriptions = new int * [ this->maxety ];
   for ( int i = 0; i < this->maxety; i++ )
      this->elemDescriptions[ i ] = this->ReadElementTypeDescription(
                                                   this->ptrToElemType[ i ] );
}

int * ansysReader::ReadElementTypeDescription( int pointer )
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
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   return elemDescription;
}

void ansysReader::ReadNodalCoordinates()
{
   if ( this->ptrNOD == 0 )
      return;

   std::cout << "\nReading Nodal Coordinates" << std::endl;

   int intPosition = this->ptrNOD;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   
   if ( numValues != 7 * this->numNodes ) 
   {
      std::cerr << "numValues = " << numValues
                << " != 7 * numNodes" << std::endl;
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
   VerifyBlock( blockSize_1, blockSize_2 );
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
   this->numCornerNodesInElement = new int [ this->numElems ];
   this->cornerNodeNumbersForElement = new int * [ this->numElems ];

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
   VerifyBlock( blockSize_1, blockSize_2 );

   // now we are ready to construct the mesh
   std::cout << "\nConstructing the mesh" << std::endl;
   this->ugrid->Allocate(this->numElems,this->numElems);
   for ( int i = 0; i < this->numElems; i++ )
   {  
      this->ReadElementDescription( i, this->ptrElemDescriptions[ i ] );
   }
   //std::cout << "done constructing the mesh" << std::endl;
}

void ansysReader::ReadElementDescription( int elemIndex, int pointer )
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

   this->numCornerNodesInElement[ elemIndex ] = numCornerNodes;
   this->cornerNodeNumbersForElement[ elemIndex ] = new int [ numCornerNodes ];

   // allocate space for the node IDs that define the corners of the element
   int * nodes = new int [ numNodesInElement ];

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
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   if ( numNodesInElement == 20 && numCornerNodes == 8 )
      this->ugrid->InsertNextCell( VTK_HEXAHEDRON, numCornerNodes, nodes );
   else if ( numNodesInElement == 10 && numCornerNodes == 4 )
      this->ugrid->InsertNextCell( VTK_TETRA, numCornerNodes, nodes );
   else if ( numNodesInElement == 8 && numCornerNodes == 4 )
      this->ugrid->InsertNextCell( VTK_QUAD, numCornerNodes, nodes );
   else if ( numNodesInElement == 3 && numCornerNodes == 2 )
      this->ugrid->InsertNextCell( VTK_LINE, numCornerNodes, nodes );
   else if ( numNodesInElement == 1 && numCornerNodes == 1 )
      this->ugrid->InsertNextCell( VTK_VERTEX, numCornerNodes, nodes );
   else
   {
      std::cerr << "Warning: Can not yet handle an element with "
         << "numNodesInElement = " << numNodesInElement
         << " and numCornerNodes = " << numCornerNodes << std::endl;
   }

   //delete [] nodes;
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
      std::cerr << "numberOfNodes = " << numberOfNodes
                << " != numNodes" << std::endl;
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
   //NSL = NodalSolution
   this->ptrNSL = ReadNthInteger( intPosition++ );
   PRINT( this->ptrNSL );

   this->ptrESL = ReadNthInteger( intPosition++ );
   PRINT( this->ptrESL );

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
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadNodalSolutions()
{
   if ( this->ptrNSL == 0 )
      return;

   std::cout << "\nReading Nodal Solutions" << std::endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrNSL;
   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   
   if ( numValues != this->numDOF * this->numNodes ) 
   {
      std::cerr << "numValues = " << numValues
                << " != numDOF * numNodes" << std::endl;
      exit( 1 );
   }

   // set up arrays to store scalar and vector data over entire mesh...
   // TODO: hardcoded for one scalar and one vector
   int numParameters = 2;
   vtkFloatArray ** parameterData = new vtkFloatArray * [ numParameters ];
   for ( int i=0; i < numParameters; i++ )
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
         exit( 1 );
      }

      parameterData[ 0 ]->SetTuple( this->nodeID[ i ], nodalSolution );

#ifdef PRINT_HEADERS
      //if ( this->nodeID[ i ] == 2108 ) 
      {
         std::cout <<  "nodalSolution[ " << this->nodeID[ i ] <<" ] = ";
         for ( int j = 0; j < this->numDOF; j++ )
            std::cout << "\t" << nodalSolution[ j ];
         std::cout << std::endl;
      }
#endif // PRINT_HEADERS

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
   delete [] parameterData;
   
   delete [] nodalSolution;

   // the last number is blockSize again
   int blockSize_2;
   fileIO::readNByteBlockFromFile( &blockSize_2, sizeof(int),
                                   1, this->s1, this->endian_flip );
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadElementSolutions()
{
   if ( this->ptrESL == 0 )
      return;

   std::cout << "\nReading Element Solutions" << std::endl;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrESL;

   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   if ( numValues != this->numElems ) 
   {
      std::cerr << "numValues = " << numValues << " != numElems" << std::endl;
      exit( 1 );
   }

   this->ptrENS = new int [ this->numElems ];

   for ( int elemIndex = 0; elemIndex < this->numElems; elemIndex++ )
   {
      int ptrElement_i = ReadNthInteger( intPosition++ );

      PRINT( ptrElement_i );

      int ptrPosition = this->ptrDataSetSolutions[ 0 ] + ptrElement_i;
      ReadElementIndexTable( elemIndex, ptrPosition );
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   this->AttachVonMisesStressToGrid();
}

void ansysReader::AttachVonMisesStressToGrid()
{
   vtkFloatArray * parameterData = vtkFloatArray::New();
   // Because the ansys vertices are one-based, increase the arrays by one
   parameterData->SetName( "von Mises stress" );
   parameterData->SetNumberOfComponents( 1 );
   parameterData->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   for ( int nodeIndex = 0; nodeIndex < this->numNodes; nodeIndex++ )
   {
      double avgVonMisesStress = 0.0;
      if ( numContributingElements[ this->nodeID[nodeIndex] ] > 0)
      {
         avgVonMisesStress = summedVonMisesStress[ this->nodeID[nodeIndex] ]
                           / numContributingElements[ this->nodeID[nodeIndex] ];

#ifdef PRINT_HEADERS
         //if ( nodeIndex == 2108 )
         //if ( this->nodeID[nodeIndex] == 2108 )
         {
            std::cout << "Node " << this->nodeID[nodeIndex]
                      << " has average vonMisesStress = " << avgVonMisesStress
                      << std::endl;
         }
#endif // PRINT_HEADERS
      }
      parameterData->SetTuple1( this->nodeID[ nodeIndex ], avgVonMisesStress );
   }

   this->ugrid->GetPointData()->AddArray( parameterData );

   parameterData->Delete();
}

void ansysReader::ReadElementIndexTable( int elemIndex, int intPosition )
{
   if ( intPosition == 0 )
      return;

#ifdef PRINT_HEADERS
   std::cout << "\nReading Element Index Table at intPosition = "
             << intPosition << " for elementIndex " << i << std::endl;
#endif // PRINT_HEADERS

   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   if ( numValues != 25 ) 
   {
      std::cerr << "numValues = " << numValues << " != 25" << std::endl;
      exit( 1 );
   }

   int ptrEMS = ReadNthInteger( intPosition++ );
   PRINT( ptrEMS );

   int ptrENF = ReadNthInteger( intPosition++ );
   PRINT( ptrENF );

   // pointer to Nodal Stresses
   this->ptrENS[ elemIndex ] = ReadNthInteger( intPosition++ );
   PRINT( this->ptrENS[ elemIndex ] );
   //std::cout << "ptrENS[ " << elemIndex << " ] = " << this->ptrENS[ elemIndex ] << std::endl;

   this->StoreNodalStessesForThisElement( elemIndex );

/*
   // read remainder of the values...
   for ( int i = 0; i < 22; i++ )
   {
      int ptrElement_i = ReadNthInteger( intPosition++ );
      PRINT( ptrElement_i );
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );
*/
}

void ansysReader::StoreNodalStessesForThisElement( int elemIndex )
{
   if ( this->ptrENS[ elemIndex ] == 0 )
      return;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrENS[ elemIndex ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   if ( reportedNumValues != 0 )
   {
      std::cerr << "expected doubles" << std::endl;
      exit( 1 );
   }

   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   if ( numCornerNodesInElement[ elemIndex ] * 11 != numValues )
   {
      std::cerr << "numValues = " << numValues
                << "!= numCornerNodesInElement[ i ] * 11 ="
                << (numCornerNodesInElement[ elemIndex ] * 11) << std::endl;
      exit( 1 );
   }

   for ( int j = 0; j < numCornerNodesInElement[ elemIndex ]; j++ )
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

      int node = this->cornerNodeNumbersForElement[ elemIndex ][ j ];

#ifdef PRINT_HEADERS
      //if ( node == 2108 )
      {
         std::cout << "Node " << node
                   << " on element " << this->elemID[ elemIndex ]
                   << " has stress:" << std::endl;
         for ( int ii = 0; ii < 11; ii++ )
         {
            std::cout << "\tstresses[ " << ii << " ]: "
                      << stresses [ ii ] << std::endl;
         }
      }
#endif // PRINT_HEADERS

      //double vonMisesStress = ComputeVonMisesStress( stresses );
      double vonMisesStress = stresses [ 10 ];

#ifdef PRINT_HEADERS
      //if ( node == 2108 )
      {
         std::cout << "Node " << node
                   << " on element " << this->elemID[ elemIndex ]
                   << " has vonMisesStress = " << vonMisesStress
                   << std::endl;
      }
#endif // PRINT_HEADERS

      // ansys node numbering goes from 1 to numNodes
      if ( node < 1 || node > this->numNodes ) 
      {
         std::cerr << "node = " << node << " is out of range" << std::endl;
         exit( 1 );
      }

      this->summedVonMisesStress [ node ] += vonMisesStress;
      this->numContributingElements[ node ]++; 
      delete [] stresses;
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
   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   if ( numValues != 200 )
   {
      std::cerr << "numValues = " << numValues << " != 200" << std::endl;
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
   VerifyBlock( blockSize_1, blockSize_2 );
}

void ansysReader::ReadGenericBlock( int intPosition )
{
   //std::cout << "\nReading block at intPosition = " << intPosition << std::endl;

   int blockSize_1 = ReadNthInteger( intPosition++ );
   //std::cout << "blockSize_1 = " << blockSize_1 << std::endl;

   int reportedNumValues = ReadNthInteger( intPosition++ );

   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );
   //std::cout << "numValues = " << numValues << std::endl;

   // read the data
   for ( int i = 0; i < numValues; i++ )
   {
      if ( reportedNumValues == 0 )
      {
         double value = ReadNthDouble( intPosition++ );
#ifdef PRINT_HEADERS
         std::cout << "\tvalue[ " << i << " ]: " << value << std::endl;
#endif // PRINT_HEADERS
         intPosition++;   // increase again for doubles only
      }
      else
      {
         int integer = ReadNthInteger( intPosition++ );
#ifdef PRINT_HEADERS
         std::cout << "\tinteger[ " << i << " ]: " << integer << std::endl;
#endif // PRINT_HEADERS
      }
   }
   //std::cout << "after loop, intPosition = " << intPosition << std::endl;

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   //std::cout << "blockSize_2 = " << blockSize_2 << std::endl;
   VerifyBlock( blockSize_1, blockSize_2 );
}

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
   //this->ComputeNodalStresses();
   return this->ugrid;
}

int ansysReader::VerifyNumberOfValues( int reportedNumValues, int blockSize_1 )
{
   int expectedNumValues;
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

void ansysReader::VerifyBlock( int blockSize_1, int blockSize_2 )
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

/*
int ansysReader::GetCornerNodeOnElement( int elementIndex, int nodeIndex )
{
   if ( elementIndex < 0 || elementIndex >= this->numElems ) 
   {
      std::cerr << "elementIndex = " << elementIndex
                << " is out of range" << std::endl;
      return -1;
   }

   // NOTE: elementIndex is not the element ID
   //std::cout << "getting a corner node for element " << this->elemID[ elementIndex ] << std::endl;

   if ( this->ptrElemDescriptions[ elementIndex ] == 0 )
      return -1;

   //std::cout << "\nReading Element Description" << std::endl;

   int intPosition = this->ptrElemDescriptions[ elementIndex ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   int numValues = ReadNthInteger( intPosition++ );

   int mat = ReadNthInteger( intPosition++ );

   int type = ReadNthInteger( intPosition++ );  //important

   //skip over next eight integers...
   //intPosition += 8;
   for ( int i = 0; i < 8; i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

   int numNodesInElement = this->elemDescriptions[ type - 1 ][ 61-1 ];
   PRINT( numNodesInElement );

   int numCornerNodes = this->elemDescriptions[ type - 1 ][ 94-1 ];
   PRINT( numCornerNodes );

   if ( nodeIndex < 0 || nodeIndex >= numCornerNodes ) 
   {
      std::cerr << "nodeIndex = " << nodeIndex
                << " is out of range" << std::endl;
      return -1;
   }

   // allocate space for the node IDs that define the corners of the element
   int * nodes = new int [ numNodesInElement ];

   // read the node IDs that define the element
   if ( fileIO::readNByteBlockFromFile( nodes,
                  sizeof(int), numNodesInElement, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit( 1 );
   }

#ifdef PRINT_HEADERS
   for ( int i = 0; i < numCornerNodes; i++ )
   {  
      std::cout << "\tcornerNodes[ " << i << " ] = " << nodes[ i ] << std::endl;
   }
#endif // PRINT_HEADERS
   int cornerNodeNumber = nodes[ nodeIndex ];
   delete [] nodes;

   // read rest of values
   intPosition += numNodesInElement;
   for ( int i = 0; i < numValues - (10 + numNodesInElement); i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   return cornerNodeNumber;
}
*/

/*
int ansysReader::ElementContainsNode( int elementIndex, int node )
{
   if ( elementIndex < 0 || elementIndex >= this->numElems ) 
   {
      std::cerr << "elementIndex = " << elementIndex
                << " is out of range" << std::endl;
      return 0;
   }

   // NOTE: elementIndex is not the element ID
   //std::cout << "looking at element " << this->elemID[ elementIndex ] << std::endl;

   if ( this->ptrElemDescriptions[ elementIndex ] == 0 )
      return 0;

   //std::cout << "\nReading Element Description" << std::endl;

   int intPosition = this->ptrElemDescriptions[ elementIndex ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );

   int mat = ReadNthInteger( intPosition++ );

   int type = ReadNthInteger( intPosition++ );  //important

   //skip over next eight integers...
   //intPosition += 8;
   for ( int i = 0; i < 8; i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

   int numNodesInElement = this->elemDescriptions[ type - 1 ][ 61-1 ];
   PRINT( numNodesInElement );

   int numCornerNodes = this->elemDescriptions[ type - 1 ][ 94-1 ];
   PRINT( numCornerNodes );

   // ansys node numbering goes from 1 to numNodes
   if ( node < 1 || node > this->numNodes ) 
   {
      std::cerr << "node = " << node << " is out of range" << std::endl;
      return 0;
   }

   // allocate space for the node IDs that define the corners of the element
   int * nodes = new int [ numNodesInElement ];

   // read the node IDs that define the element
   if ( fileIO::readNByteBlockFromFile( nodes,
                  sizeof(int), numNodesInElement, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit( 1 );
   }

   for ( int i = 0; i < numCornerNodes; i++ )
   {  
      //std::cout << "\tcornerNodes[ " << i << " ] = " << nodes[ i ] << std::endl;
      if ( node == nodes[ i ] )
      {
         delete [] nodes;
         return 1;
      }
   }
   delete [] nodes;

   // read rest of values
   intPosition += numNodesInElement;
   for ( int i = 0; i < numValues - (10 + numNodesInElement); i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   return 0;
}
*/

int ansysReader::GetCornerNodeIndex( int elementIndex, int node )
{
   if ( elementIndex < 0 || elementIndex >= this->numElems ) 
   {
      std::cerr << "elementIndex = " << elementIndex
                << " is out of range" << std::endl;
      return -1;
   }

   // ansys node numbering goes from 1 to numNodes
   if ( node < 1 || node > this->numNodes ) 
   {
      std::cerr << "node = " << node << " is out of range" << std::endl;
      return -1;
   }

   // NOTE: elementIndex is not the element ID
   //std::cout << "elementIndex = " << elementIndex << ", looking at element " << this->elemID[ elementIndex ] << std::endl;

   if ( this->ptrElemDescriptions[ elementIndex ] == 0 )
      return -1;

   int intPosition = this->ptrElemDescriptions[ elementIndex ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   
   int numValues = ReadNthInteger( intPosition++ );

   int mat = ReadNthInteger( intPosition++ );

   int type = ReadNthInteger( intPosition++ );  //important

//std::cout << "intPosition = " << intPosition << std::endl;
   //skip over next eight integers...
   //intPosition += 8;  //does NOT work for some reason
   for ( int i = 0; i < 8; i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

//std::cout << "\tintPosition = " << intPosition << std::endl;

   int numNodesInElement = this->elemDescriptions[ type - 1 ][ 61-1 ];
   PRINT( numNodesInElement );

   int numCornerNodes = this->elemDescriptions[ type - 1 ][ 94-1 ];
   PRINT( numCornerNodes );

   // allocate space for the node IDs that define the corners of the element
   int * nodes = new int [ numNodesInElement ];

   // read the node IDs that define the element
   if ( fileIO::readNByteBlockFromFile( nodes,
                  sizeof(int), numNodesInElement, this->s1, this->endian_flip ) )
   {
      std::cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile, so exiting"
           << std::endl;
      exit( 1 );
   }

   for ( int i = 0; i < numCornerNodes; i++ )
   {  
      //std::cout << "\tcornerNodes[ " << i << " ] = " << nodes[ i ] << std::endl;
      if ( node == nodes[ i ] )
      {
         //std::cout << "numCornerNodes = " << numCornerNodes << std::endl;
         delete [] nodes;
         return i;   // success
      }
   }
   delete [] nodes;

   // read rest of values
   intPosition += numNodesInElement;
   for ( int i = 0; i < numValues - (10 + numNodesInElement); i++ )
   {  
      int integer = ReadNthInteger( intPosition++ );
   }

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );

   return -1;
}

double * ansysReader::GetNodalComponentStresses( int elementIndex, int nodeIndex )
{
   if ( this->ptrENS[ elementIndex ] == 0 )
      return NULL;

   int intPosition = this->ptrDataSetSolutions[ 0 ] + this->ptrENS[ elementIndex ];
   int blockSize_1 = ReadNthInteger( intPosition++ );
   int reportedNumValues = ReadNthInteger( intPosition++ );
   if ( reportedNumValues != 0 )
   {
      std::cerr << "expected doubles" << std::endl;
      exit( 1 );
   }

   int numValues = VerifyNumberOfValues( reportedNumValues, blockSize_1 );

   double * stresses = new double [ 11 ];
   // SX, SY, SZ, SXY, SYZ, SXZ, S1, S2, S3, SI, SIGE

   // skip over stresses not of interest...
   intPosition += nodeIndex * 11 * 2;
   for ( int i = 0; i < 11; i++ )
   {
      stresses [ i ] = ReadNthDouble( intPosition++ );
      intPosition++;  //increment again for doubles
#ifdef PRINT_HEADERS
      std::cout << "\tstresses[ " << i << " ]: " << stresses [ i ] << std::endl;
#endif // PRINT_HEADERS
   }
   
/*
   // if want full error checking then must pass by remainder of stress terms
   // ......

   // the last number is blockSize again
   int blockSize_2 = ReadNthInteger( intPosition++ );
   VerifyBlock( blockSize_1, blockSize_2 );
*/

   return stresses;
}
/*
void ansysReader::ComputeNodalStresses()
{
   std::cout << "\nComputing Nodal Stresses" << std::endl;

   vtkFloatArray * parameterData = vtkFloatArray::New();
   // Because the ansys vertices are one-based, increase the arrays by one
   parameterData->SetName( "von Mises stress" );
   parameterData->SetNumberOfComponents( 1 );
   parameterData->SetNumberOfTuples( this->numNodes + 1 );   // note: +1

   //int nodeIndex = 2233; //tets
   //int nodeIndex = 180885; //big model feb2005
   for ( int nodeIndex = 0; nodeIndex < this->numNodes; nodeIndex++ )
   {
      double avgVonMisesStress = 0.0;

      double avgStresses [ 11 ];
      for ( int j = 0; j < 11; j++ )
         avgStresses [ j ] = 0.0;

      int numElementsContainingNode = 0;

      for ( int i = 0; i < this->numElems; i++ )
      {
         int cNodeIndex = GetCornerNodeIndex( i, nodeID[ nodeIndex ] );
         if ( cNodeIndex != -1 )
         {
            std::cout << "element " << this->elemID[ i ]
                      << " contains corner node " << nodeID[ nodeIndex ]
                      << " at index " << cNodeIndex << std::endl;

            double * stresses = GetNodalComponentStresses( i, cNodeIndex );
            if ( stresses != NULL )
            {
               numElementsContainingNode++;

               for ( int j = 0; j < 11; j++ )
               {
                  std::cout << "stresses [ " << j << " ] = "
                            << stresses [ j ] << std::endl;
               }

               for ( int j = 0; j < 11; j++ )
                  avgStresses [ j ] += stresses [ j ];

               double vonMisesStress = ComputeVonMisesStress( stresses );

               std::cout << "Node " << nodeID[ nodeIndex ]
                         << " on element " << this->elemID[ i ]
                         << " has vonMisesStress = " << vonMisesStress
                         << std::endl;

               avgVonMisesStress += vonMisesStress;

               delete [] stresses;
            }
            else
            {
               //std::cout << "stresses == NULL" << std::endl;
            }
         }
      }

      PRINT( numElementsContainingNode );

      std::cout << nodeIndex << "\tFor node " << nodeID[ nodeIndex ] << ", ";
      if ( numElementsContainingNode > 0 )
      {
         //std::cout << std::endl;
         for ( int j = 0; j < 11; j++ )
         {
            avgStresses [ j ] /= numElementsContainingNode;
            std::cout << "\tavgStresses [ " << j << " ] = "
                      << avgStresses [ j ] << std::endl;
         }

         avgVonMisesStress /= numElementsContainingNode;
      }

      std::cout << "avgVonMisesStress = " << avgVonMisesStress
                << std::endl;

      parameterData->SetTuple1( this->nodeID[ nodeIndex ], avgVonMisesStress );
   }

   this->ugrid->GetPointData()->AddArray( parameterData );

   parameterData->Delete();
}
*/

double ansysReader::ComputeVonMisesStress( double stresses [ 11 ] )
{
   // stresses = SX, SY, SZ, SXY, SYZ, SXZ, S1, S2, S3, SI, SIGE
   double vonMisesStress = sqrt ( ( pow( (stresses [ 6 ] - stresses [ 7 ]), 2.0 )
                                  + pow( (stresses [ 7 ] - stresses [ 8 ]), 2.0 )
                                  + pow( (stresses [ 6 ] - stresses [ 8 ]), 2.0 ) )
                                  * 0.5 );
   return vonMisesStress;
}

