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
   long position = n * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   int integer;
   if (fileIO::readNByteBlockFromFile( &integer, sizeof(int), 1, this->s1, this->endian_flip ))
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
      exit( 1 );
   }
   return integer;
}

void ansysReader::ReadHeader()
{
   cout << "Reading header" << endl;

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
      cerr << "headerSize = " << headerSize << " != 404, will flip endian flag" << endl;
      this->FlipEndian();
      headerSize = ReadNthInteger( 0 );
      if ( headerSize != 404 ) 
      {
         cerr << "headerSize = " << headerSize << " != 404, will exit" << endl;
         exit( 1 );
      }
   }

   // the ANSYS header is 100 ints long
   this->headerBlockSize = ReadNthInteger( 1 );
   if ( this->headerBlockSize != 100 ) 
   {
      cerr << "headerBlockSize = " << this->headerBlockSize
           << " != 100" << endl;
      exit( 1 );
   }

   int itemNumber;

   //create and null terminate end of 4 character buffer
   char buffer4[ 5 ];
   buffer4[ 4 ] = '\0';
   long position;

   itemNumber = 1;      // file number (12 for results files)
   int fileNumber = ReadNthInteger( itemNumber+1 );
   //cout << "fileNumber = " << fileNumber << endl;
   if ( fileNumber != 12 ) 
   {
      cerr << "fileNumber = " << fileNumber << " != 12" << endl;
      exit( 1 );
   }

   int width = 30;

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
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw(width) << "ANSYS release level = " << "\""
        << buffer4 << "\"" << endl;

   itemNumber = 11;     // date of ANSYS release
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, this->s1);
   cout << setw(width) << "date of ANSYS release = " << "\""
        << buffer4 << "\"" << endl;

   // item number 12-14 is machine identifier
   cout << setw(width) << "machine identifier = " << "\"";
   for ( itemNumber = 12; itemNumber <= 14; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 15-16 is jobname
   cout << setw(width) << "short form of jobname = " << "\"";
   for ( itemNumber = 15; itemNumber <= 16; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 17-18 is ANSYS product name
   cout << setw(width) << "ANSYS product name = " << "\"";
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
   cout << setw(width) << "ANSYS special version label = " << "\""
        << buffer4 << "\"" << endl;

   // item number 20-22 is username
   cout << setw(width) << "username = " << "\"";
   for ( itemNumber = 20; itemNumber <= 22; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 23-25 is machine identifier
   cout << setw(width) << "machine identifier = " << "\"";
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
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 41-60 is main analysis title 
   cout << setw(width) << "main analysis title = " << "\"";
   for ( itemNumber = 41; itemNumber <= 60; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(this->s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, this->s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 61-80 is first subtitle
   cout << setw(width) << "first subtitle = " << "\"";
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
   cout << setw(width) << "split point of the file = " << splitPoint << endl;

   // item number 97-98 is filesize at write
   itemNumber = 97;
   position = (itemNumber+1) * sizeof(int);
   fseek(this->s1,position,SEEK_SET);
   long filesize;
   if (fileIO::readNByteBlockFromFile( &filesize, sizeof(long), 1,
                                       this->s1, this->endian_flip ))
   {
      cerr << "Error reading \'filesize at write\', so exiting" << endl;
      exit( 1 );
   }
   else
      cout << setw(width) << "filesize at write = " << filesize << endl;

   // the number at integer position 102 is 404
   headerSize = ReadNthInteger( 102 );
   if ( headerSize != 404 ) 
   {
      cerr << "headerSize = " << headerSize << " != 404" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadSecondBlock()
{
   cout << "Reading second block" << endl;

   // we have now read headerSize, headerBlockSize, headerSize
   int position = this->headerBlockSize + 3;

   // the number at the next integer position 164
   int blockSize = ReadNthInteger( position++ );
   if ( blockSize != 164 ) 
   {
      cerr << "blockSize = " << blockSize << " != 16" << endl;
      exit( 1 );
   }
   
   // this block is 40 ints long
   this->secondBlockSize = ReadNthInteger( position++ );
   if ( this->secondBlockSize != 40 ) 
   {
      cerr << "secondBlockSize= " << this->secondBlockSize << " != 40" << endl;
      exit( 1 );
   }

   // read all integers
   for ( int i=0; i < this->secondBlockSize; i++ )
   {
      int integer = ReadNthInteger( position++ );
      cout << "\tinteger[ " << i << " ]: " << integer << endl;
   }

   // the last number is blockSize again
   blockSize = ReadNthInteger( position++ );
   if ( blockSize != 164 ) 
   {
      cerr << "blockSize = " << blockSize << " != 164" << endl;
      exit( 1 );
   }
}

void ansysReader::ReadThirdBlock()
{
   cout << "Reading third block" << endl;

   // we have now read headerSize, headerBlockSize, headerSize
   int position = this->headerBlockSize + 3 + this->secondBlockSize + 3;

   // the number at the next integer position 16 
   int blockSize = ReadNthInteger( position++ );
   if ( blockSize != 16 ) 
   {
      cerr << "blockSize = " << blockSize << " != 16" << endl;
      exit( 1 );
   }
   
   // this block is 3 ints long
   this->thirdBlockSize = ReadNthInteger( position++ );
   if ( this->thirdBlockSize != 3 ) 
   {
      cerr << "thirdBlockSize = " << this->thirdBlockSize << " != 3" << endl;
      exit( 1 );
   }

   // read all integers
   for ( int i=0; i < this->thirdBlockSize; i++ )
   {
      int integer = ReadNthInteger( position++ );
      cout << "\tinteger[ " << i << " ]: " << integer << endl;
   }

   // the last number is blockSize again
   blockSize = ReadNthInteger( position++ );
   if ( blockSize != 16 ) 
   {
      cerr << "blockSize = " << blockSize << " != 164" << endl;
      exit( 1 );
   }
}
