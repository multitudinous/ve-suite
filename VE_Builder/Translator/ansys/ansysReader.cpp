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

#include <iostream>
#include <cstdio>
#include "fileIO.h"

using namespace std;

int ReadNthInteger( int n, FILE *s1, bool endian_flip )
{
   long position = n * sizeof(int);
   fseek(s1,position,SEEK_SET);
   int integer;
   if (fileIO::readNByteBlockFromFile( &integer, sizeof(int), 1, s1, endian_flip ))
   {
      cerr << "ERROR: bad read in fileIO::readNByteBlockFromFile" << endl;
      exit( 1 );
   }
   return integer;
}

void ReadHeader( char * ansysFileName, int debug )
{
   // open file
   FILE *s1;
   if((s1=fopen(ansysFileName,"r"))==NULL)
   {
      cerr << "ERROR: can't open file \"" << ansysFileName
           << "\", so exiting" << endl;
   }
   
   bool endian_flip = 1;

   cout << "\nReading data file \"" << ansysFileName << "\"" << endl;

/*
   // read all integers
   int intArray[ 100 ];
   for ( int i=0; i < 100; i++ )
   {
      intArray[ i ] = ReadNthInteger( i, s1, endian_flip );
      cout << "\tintArray[ " << i << " ]: " << intArray[ i ] << endl;
   }
*/

   // the very first number is the integer 404
   int headerSize = ReadNthInteger( 0, s1, endian_flip );
   cout << "headerSize = " << headerSize << endl;
   if ( headerSize != 404 ) 
   {
      cerr << "headerSize = " << headerSize << " != 404" << endl;
      exit( 1 );
   }

   // the ansys header is 100 ints long
   int numInts = ReadNthInteger( 1, s1, endian_flip );
   if ( numInts != 100 ) 
   {
      cerr << "numInts = " << numInts << " != 100" << endl;
      exit( 1 );
   }

   int itemNumber;
   //create and null terminate end of 4 character buffer
   char buffer4[ 5 ];
   buffer4[ 4 ] = '\0';
   long position;

   // item number 1 is file number (12 for results files)
   itemNumber = 1;
   int fileNumber = ReadNthInteger( itemNumber+1, s1, endian_flip );
   //cout << "fileNumber = " << fileNumber << endl;
   if ( fileNumber != 12 ) 
   {
      cerr << "fileNumber = " << fileNumber << " != 12" << endl;
      exit( 1 );
   }

   // item number 2 is file format
   itemNumber = 2;
   int fileFormat = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "fileFormat = " << fileFormat << " (0=internal,1=external)" << endl;

   // item number 3 is time
   itemNumber = 3;
   int time = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "time = " << time << endl;

   // item number 4 is date
   itemNumber = 4;
   int date = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "date = " << date << endl;

   // item number 5 is units
   itemNumber = 5;
   int units = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "units = " << units << " (0=user-defined,1=SI,2=CSG,3=feet,4=inches)"<< endl;

   // item number 10 is ansys release level
   itemNumber = 10;
   position = (itemNumber+1) * sizeof(int);
   fseek(s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, s1);
   cout << "ansys release level: \"" << buffer4 << "\"" << endl;

   // item number 11 is date of ansys release
   itemNumber = 11;
   position = (itemNumber+1) * sizeof(int);
   fseek(s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, s1);
   cout << "date of ansys release: \"" << buffer4 << "\"" << endl;

   // item number 12-14 is machine identifier
   cout << "machine identifier: \"";
   for ( itemNumber = 12; itemNumber <= 14; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 15-16 is jobname
   cout << "jobname: \"";
   for ( itemNumber = 15; itemNumber <= 16; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 17-18 is ANSYS product name
   cout << "ANSYS product name: \"";
   for ( itemNumber = 17; itemNumber <= 18; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 19 is ANSYS special version label
   itemNumber = 19;
   position = (itemNumber+1) * sizeof(int);
   fseek(s1,position,SEEK_SET);
   fread(buffer4, sizeof(char), 4, s1);
   cout << "ANSYS special version label: \"" << buffer4 << "\"" << endl;

   // item number 20-22 is username
   cout << "username: \"";
   for ( itemNumber = 20; itemNumber <= 22; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 23-25 is machine identifier
   cout << "machine identifier: \"";
   for ( itemNumber = 23; itemNumber <= 25; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 26 is system record size
   itemNumber = 26;
   int systemRecordSize = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "system record size = " << systemRecordSize << endl;

   // item number 27 is maximum file length
   itemNumber = 27;
   int maximumFileLength = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "maximum file length = " << maximumFileLength << endl;

   // item number 28 is maximum record size
   itemNumber = 28;
   int maximumRecordSize = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "maximum record size = " << maximumRecordSize << endl;

   // item number 31-38 is jobname
   cout << "jobname: \"";
   for ( itemNumber = 31; itemNumber <= 38; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 41-60 is main analysis title 
   cout << "main analysis title: \"";
   for ( itemNumber = 41; itemNumber <= 60; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 61-80 is first subtitle
   cout << "first subtitle: \"";
   for ( itemNumber = 61; itemNumber <= 80; itemNumber++ )
   {
      position = (itemNumber+1) * sizeof(int);
      fseek(s1,position,SEEK_SET);
      fread(buffer4, sizeof(char), 4, s1);
      cout << buffer4;
   }
   cout << "\"" << endl;

   // item number 95 is split point of the file
   itemNumber = 95;
   int splitPoint = ReadNthInteger( itemNumber+1, s1, endian_flip );
   cout << "split point of the file = " << splitPoint << endl;

   // item number 97-98 is filesize at write
   itemNumber = 97;
   position = (itemNumber+1) * sizeof(int);
   fseek(s1,position,SEEK_SET);
   long filesize;
   if (fileIO::readNByteBlockFromFile( &filesize, sizeof(long), 1, s1, endian_flip ))
   {
      cerr << "Error reading \'filesize at write\', so exiting" << endl;
      exit( 1 );
   }
   else
      cout << "filesize at write = " << filesize << endl;

   // the number at integer position 102 is 404
   headerSize = ReadNthInteger( 102, s1, endian_flip );
   //cout << "headerSize = " << headerSize << endl;
   if ( headerSize != 404 ) 
   {
      cerr << "headerSize = " << headerSize << " != 404" << endl;
      exit( 1 );
   }
}

int main()
{
   ReadHeader( "test_case.rst", 1 );
   return 0;
}
