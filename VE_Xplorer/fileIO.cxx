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
 * File:          $RCSfile: fileIO.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "fileIO.h"
#include <string.h>
#include <iostream>
#include <fstream>
#include <stdlib.h>
#include <sys/stat.h>

fileIO::fileIO( )
{
}

fileIO::~fileIO( )
{
}

int fileIO::isFileReadable( const char * const filename )
{
   std::ifstream fileIn( filename, std::ios::in ); 
   if ( ! fileIn )
      return 0;   // failure

   fileIn.close();
   return 1;      // success
}

int fileIO::isFileWritable( char *filename )
{
/*
   // THIS CODE WILL MESS UP A FILE IF ALREADY OPEN
   // NEED A BETTER WAY

   //"a" means append -- don't erase existing file
   FILE *fileID = fopen( filename, "a" );
   if ( !fileID ) return 0;    // failure
   fclose( fileID );
   remove( filename );
*/
   return 1;                   // success
}

int fileIO::isDirWritable( char *dirname )
{
   char file [] = {"/testing.txt"};
   char * filename = new char [ strlen(dirname)+strlen(file)+1 ];
   strcpy( filename, dirname );
   strcat( filename, file );
   //std::cout << "test file is \"" << filename << "\"" << std::endl;

   //"a" means append -- don't erase existing file
   FILE *fileID = fopen( filename, "a" );
   if ( !fileID )
   {
      delete [] filename;
      return 0;               // failure
   }
   fclose( fileID );
   remove( filename );
   delete [] filename;
   return 1;                  // success
}

char * fileIO::getWritableDir( )
{
   char * dirName;
   do
   {
      std::cout << "Input an existing writable directory: ";
      std::cout.flush();
      char tempName [1024];
      std::cin >> tempName;
      std::cin.ignore();
      dirName = new char[ strlen(tempName) + 1 ];
      strcpy( dirName, tempName );

      if ( isDirWritable( dirName ) )
         break;
      else
      {
         std::cerr << "\nERROR: Can't write to \"" << dirName
                   << "\" directory" << std::endl;
         delete [] dirName;
      }
   } while( 1 );
   return dirName;
}

char * fileIO::getFilenameFromDefault( char* fileContents, char* defaultName )
{
/*
   std::cout << "\nThe default name for " << fileContents << " is \"" 
             << defaultName << "\"" << std::endl;
*/

   int answer = 1;

   // if supplied with a default name, ask if it is OK...
   if ( strcmp(defaultName,"") )
   {
      std::cout << "\nThe default name for " << fileContents 
                << " is \"" << defaultName << "\"" << std::endl;
      std::cout << "Do you want to change it ? (0) Use default (1) Change" 
                << std::endl;
      answer = getIntegerBetween( 0, 1 );
   }

   char * fileName = NULL;
   if ( answer == 1 )
   {
      char tempName [1024];
      std::cout << "Enter filename for " << fileContents << ": ";
      std::cin >> tempName;
      std::cin.ignore();
      fileName = new char[ strlen(tempName) + 1 ];
      strcpy( fileName, tempName );
   }
   else
   {
      fileName = new char[ strlen(defaultName)+1 ];
      strcpy( fileName, defaultName );
   }

   return fileName;
}

char * fileIO::getReadableFileFromDefault( char* stringDescribingfileContents,
                                           const char* const defaultName )
{
    // initialize return name with default name, and if not readable set to null
    char validDefaultName [ 256 ];
    if ( ! isFileReadable( defaultName ) )
       strcpy( validDefaultName, "" );
    else
       strcpy( validDefaultName, defaultName );

    char * filename;
    do
    {
        filename = getFilenameFromDefault( stringDescribingfileContents,
                                           validDefaultName );

        if ( ! isFileReadable(filename) )
        {
            std::cerr << "\n\"" << filename << "\" is not readable."
                      << std::endl;
            delete [] filename; 
            filename = NULL;
        }
    }
    while ( ! filename );

    return filename;
}

char * fileIO::getWritableFile( char* defaultName )
{
    char* filename;
/*
    // initialize return name with default name, and if not writable set to null
    // commented out because this will leave a small empty file if user doesn't accept the default
    if( ! isFileWritable(defaultName) ) strcpy(defaultName,"");
*/

    do
    {
        filename = getFilenameFromDefault( "output", defaultName );

        if( ! isFileWritable(filename) )
        {
            std::cerr << "\n\"" << filename << "\" is not writable." << std::endl;
            delete [] filename; 
            filename = NULL;
        }
    }
    while ( ! filename );

    return filename;
}

int fileIO::readNByteBlockFromFile(void *ptr, const unsigned int nByte, const unsigned int num, FILE *stream, const bool endian_flip)
{
    if ( feof(stream) ) return(1);
    
    // num is the number of nByte byte blocks to be read
    if (fread(ptr,nByte,num,stream) != num) return(1);

    // do we need to endian flip????
    if(endian_flip)
    {
        char * buf = new char [nByte];
        for(unsigned int i=0; i < num*nByte; i += nByte)
        {
            for(unsigned int j=0; j<nByte; j++) buf[nByte-1-j] =* (((char*)ptr)+i+j);
            memcpy(((char*)ptr)+i,buf,nByte);
        }
    }
    return(0);  //success
}

void fileIO::processCommandLineArgs( int argc, char *argv[], char verb[],
                                     char * & inFileName, char * & outFileName )
{
   if (argc > 1)
   {
      inFileName = new char [ strlen(argv[1])+1 ];
      strcpy( inFileName, argv[1] );

      if (argc > 2)
      {
         outFileName = new char [ strlen(argv[2])+1 ];
         strcpy( outFileName, argv[2] );
      }
      else
      {
         if ( outFileName == NULL )
            outFileName = fileIO::getWritableFile( "outFile.vtk" );
         else
         {
            char * defaultName = new char [ strlen(outFileName)+1 ];
            strcpy( defaultName, outFileName );
            delete [] outFileName;
            outFileName = fileIO::getWritableFile( defaultName );
            delete [] defaultName;
         }
      }

      // if more than three arguments are on the commandline,
      // then assume that a shell script is in use and don't ask to verify
      if (argc > 3)
         return;

      char response;
      do 
      {
         std::cout << "\nSo you want to " << verb << " " << inFileName 
              << " and save to " << outFileName << "? (y/n/q): ";
         std::cin >> response;
         std::cin.ignore();
      } while (response != 'y' && response != 'Y' && 
      response != 'n' && response != 'N' &&
      response != 'q' && response != 'Q' );

      if (response == 'q' || response == 'Q')
      {
         inFileName = NULL;
         outFileName = NULL;
         return;
      }

      // if anything other than y/Y was input then reset argc to get
      // filenames from user...
      if (response != 'y' && response != 'Y')
         argc = 1;
   }

   if (argc == 1)  // then get filenames from user...
   {
      char phrase [] = {"input"};
      char * tempText = new char [ strlen(phrase)+1 ];
      strcpy( tempText, phrase );
      inFileName = fileIO::getReadableFileFromDefault( tempText, "inFile.vtk" );
      outFileName = fileIO::getWritableFile( "outFile.vtk" );
      delete [] tempText;
   }
}

char * fileIO::getExtension( char * filename )
{
   int len = strlen( filename );
   //std::cout << "len = " << len << std::endl;
   int i, foundPeriod = 0;
   char * extension = NULL;
   for(i=len-1; i >= 0; i-- )
   {
      //std::cout << "filename[" << i << "] = " << filename[i] << std::endl;
      if ( filename[i] == '.' )
      {
         //std::cout << "found period" << std::endl;
         foundPeriod = 1;
         i++;   //increment i to eliminate the period
         break;
      }
   }

   if ( foundPeriod )
   {
      //std::cout << "filename[i] " << filename[i] << std::endl;
      len = strlen( &filename[i] );
      //std::cout << "len = " << len << std::endl;
      extension = new char [len];
      strcpy( extension, &filename[i] );
      //std::cout << "extension = \"" << extension << "\"" << std::endl;
   }

   return extension;
}

void fileIO::readToFileEnd( FILE *inputFile )
{
   // read to file end
   float junk2;
   for (int i=0; i<100000000; i++)
   {
      if ( fileIO::readNByteBlockFromFile( &junk2, sizeof(float), 1, inputFile ) )
      {
         std::cout << "end of file found after reading " << i 
               << " more floats" << std::endl;
         break;
      }
      else std::cout << "junk2 = " << junk2 << std::endl;
   }
}

void fileIO::StripTrailingSpaces( char line [] )
{ 
   //std::cout << "strlen(line) = " << strlen(line) << std::endl;
   for (int i = strlen(line) - 1; i >= 0; i--)
   {
      if (line[i] != ' ' ) break;
      line[i] = '\0';
   }
}
char * fileIO::StripLeadingSpaces( char line [] )
{ 
   //std::cout << "StripLeadingSpaces has \"" << line << "\"" << std::endl;
   char * shortLine;
   //std::cout << "strlen(line) = " << strlen(line) << std::endl;
   for (int i =  0; i < (int)strlen(line); i++ )
   {
      if (line[i] != ' ' )
      {
         shortLine = new char [ strlen(line) - i ];
         strcpy( shortLine, &line[i] );
         //std::cout << "1 returning \"" << shortLine << "\"" << std::endl;
         return shortLine;
      }
   }
   shortLine = new char [ 1 ];
   shortLine[0] = '\0';
   //std::cout << "2 returning \"" << shortLine << "\"" << std::endl;
   return shortLine;
}

int fileIO::extractIntegerBeforeExtension( char filename [] )
{
   char * changeable_filename = new char [ strlen(filename)+1 ];
   strcpy( changeable_filename, filename );

   // last token will be the extension
   // secondLastToken will be the integer counter
   char lastToken[100], secondLastToken[100];
   lastToken[0] = '\0';
   secondLastToken[0] = '\0';

   char excludes[] = "-_.";   // define the allowable separators 
   char * pch = strtok( changeable_filename, excludes );
   while ( pch != NULL )
   {
      //printf ("%s\n",pch);
      strcpy( secondLastToken, lastToken );
      strcpy( lastToken, pch );
      pch = strtok( NULL, excludes );
   }
   //std::cout << "secondLastToken = " << secondLastToken << std::endl;
   delete [] changeable_filename;
   return atoi(secondLastToken);
}

void fileIO::getTagAndValue(char *textline, char *TagName, char *TagValue)
{
   //pre: param file must have TAGNAME=TAGVALUE on each line, TAGVALUE cannot
   //     have any spaces in it
   //mod: extracts TAGNAME from .param file and stores in TagName, same for TagValue
   //post: none
   int i = 0;
   int j = 0;
   
   while(textline[i] != '=')
   {
      TagName[j++] = textline[i++];
   }
   TagName[j] = '\0';

   j = 0;
   i++;
   while(textline[i]  >= 33 && textline[i] <= 126)
   {
      TagValue[j++]  = textline[i++];
   }
   TagValue[j] = '\0';

   return;
}
void fileIO::IdentifyTagAssignValue(char *TagName, char *TagValue )
{
   if (strcmp("STARVRT", TagName)==0)
   {
      std::cout << "Yes " << TagValue << std::endl;
   } else {
      std::cout << " " << std::endl;
   }
}

int fileIO::getIntegerBetween( const int min, const int max )
{
   int value = 0;
   char string[100];
   string[0] = '\0';
   int index;

   // repeat while value is not between min and max inclusive
   do 
   {
      index = 0;
      int haveNumber = 0;
      int finishedNumber = 0;
      std::cout << "Enter value : ";
      std::cout.flush();
      while(1) {
         char c = std::cin.get();
         //std::cout << "(int)c = " << (int)c << std::endl;
         if ( c == '\n' ) // ENTER key pressed ( c == 10 works too )
         {
            //std::cout << "ENTER key pressed" << std::endl;
            string[index] = '\0';
            break;
         }
         else if ( haveNumber && c == ' ' ) // SPACE key pressed
         {
            //std::cout << "SPACE key pressed" << std::endl;
            // read an entry of "1  2" as 1 instead of 12
            string[index] = '\0';
            finishedNumber = 1;
         }
         else if ( ! haveNumber && c == '-' ) // minus sign pressed
         {
            //std::cout << "minus sign pressed" << std::endl;
            haveNumber = 1;
            string[index] = c;
            index++;
         }
         else if ( ! finishedNumber )
         {
            // check if key entered is between 0 - 9.
            if ( (c - '0' >= 0) && (c - '0' <= 9) ) 
            {
               string[index] = c;
               index++;
               haveNumber = 1;
            }
            else //if ( (c  >= 'A') && (c  <= 'z') ) 
            {
               //std::cout << "A-z pressed" << std::endl;
               string[0] = ' ';  // this will force a new loop
               string[1] = '\0';
               index = 2;
               finishedNumber = 1;
            }
         }
      } // end while

      if (!strcmp( string, "" ) )   // if only ENTER was pressed
      {
         //std::cout << "ENTER key pressed" << std::endl;
         //return defaultVal;         // return defaultVal
      }
      else
      {
         StripTrailingSpaces( string );
         char * shortString = StripLeadingSpaces( string );
         strcpy( string, shortString );
         if (!strcmp( string, "" ) )   // if all spaces
            index = 0;                 // force a new loop
         else if (!strcmp( string, "-" ) )   // if only neg sign
            index = 0;                       // force a new loop
         else
         {
            // convert array of chars to integer
            value = atoi(string);
         }
         delete [] shortString;
      }
      //std::cout << "value = " << value << std::endl;
   } while ( index == 0 || (value < min || value > max ) );

   return value;
}

char * fileIO::GetFile( char fileKeyword[], char fileLocation[] )
{
   char * file = new char [100];
   file[0] = '\0';

   char* path = getenv("VE_SUITE_HOME");
   //std::cout << "path is \"" << path << "\"" << std::endl;
   if(path != NULL)
   {
      strcpy( file, path );
      strcat( file, fileLocation );
   }
   else
   {
      std::cerr << "ERROR: environment variable VE_SUITE_HOME is not defined"
                << std::endl;
      return NULL; 
   }

   if ( ! fileIO::isFileReadable( file ) )
   {
      std::cerr << "ERROR: Can't read the " << fileKeyword 
                << " file named \"" << file << "\"" << std::endl;
      return NULL; 
   }
   else
   {
      std::cout << "|   Found " << fileKeyword << " file" << std::endl;
   }
   
   return file;
}

