/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: fileIO.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef FILEIO_H
#define FILEIO_H

#include <iosfwd>
#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"


namespace VE_Util
{
class VE_UTIL_EXPORTS fileIO
{ 
public:
   fileIO();
   ~fileIO();
   static int isFileReadable( const std::string filename );
   static int isFileWritable(  std::string filename );
   static int DirectoryExists( std::string dirName );
   static int isDirWritable( const std::string dirname );
   static const std::string getWritableDir( void );
   static std::string getFilenameFromDefault( std::string, std::string );
   static std::string getReadableFileFromDefault( 
                                   std::string stringDescribingfileContents, 
                                   const std::string defaultName );
   static std::string getWritableFile( std::string defaultName );
   static int readNByteBlockFromFile( void *ptr, const unsigned int nByte,
                                   const unsigned int num, FILE *stream, 
                                   const bool endian_flip = 1 );
   static void processCommandLineArgs( int argc, char *argv[], char verb[],
         std::string & inFileName, std::string & outFileName );
   static std::string getExtension( std::string filename );
   static void readToFileEnd( FILE *inputFile );
   static void StripTrailingSpaces( std::string line );
   static std::string StripLeadingSpaces( std::string line );
   static int extractIntegerBeforeExtension( std::string filename );
   static void IdentifyTagAssignValue(std::string TagName, std::string TagValue);
   static void getTagAndValue(std::string textline, std::string& TagName,  std::string& TagValue);

   static int getIntegerBetween( const int min, const int max );

   static std::string GetFile( std::string, std::string );
   static int ExtractIntegerFromString( std::string filename );
   static int ExtractIntegerBeforeExtension( std::string filename );
   static std::vector<std::string> fileIO::GetFilesInDirectory(std::string dir, std::string extension);
};
}
#endif
