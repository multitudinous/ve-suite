/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
#include "VE_Xplorer/cfdConfig.h"

class WXPLUGIN_DECLSPEC fileIO
{ 
   public:
      fileIO();
      ~fileIO();
      static int isFileReadable( const char * const filename );
      static int isFileWritable( char *filename );
      static int DirectoryExists( char * dirName );
      static int isDirWritable( char *dirname );
      static char * getWritableDir( );
      static char * getFilenameFromDefault( char* fileContents,
                                            char* defaultName );
      static char * getReadableFileFromDefault( 
                                         char* stringDescribingfileContents, 
                                         const char* const defaultName );
      static char * getWritableFile( char* defaultName );
      static int readNByteBlockFromFile( void *ptr, const unsigned int nByte,
                                         const unsigned int num, FILE *stream, 
                                         const bool endian_flip = 1 );
      static void processCommandLineArgs( int argc, char *argv[], char verb[],
               char * & inFileName, char * & outFileName );
      static char * getExtension( char * filename );
      static void readToFileEnd( FILE *inputFile );
      static void StripTrailingSpaces( char line [] );
      static char * StripLeadingSpaces( char line [] );
      static int extractIntegerBeforeExtension( char filename [] );
      static void IdentifyTagAssignValue(char *TagName, char *TagValue);
      static void getTagAndValue(char *textline, char *TagName, char *TagValue);
 
      static int getIntegerBetween( const int min, const int max );

      static char * GetFile( char [], char [] );
      static int ExtractIntegerFromString( char filename [] );
      static int ExtractIntegerBeforeExtension( char filename [] );
};
#endif
