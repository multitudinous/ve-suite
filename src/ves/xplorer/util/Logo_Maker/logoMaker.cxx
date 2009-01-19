/*************** <auto-copyright.rb BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <fstream>
#include <iostream>
#include <string>

void ReplaceCharacters( std::string& data, std::string character );
void StripCharacters( std::string& data, std::string character );

int main( int argc, char* argv[] )
{
    if (( argc < 2 ) || ( std::string( argv[ 1 ] ) == "--help" ) )
    {
        std::cout << "Usage : " << argv[ 0 ] << " <filename_without_extension> " << std::endl;
        std::cout << "* Note * The file must be an osg file type." << std::endl;
        return 0;
    }
    std::string filename = std::string( argv[ 1 ] ) + std::string( ".osg" );
    std::ifstream osgFile( filename.c_str() );

    if( !osgFile.good() )
    {
        std::cerr << filename << " could not be opened." << std::endl;
        return 1;
    }

    char lineData[ 1024 ];

    std::string outputFilename;
    outputFilename = std::string( argv[ 1 ] ) + std::string( ".h" );

    std::ofstream hFile( outputFilename.c_str() );

    hFile << "#ifndef GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
    << "#define GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
    << std::endl
    << "#include <string>" << std::endl
    << std::endl
    << "std::string GetVESuite_" << argv[ 1 ] << "( void )" << std::endl
    << "{" << std::endl
    << "  std::string osgData;" << std::endl;


    std::string tempData;
    do
    {
        osgFile.get( lineData, 1024 );
        tempData.append( lineData );
        //This check is here because windows cannot handle strings larger than 16380
        if( tempData.size() > 16000 )
        {
            ReplaceCharacters( tempData, std::string( "\"" ) );
            StripCharacters( tempData, std::string( "\n" ) );
            StripCharacters( tempData, std::string( "\r" ) );
            hFile << "  osgData.append( \"" << tempData << "\" );" << std::endl;
            tempData.erase();
        }
        osgFile.getline( lineData, 1024 );
    }
    while( !osgFile.eof() );
    osgFile.close();

    if( tempData.size() > 0 )
    {
        ReplaceCharacters( tempData, std::string( "\"" ) );
        StripCharacters( tempData, std::string( "\n" ) );
        StripCharacters( tempData, std::string( "\r" ) );
        hFile << "  osgData.append( \"" << tempData << "\" );" << std::endl;
        tempData.erase();
    }


    hFile << "  return osgData;" << std::endl
    << "}" << std::endl
    << "#endif" << std::endl
    << std::endl;
    hFile.close();

    return 0;
}

void ReplaceCharacters( std::string& data, std::string character )
{
    for( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if( index != std::string::npos )
        {
            data.insert( index, std::string( "\\" ) );
            index += 2;
        }
    }
}

void StripCharacters( std::string& data, std::string character )
{
    for( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if( index != std::string::npos )
        {
            data.erase( index, 1 );
            //index+=2;
        }
    }
}

