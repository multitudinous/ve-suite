/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#include <fstream>
#include <iostream>
#include <string>

////////////////////////////////////////////////////////////////////////////////
int main( int argc, char* argv[] )
{
    if (( argc < 2 ) || ( std::string( argv[ 1 ] ) == "--help" ) )
    {
        std::cout << "Usage : " << argv[ 0 ] << " <filename> " << std::endl;
        return 0;
    }
    std::string filename = std::string( argv[ 1 ] );
    std::ifstream osgFile( filename.c_str() );
    if( !filename.compare( 0, 2, std::string( "./" ) ) )
    {
        filename = filename.erase( 0, 2 );
    }

    size_t indexOfFirstDot = filename.find_last_of( '.' );
    std::string shortName( filename.begin(), filename.begin() + indexOfFirstDot );
    //replace . / - with _
    for( size_t index = 0; index < shortName.length(); )
    {
        index = shortName.find( '.', index );
        if( index != std::string::npos )
            shortName[ index ] = '_';
    }

    for( size_t index = 0; index < shortName.length(); )
    {
        index = shortName.find( '/', index );
        if( index != std::string::npos )
            shortName[ index ] = '_';
    }

    for( size_t index = 0; index < shortName.length(); )
    {
        index = shortName.find( '-', index );
        if( index != std::string::npos )
            shortName[ index ] = '_';
    }
    /*
    // Sample for C++ File I/O binary file read

    void read_from_binary_file()
    {
        WebSites p_Data;
        fstream binary_file("c:\\test.dat",ios::binary|ios::in);
        binary_file.read(reinterpret_cast<char *>(&p_Data),sizeof(WebSites));
        binary_file.close();
        
        cout<<p_Data.SiteName<<endl;
        cout<<"Rank :"<< p_Data.Rank<<endl;
        
    }

    fstream binary_file("c:\\test.dat",ios::out|ios::binary|ios::app); 
    binary_file.write(reinterpret_cast<char *>(&p_Data),sizeof(WebSites));
    binary_file.close();
    */

    if( !osgFile.good() )
    {
        std::cerr << filename << " could not be opened." << std::endl;
        return 1;
    }

    char lineData[ 1024 ];

    std::string outputFilename;
    outputFilename = shortName + std::string( ".h" );

    std::ofstream hFile( outputFilename.c_str(), std::ios::binary );

    hFile << "#ifndef GETVESUITE_" << shortName << "_H" << std::endl
    << "#define GETVESUITE_" << shortName << "_H" << std::endl
    << "//Usage of this file" << std::endl
    << "//std::istringstream tempStreamI( GetVESuite_whatever() );" << std::endl
    << "//osg::ref_ptr< osg::Node > tempNode = osgDB::Registry::instance()->" << std::endl
    << "//    getReaderWriterForExtension( \"ive\" )->readNode( tempStreamI ).getNode();" << std::endl
    << "//osg::ref_ptr< osg::Image > tempImage = osgDB::Registry::instance()->" << std::endl
    << "//    getReaderWriterForExtension( \"png\" )->readNode( tempStreamI ).getImage();" << std::endl
    << std::endl
    << "#include <string>" << std::endl
    << std::endl
    << "std::string GetVESuite_" << shortName << "( void )" << std::endl
    << "{" << std::endl
    << "    unsigned char osgData";


    //get size of binary file
    osgFile.seekg( 0, std::ios::end );
    size_t binaryFileSize = osgFile.tellg();
    //rewind
    osgFile.seekg( 0, std::ios::beg );

    char* dataBuffer = new char[ binaryFileSize ];
    osgFile.read( dataBuffer, binaryFileSize );
    osgFile.close();

    hFile << "[ " << binaryFileSize << " ] = ";
    hFile << "{ " << std::endl;
    unsigned char temp = dataBuffer[ 0 ];
    hFile << "        " << static_cast< unsigned int >( temp );
    for( size_t i = 1; i < binaryFileSize; ++i )
    {
        temp = dataBuffer[ i ];
        hFile << "," << static_cast< unsigned int >( temp );
        //This check is here because windows cannot handle strings larger than 16380
        if( !( i % 16000 ) )
        {
            hFile << std::endl;
            hFile << "        ";
        }
    }

    hFile << std::endl << "        };" << std::endl;
    hFile << "    std::string strOsgData;" << std::endl;
    hFile << "    for( size_t i = 0; i < " << binaryFileSize << "; ++i )" << std::endl;
    hFile << "    {" << std::endl;
    hFile << "        strOsgData.push_back( static_cast< char >( osgData[ i ] ) );" << std::endl;
    hFile << "    }" << std::endl;

    hFile << "    return strOsgData;" << std::endl
    << "}" << std::endl
    << "#endif" << std::endl
    << std::endl;
    hFile.close();

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
