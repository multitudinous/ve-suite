/*************** <auto-copyright.pl BEGIN do not edit this line> *************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-06-15 11:06:13 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8206 $
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
    if( (argc < 2) || (std::string( argv[ 1 ] ) == "--help") )
    {
       std::cout << "Usage : " << argv[ 0 ] << " <filename_without_extension> " << std::endl;
       std::cout << "* Note * The file must be an osg file type." << std::endl;
       return 0;
    }
    std::string filename = std::string( argv[ 1 ] ) + std::string( ".ive" );
    std::ifstream osgFile( filename.c_str(), std::ios::binary );
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
    outputFilename = std::string( argv[ 1 ] ) + std::string( ".h" );

    std::ofstream hFile( outputFilename.c_str() );
   
    hFile << "#ifndef GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
         << "#define GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
         << std::endl
         << "#include <string>" << std::endl
         << std::endl
         << "char* GetVESuite_" << argv[ 1 ] << "( void )" << std::endl
         << "{" << std::endl
         << "    char* osgData = 0;" << std::endl;
 

    //get size of binary file
    osgFile.seekg (0, std::ios::end);
    size_t binaryFileSize = osgFile.tellg();
    //rewind
    osgFile.seekg (0, std::ios::beg);
    
    char* dataBuffer = new char[ binaryFileSize ];
    osgFile.read( dataBuffer, binaryFileSize );
    osgFile.close();
    
    hFile << "    osgData = new char[ " << binaryFileSize << " ];" << std::endl;
    hFile << "    osgData = { " << std::endl;
    hFile << "        " << static_cast< unsigned int >( dataBuffer[ 0 ] );
    for( size_t i = 1; i < binaryFileSize; ++i )
    {
        hFile << "," << static_cast< unsigned int >( dataBuffer[ i ] );
        //This check is here because windows cannot handle strings larger than 16380
        if( !(i%16000) )
        {
            hFile << std::endl;
            hFile << "        ";
        }
    }
    hFile << std::endl << "        };" << std::endl;
    hFile << "    return osgData;" << std::endl
         << "}" << std::endl
         << "#endif" << std::endl
         << std::endl;
    hFile.close();

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
