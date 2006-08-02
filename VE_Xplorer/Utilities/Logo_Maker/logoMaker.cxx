#include <fstream>
#include <iostream>

#include "Triangles.h"

void StripCharacters( std::string& data, std::string character );

int main( int argc, char* argv[] )
{
   if ( (argc < 2) || (std::string( argv[ 1 ] ) == "--help") )
   {
      std::cout << "Usage : " << argv[ 0 ] << " <filename_without_extension> " << std::endl;
      std::cout << "* Note * The file must be an osg file type." << std::endl;
      return 0;
   }
   std::string filename = std::string( argv[ 1 ] ) + std::string( ".osg" );
   std::ifstream osgFile( filename.c_str() );
   char lineData[ 1024 ];

   std::string outputFilename;
   outputFilename = std::string( argv[ 1 ] ) + std::string( ".h" );

   std::ofstream hFile( outputFilename.c_str() );
   
   hFile << "#ifndef GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
         << "#define GETVESUITE_" << argv[ 1 ] << "_H" << std::endl
         << std::endl
         << "#include <string>" << std::endl
         << std::endl
         << "std::istream GetVESuite_" << argv[ 1 ] << "( void )" << std::endl
         << "{" << std::endl
         << "  std::string osgData;" << std::endl;
 

   do
   {
      osgFile.get( lineData, 1024 );
      std::string tempData( lineData );
      StripCharacters( tempData, std::string( "\"" ) );
      hFile << "  osgData.append( \"" << tempData << "\" );" << std::endl;
      osgFile.getline( lineData, 1024 );
   }
   while( !osgFile.eof() );
   osgFile.close();

   hFile << "  return osgData;" << std::endl
         << "}" << std::endl
         << "#endif" << std::endl
         << std::endl;
   hFile.close();

   return 0;
}

void StripCharacters( std::string& data, std::string character )
{
   for ( size_t index = 0; index < data.length(); )
   {
      index = data.find( character, index );
      if ( index != std::string::npos )
      {
         data.insert( index, std::string( "\\" ) );
         index+=2;
      }
   }
}
