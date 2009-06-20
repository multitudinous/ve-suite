/*
Copyright (c) 2004, Mayukh Bose
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

* Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.  

* Redistributions in binary form must reproduce the above copyright
notice, this list of conditions and the following disclaimer in the
documentation and/or other materials provided with the distribution.

* Neither the name of Mayukh Bose nor the names of other
contributors may be used to endorse or promote products derived from
this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

#include <iostream>
#include <fstream>
#include <string>
#include <sstream>
#include "csvparser.h"
#include <map>
#include <vector>

using namespace std;

void StripCharacters( std::string& data, const std::string& character );

int main( int argc, char* argv[] )
{
  string sLine;
  string sCol1, sCol3, sCol4;
  double fCol2;
  int iCol5, iCol6;

  CSVParser parser;

    ifstream infile( argv[ 1 ] );
    //std::streampos beforeNetwork;
    //beforeNetwork = inFile.tellg();
    
    infile.seekg( 0, ios::end);
    std::streampos length = infile.tellg();
    infile.seekg (0, ios::beg);
    
    //Now we have passed the network data so record it
    //std::streampos afterNetwork;
    //afterNetwork = inFile.tellg();
    //go back to the beginning of the network
    //inFile.seekg( beforeNetwork );
    // allocate memory:
    char* buffer = new char [ length ];
    // read data as a block:
    infile.read( buffer, (length) );
    //std::ofstream tester4 ("tester4.txt");
    //tester4<<buffer<<std::endl;
    //tester4.close();
    std::string networkData( buffer );
    delete [] buffer;
    StripCharacters( networkData, "\r" );

    //std::cout << networkData << std::endl;
    //build network information
    //CreateNetworkInformation( networkData );
    istringstream iss;
    iss.str( networkData );

    std::getline(iss, sLine); // Get a line
    parser << sLine; // Feed the line to the parser
    size_t columnCount = 0;
    std::map< int, std::vector< std::string > > csvDataMap;
    
    while( parser.getPos() < sLine.size() )
    {
        parser >> sCol1; // Feed the line to the parser
        //std::cout << sCol1 << std::endl;
        std::vector< std::string > data;
        if( sCol1.empty() )
        {
            ostringstream headerTemp;
            headerTemp << "N/A " << columnCount;
            sCol1 = headerTemp.str();
        }
        data.push_back( sCol1 );
        //std::vector< std::string > data;
        csvDataMap[ columnCount ] = data;
        columnCount += 1;
    }

  while (!iss.eof()) {
    std::getline(iss, sLine); // Get a line
    if (sLine == "")
      continue;

    parser << sLine; // Feed the line to the parser
    cout << sLine << std::endl;
    for( size_t i = 0; i < columnCount; ++i )
    {
        parser >> sCol1;
        //std::cout << sCol1 << " ";
        csvDataMap[ i ].push_back( sCol1 );
    }
    //std::cout << std::endl;
    // Now extract the columns from the line
    /*parser >> sCol1 >> fCol2 >> sCol3;
    parser >> sCol4 >> iCol5;
    parser >> iCol6;

    cout << "Column1: " << sCol1 << endl
         << "Column2: " << fCol2 << endl
         << "Column3: " << sCol3 << endl
         << "Column4: " << sCol4 << endl
         << "Column5: " << iCol5 << endl
         << "Column6: " << iCol6 << endl
         << endl;*/
  }
  //iss.close();
 std::vector< std::string > prtnumbers = csvDataMap[ 2 ];
 for( size_t i = 0; i < prtnumbers.size(); ++i )
 {
     std::cout << prtnumbers.at( i ) << std::endl;
 }
  return 0;
}
///////////////////////////////////////////////////////////////////////////////
void StripCharacters( std::string& data, const std::string& character )
{
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( character, index );
        if ( index != std::string::npos )
        {
            data.replace( index, 1, "\n" );
            //data.erase( index, 1 );
        }
    }
}