/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <string>
#include <iostream>
#include <fstream>
#include <vector>
#include <list>
#include <map>

#include <boost/program_options.hpp>
#include <boost/filesystem/operations.hpp> 
#include <boost/filesystem/path.hpp>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>

#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/DateTimeFormatter.h>
#include <Poco/DateTimeParser.h>

#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>

#include "csvparser.h"

namespace po = boost::program_options;
namespace fs = boost::filesystem;

///Prototypes
std::vector<std::string> GetFilesInDirectory( const std::string dir, const std::string extension );
void ParseDataFile( std::string csvFilename );
void CreateDB();
void RegisterDB();

std::map< std::string, std::vector< std::pair< std::string, std::string > > > m_dataMap;
std::map< int, std::vector< std::string > > m_csvDataMap;
std::string m_dbFilename( "sample.db" );
///End Prototypes
////////////////////////////////////////////////////////////////////////////////
int main( int argc, char* argv[] )
{
    std::string root( "./" );
    std::string extension( ".lis" );
    //Iterate over all of the directories
    std::vector< std::string > lisFiles = GetFilesInDirectory( root, extension );
    //Find if a file has a .lis extension
    for( size_t i = 0; i < lisFiles.size(); ++i )
    {
        std::string dbTableName = lisFiles.at( i );
        boost::algorithm::replace_first( dbTableName, root, "" );
        boost::algorithm::replace_last( dbTableName, extension, "" );
        boost::algorithm::replace_all( dbTableName, "/", "_" );
        std::cout << dbTableName << " " << lisFiles.at( i ) << std::endl;
    }
    //Open the file and parse it
    //Add the data from each .lis file to an .lis specific table in a db
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> GetFilesInDirectory( const std::string dir, const std::string extension )
{
    boost::filesystem::path dir_path( dir.c_str(), boost::filesystem::no_check );
    std::list< std::string > filesInDir;
    try
    {
        if( boost::filesystem::is_directory( dir_path ) )
        {
            boost::filesystem::directory_iterator end_iter;
            for( boost::filesystem::directory_iterator dir_itr( dir_path );
                dir_itr != end_iter; ++dir_itr )
            {
                try
                {
                    if( dir_itr->path().extension() == extension )
                    {
                        std::string pathAndFileName;
                        pathAndFileName.assign( dir_path.string() );
                        pathAndFileName.append( "/" );
                        pathAndFileName.append( dir_itr->leaf() );
                        
                        filesInDir.push_back( pathAndFileName );
                    }
                    else if( fs::is_directory( dir_itr->status() ) )
                    {
                        std::vector<std::string> tempFiles = 
                            GetFilesInDirectory( dir_itr->path().string(), extension );
                        for( size_t j = 0; j < tempFiles.size(); ++j )
                        {
                            filesInDir.push_back( tempFiles.at( j ) );
                        }
                    }
                }
                catch ( const std::exception& ex )
                {
                    std::cout << ex.what() << std::endl;
                }
            }
        }
    }
    catch ( const std::exception& ex )
    {
        std::cout << ex.what() << std::endl;
    }
    filesInDir.sort();
    
    std::vector< std::string > filesList;
    for( std::list< std::string >::const_iterator iter = filesInDir.begin(); iter != filesInDir.end(); ++iter )
    {
        filesList.push_back( *iter );
    }
    
    return filesList;
}
////////////////////////////////////////////////////////////////////////////////
void RegisterDB()
{
    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();
    
    m_dbFilename = "sample.db";
}
////////////////////////////////////////////////////////////////////////////////
void CreateDB()
{
    // create a session
    Poco::Data::Session session("SQLite", m_dbFilename );
    //manage open and closing the session our self so that the population of the
    //db is faster
    session.begin();
    // drop sample table, if it exists
    session << "DROP TABLE IF EXISTS Parts", Poco::Data::now;
    
    // (re)create table
    //session << "CREATE TABLE Parts (Part_Number VARCHAR, Description VARCHAR, Claims INT, Claim_Cost DOUBLE, FPM DOUBLE, CCPM DOUBLE, By VARCHAR)", now;
    
    std::ostringstream createCommand;
    createCommand << "CREATE TABLE Parts (";
    std::vector< std::pair< std::string, std::string > > tempData = m_dataMap.begin()->second;
    for( size_t i = 0; i < tempData.size(); ++i )
    {
        bool isString = false;
        bool isDate = false;
        try
        {
            double test = boost::lexical_cast<double>( tempData.at( i ).second );
            boost::ignore_unused_variable_warning( test );   
        }
        catch( boost::bad_lexical_cast& ex )
        {
            std::cout << "|\tIs string data " 
            << tempData.at( i ).first << std::endl;
            std::cout << "|\tData is " 
            << tempData.at( i ).second << std::endl;
            std::cout << "|\t"<< ex.what() << std::endl;
            isString = true;
        }
        
        if( isString )
        {
            createCommand << "'" << tempData.at( i ).first << "' VARCHAR";
        }
        else
        {
            createCommand << "'" << tempData.at( i ).first << "' DOUBLE";
        }
        
        if( i < tempData.size() - 1 )
        {
            createCommand << ",";
        }
    }
    createCommand << ")";
    
    try
    {
        session << createCommand.str(), Poco::Data::now;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    
    std::ostringstream insertCommand;
    std::map< std::string, std::vector< std::pair< std::string, std::string > > >::iterator iter;
    for( iter = m_dataMap.begin(); iter != m_dataMap.end(); ++iter )
    {
        tempData = iter->second;
        insertCommand << "INSERT INTO Parts VALUES(";
        for( size_t i = 0; i < tempData.size(); ++i )
        {
            bool isString = false;
            bool isDate = false;
            double tempDouble = 0;
            try
            {
                tempDouble = boost::lexical_cast<double>( tempData.at( i ).second );
            }
            catch( boost::bad_lexical_cast& ex )
            {
                //std::cout << "Bad Field " << tempData.at( i ).first << std::endl;
                //std::cout << ex.what() << std::endl;
                isString = true;
            }
            
            if( isString )
            {
                insertCommand << "'"<< tempData.at( i ).second << "'";
            }
            else if( isDate )
            {
                insertCommand << "'"<< tempData.at( i ).second << "'";
            }
            else
            {
                insertCommand << tempDouble;
            }
            
            if( i < tempData.size() - 1 )
            {
                insertCommand << ",";
            }            
        }
        insertCommand << ")";
        
        //insertCommand << "INSERT INTO Parts VALUES('" << tempData.at( 2 ).second << "','" << tempData.at( 3 ).second << "'," << boost::lexical_cast<int>( tempData.at( 4 ).second )
        //    << "," << boost::lexical_cast<double>( tempData.at( 5 ).second ) << "," << boost::lexical_cast<double>( tempData.at( 6 ).second )
        //    << "," <<  boost::lexical_cast<double>( tempData.at( 7 ).second ) << ",'" << tempData.at( 8 ).second << "')";
        {
            Poco::Data::Statement insert( session );
            try
            {
                insert << insertCommand.str(), Poco::Data::now;
            }
            catch( Poco::Data::DataException& ex )
            {
                std::cout << ex.displayText() << std::endl;
            }
            insertCommand.str("");
        }
    }
    
    //Now close the session to match the previous being statement
    session.commit();    
}
////////////////////////////////////////////////////////////////////////////////
void ParseDataFile( std::string csvFilename )
{
    std::string sLine;
    std::string sCol1, sCol3, sCol4;
    
    CSVParser parser;
    
    std::ifstream infile( csvFilename.c_str() );
    //Get file name string from directory passed in
    boost::filesystem::path csvFile( csvFilename.c_str(), 
                                    boost::filesystem::no_check );
    //Set m_dbFilename with the filename of the csv file
    //csvFile.replace_extension( ".db" );
    //m_dbFilename = csvFile.filename();
    
    //std::streampos beforeNetwork;
    //beforeNetwork = inFile.tellg();
    
    infile.seekg( 0, std::ios::end);
    std::streampos length = infile.tellg();
    infile.seekg (0, std::ios::beg);
    
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
    infile.close();
    std::string networkData( buffer );
    delete [] buffer;
    boost::algorithm::replace_all( networkData, "\r", "" );
    
    //std::cout << networkData << std::endl;
    //build network information
    //CreateNetworkInformation( networkData );
    std::istringstream iss;
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
            std::ostringstream headerTemp;
            headerTemp << "N/A " << columnCount;
            sCol1 = headerTemp.str();
        }
        //ReplaceSpacesCharacters( sCol1 );
        data.push_back( sCol1 );
        //std::vector< std::string > data;
        csvDataMap[ columnCount ] = data;

        columnCount += 1;
    }
    
    while( iss.good() )
    {
        std::getline(iss, sLine); // Get a line
        
        if( !iss.good() )
        {
            break;
        }
        
        if (sLine == "")
            continue;
        
        //boost::algorithm::replace_all( sLine, "'", "" );
        //boost::algorithm::replace_all( sLine, "%", "" );
        
        parser << sLine; // Feed the line to the parser
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            //StripDollarCharacters( sCol1 );
            boost::algorithm::trim( sCol1 );

            csvDataMap[ i ].push_back( sCol1 );
        }
    }
    //iss.close();
    m_csvDataMap = csvDataMap;
    /*mLoadedPartNumbers = csvDataMap[ m_partNumberColumn ];
    for( size_t i = 1; i < mLoadedPartNumbers.size(); ++i )
    {
        std::vector< std::pair< std::string, std::string > > partData;
        for( size_t j = 0; j < columnCount; ++j )
        {
            partData.push_back( std::pair< std::string, std::string >( csvDataMap[ j ].at( 0 ), csvDataMap[ j ].at( i ) ) );
        }
        
        if( !mLoadedPartNumbers.at( i ).empty() )
        {
            m_dataMap[ mLoadedPartNumbers.at( i ) ] = partData;
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
