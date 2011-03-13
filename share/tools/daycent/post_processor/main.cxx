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
#include <boost/tokenizer.hpp>

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

namespace po = boost::program_options;
namespace fs = boost::filesystem;

///Prototypes
std::vector<std::string> GetFilesInDirectory( const std::string dir, const std::string extension );
void ParseDataFile( std::string csvFilename, std::string dbTableName );
void RegisterDB();

std::string m_dbFilename( "sample.db" );
///End Prototypes
////////////////////////////////////////////////////////////////////////////////
int main( int argc, char* argv[] )
{
    std::string root( "./" );
    std::string extension( ".lis" );
    //Iterate over all of the directories
    //Find if a file has a .lis extension
    std::vector< std::string > lisFiles = GetFilesInDirectory( root, extension );
    //Open the file and parse it
    //Add the data from each .lis file to an .lis specific table in a db
    RegisterDB();
    //Iterate over the files
    for( size_t i = 0; i < lisFiles.size(); ++i )
    {
        std::string dbTableName = lisFiles.at( i );
        boost::algorithm::replace_first( dbTableName, root, "" );
        boost::algorithm::replace_last( dbTableName, extension, "" );
        boost::algorithm::replace_all( dbTableName, "/", "_" );
        boost::algorithm::replace_all( dbTableName, " ", "_" );
        std::cout << dbTableName << " " << lisFiles.at( i ) << std::endl;
        
        ParseDataFile( lisFiles.at( i ), dbTableName );
    }

    return 0;
}
////////////////////////////////////////////////////////////////////////////////
std::vector<std::string> GetFilesInDirectory( const std::string dir, const std::string extension )
{
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    boost::filesystem::path dir_path( dir.c_str() );
#else
    boost::filesystem::path dir_path( dir.c_str(), boost::filesystem::no_check );
#endif
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
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
                        pathAndFileName.append( dir_itr->path().string() );
#else
                        pathAndFileName.append( dir_itr->leaf() );
#endif                   
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
                catch( const std::exception& ex )
                {
                    std::cout << ex.what() << std::endl;
                }
            }
        }
    }
    catch( const std::exception& ex )
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
    //
    m_dbFilename = "sample.db";
}
////////////////////////////////////////////////////////////////////////////////
void ParseDataFile( std::string csvFilename, std::string dbTableName )
{
    std::ifstream infile( csvFilename.c_str() );
    
    infile.seekg( 0, std::ios::end);
    std::streampos length = infile.tellg();
    infile.seekg (0, std::ios::beg);

    char* buffer = new char [ length ];
    infile.read( buffer, (length) );
    infile.close();

    std::string networkData( buffer );
    delete [] buffer;
    boost::algorithm::replace_all( networkData, "\r", "" );
    
    typedef boost::tokenizer< boost::escaped_list_separator<char> > Tok;
    boost::escaped_list_separator<char> sep( "", " \n", "");
    Tok tok( networkData, sep );
    std::string tempTok;
    double tempDouble = 0;
    size_t columnCount1 = 0;
    std::vector< std::string > columnNames;
    Tok::iterator firstDouble;
    for(Tok::iterator tok_iter = tok.begin(); tok_iter != tok.end(); ++tok_iter)
    {
        tempTok = *tok_iter;
        if( tempTok.empty() )
        {
            continue;
        }
        //std::cout << "<" << tempTok << "> ";
            
        try
        {
            tempDouble = boost::lexical_cast<double>( tempTok );
            firstDouble = tok_iter;
            break;
            //std::cout << tempDouble << " "; 
        }
        catch( boost::bad_lexical_cast& ex )
        {
            //std::cout << tempTok << " ";
            columnNames.push_back( tempTok );
            columnCount1 +=1;
        }
    }
    std::cout << "Column Count " << columnCount1 << std::endl;
    
    //////////////////////////////
    //////////////////////////////

    Poco::Data::Session session("SQLite", m_dbFilename );
    //manage open and closing the session our self so that the population of the
    //db is faster
    session.begin();
    // drop sample table, if it exists
    std::ostringstream dropTable;
    dropTable << "DROP TABLE IF EXISTS " << dbTableName;
    session << dropTable.str().c_str(), Poco::Data::now;
    
    std::ostringstream createCommand;
    createCommand << "CREATE TABLE " << dbTableName << " (";
    for( size_t i = 0; i < columnNames.size(); ++i )
    {
        createCommand << "'" << columnNames.at( i ) << "' DOUBLE";
        
        if( i < columnCount1 - 1 )
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
    
    //////////////////////////////
    //////////////////////////////
    
    size_t columnCounter = 0;
    std::ostringstream insertCommand;
    for(Tok::iterator tok_iter = firstDouble; tok_iter != tok.end(); ++tok_iter)
    {
        tempTok = *tok_iter;
        if( tempTok.empty() )
        {
            continue;
        }
        //std::cout << "<" << tempTok << "> ";

        try
        {
            tempDouble = boost::lexical_cast<double>( tempTok );
            if( columnCounter == 0 )
            {
                insertCommand << "INSERT INTO " << dbTableName << " VALUES(";
            }
        }
        catch( boost::bad_lexical_cast& ex )
        {
            ;
        }
        
        insertCommand << tempDouble;
    
        if( columnCounter < columnCount1 - 1 )
        {
            insertCommand << ",";
            ++columnCounter;
        }
        else
        {
            insertCommand << ")";
            //std::cout << insertCommand.str() << std::endl;
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
            columnCounter = 0;
        }
    }

    //Now close the session to match the previous being statement
    session.commit();    
}
////////////////////////////////////////////////////////////////////////////////
