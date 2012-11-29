/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

#include <ves/xplorer/communication/CommunicationHandler.h>

// --- My Includes --- //
#include "WarrantyToolGP.h"
#include "csvparser.h"
#include "ves/xplorer/ModelCADHandler.h"
#include "ves/xplorer/Model.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/OneDStringArray.h>

#include <ves/xplorer/scenegraph/util/TransparencySupport.h>
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>
#include <ves/xplorer/scenegraph/util/ToggleNodesVisitor.h>

#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/HighlightNodesByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>
#include <ves/xplorer/scenegraph/HeadPositionCallback.h>
#include <ves/xplorer/scenegraph/HeadsUpDisplay.h>

#include <ves/xplorer/environment/TextTextureCallback.h>

#include <ves/xplorer/EnvironmentHandler.h>

#include <osgUtil/LineSegmentIntersector>
#include <osg/Depth>

#include <sstream>
#include <iostream>
#include <fstream>
#include <algorithm>

using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

#include <Poco/SharedPtr.h>
#include <Poco/Tuple.h>
#include <Poco/DateTimeFormatter.h>
#include <Poco/DateTimeParser.h>

#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>
#include <vector>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>
#include <boost/algorithm/string/case_conv.hpp>
#include <boost/filesystem.hpp>
#include <boost/algorithm/string/find.hpp>
#include <boost/algorithm/string/predicate.hpp>

#include <ves/xplorer/device/KeyboardMouse.h>

using namespace Poco::Data;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::WarrantyToolGP()
    :
    PluginBase(),
    mAddingParts( false ),
    m_keyboard( 0 ),
    m_groupedTextTextures( 0 ),
    m_cadRootNode( 0 ),
    m_hasPromiseDate( false ),
    m_mouseSelection( false ),
    m_currentStatement( 0 )
{
    std::cout << "WarrantyToolGP ctor " << this << std::endl << std::flush;
    //Needs to match inherited UIPluginBase class name
    mObjectName = "WarrantyToolUI";
    m_dbFilename = "sample.db";
    
    mEventHandlerMap[ "WARRANTY_TOOL_PART_TOOLS" ] = this;
    mEventHandlerMap[ "WARRANTY_TOOL_DB_TOOLS" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::~WarrantyToolGP()
{
    //std::cout << "~WarrantyToolGP " << this << std::endl << std::flush;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );

    //Connect signals here so that only specific plugins are connected to the
    //the ui plugin. This is sort of safe...
    CONNECTSIGNALS_1( "%ToggleUnselected",
                    void( const bool& ),
                    &WarrantyToolGP::ToggleUnselected,
                    m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_1( "%MouseSelection",
                    void( const bool& ),
                    &WarrantyToolGP::SetMouseSelection,
                    m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_1( "%HighlightPart",
                     void( const std::string& ),
                     &WarrantyToolGP::HighlightPart,
                     m_connections, any_SignalType, normal_Priority );

    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::PreFrameUpdate()
{
    if( !m_keyboard )
    {
        return;
    }
        
    if( m_groupedTextTextures.valid() )
    {
        if( !m_groupedTextTextures->AnimationComplete() )
        {
            m_groupedTextTextures->UpdateTexturePosition();
            return;
        }
    }

    //If the mouse made a pick event
    /*if( !m_keyboard->GetMousePickEvent() )
    {
        return;
    }*/


    //If we had keyboard input then try and highlight the cad
    bool pickedParts = false;
    if( m_mouseSelection )
    {
        pickedParts = FindPartNodeAndHighlightNode();
    }

    //If we did not pick any parts and we have already queried for data
    if( m_groupedTextTextures.valid() && !pickedParts )
    {
        PickTextTextures();
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
    std::cout << "WarrantyToolGP::SetCurrentCommand" << std::endl << std::flush;
    if( !command )
    {
        return;
    }
    m_currentCommand = command;
    
    const std::string commandName = m_currentCommand->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = 
        m_currentCommand->GetDataValuePair( 0 );

    //Before anything else remove the glow if there is glow
    if( commandName == "WARRANTY_TOOL_PART_TOOLS" )
    {
        m_cadRootNode = mModel->GetModelCADHandler()->
            GetAssembly( mModel->GetModelCADHandler()->GetRootCADNodeID() );
        if( !m_cadRootNode )
        {
            std::cout << "m_cadRootNode is invalid" << std::endl << std::flush;
            return;
        }
        
        const std::string dvpName = dvp->GetDataName();
        if( dvpName == "RESET" )
        {
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( m_cadRootNode, "", false, true );
            //Make everything opaque
            transparentDisable( m_cadRootNode );

            mAddingParts = false;
        }
        else if( dvpName == "ADD" )
        {
            //Highlight the respective node
            //Make a user specified part glow
            //if( !mAddingParts )
            {
                transparentEnable( m_cadRootNode, 0.3f );

                //mAddingParts = true;
                m_assemblyPartNumbers.clear();
                m_joinedPartNumbers.clear();
            }
            //Highlight part
            m_lastPartNumber = dvp->GetDataString();
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
                highlight( m_cadRootNode, m_lastPartNumber, true, true );
            RenderTextualDisplay( true );
        }
        else if( dvpName == "SCROLL" )
        {
            m_assemblyPartNumbers.clear();
            m_joinedPartNumbers.clear();
            //Highlight the respective node
            //Make a user specified part glow
            transparentEnable( m_cadRootNode, 0.3f );

            mAddingParts = false;
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
                highlight2( m_cadRootNode, "", false, true );
            //Highlight part
            m_lastPartNumber = dvp->GetDataString();
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
                highlight( m_cadRootNode, m_lastPartNumber, true, true );
            RenderTextualDisplay( true );
        }
        else if( dvpName == "CLEAR" )
        {
            transparentEnable( m_cadRootNode, 0.3f );
            
            bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
            boost::ignore_unused_variable_warning( removed );

            RenderTextualDisplay( false );

            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
                highlight2( m_cadRootNode, "", false, true );
            
            //Clear the db of tables
            ClearDatabaseUserTables();
            
            //Clear the list of active part numbers
            m_assemblyPartNumbers.clear();
            m_joinedPartNumbers.clear();
        }
        else if( dvpName == "TOGGLE_PARTS" )
        {
            unsigned int checkBox;
            dvp->GetData( checkBox );
            ToggleUnselected( checkBox );
        }
        else if( dvpName == "MOUSE_SELECTION" )
        {
            unsigned int checkBox;
            dvp->GetData( checkBox );
            SetMouseSelection( checkBox );
        }
        else if( dvpName == "WARRANTY_FILE" )
        {
            std::string filename = dvp->GetDataString();
            std::cout << "WarrantyToolGP::SetCurrentCommand::WARRANTY_FILE: " <<  filename << std::endl << std::flush;
            boost::filesystem::path dataPath( filename );
            if( dataPath.extension() == ".db" )
            {
                ParseDataBase( dataPath.string() );
            }
            else if( dataPath.extension() == ".csv" )
            {
                ParseDataFile( dataPath.string() );
            }
            /*for( size_t i = 1; i < mLoadedPartNumbers.size(); ++i )
            {
                ves::xplorer::scenegraph::util::FindChildWithNameVisitor 
                    childVisitor( mDCS.get(), mLoadedPartNumbers.at( i ), false, true );
                if( childVisitor.FoundChild() )
                {
                    ;//std::cout << "Found graphics node match for " << mLoadedPartNumbers.at( i ) << std::endl;
                }
                else
                {
                    std::cout << "Did not find graphics node for " << mLoadedPartNumbers.at( i ) << std::endl;
                }
            }*/
            //m_keyboard->SetProcessSelection( false );
            CreateTextTextures();
        }
        else if( dvpName == "SET_ACTIVE_TABLES" )
        {
            std::vector<std::string> stringArray = 
                boost::static_pointer_cast<ves::open::xml::OneDStringArray>( 
                dvp->GetDataXMLObject() )->GetArray();
            
            m_tableNames = 
                std::make_pair< std::string, std::string >( 
                stringArray.at( 0 ), stringArray.at( 1 ) );
        }
        else if( dvpName == "SAVE" )
        {
            std::string saveFile;
            dvp->GetData( saveFile );
            SaveCurrentQuery( saveFile );
        }
    }
    else if( commandName == "WARRANTY_TOOL_DB_TOOLS" )
    {
        m_cadRootNode = mModel->GetModelCADHandler()->
            GetAssembly( mModel->GetModelCADHandler()->GetRootCADNodeID() );
        if( !m_cadRootNode )
        {
            std::cout << "WarrantyToolGP::SetCurrentCommand::WARRANTY_TOOL_DB_TOOLS: m_cadRootNode invalid" << std::endl << std::flush;
            return;
        }
        
        dvp = command->GetDataValuePair( "QUERY_STRING" );
        CreateDBQuery( dvp );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::StripCharacters( std::string& data, const std::string& character )
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
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::ParseDataFile( const std::string& csvFilename )
{
    std::cout << "WarrantyToolGP::ParseDataFile " << csvFilename << std::endl << std::flush;
    std::string sLine;
    std::string sCol1, sCol3, sCol4;
    //double fCol2;
    //int iCol5, iCol6;
    
    CSVParser parser;
    
    std::ifstream infile( csvFilename.c_str() );
    //Get file name string from directory passed in
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    boost::filesystem::path csvFile( csvFilename.c_str() );
#else
    boost::filesystem::path csvFile( csvFilename.c_str(), 
        boost::filesystem::no_check );
#endif
    //Set m_dbFilename with the filename of the csv file
    csvFile.replace_extension( ".db" );
#if (BOOST_VERSION >= 104600) && (BOOST_FILESYSTEM_VERSION == 3)
    m_dbFilename = csvFile.filename().string();
#else
    m_dbFilename = csvFile.filename();
#endif
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
    StripCharacters( networkData, "\r" );
    
    //build network information
    //CreateNetworkInformation( networkData );
    std::istringstream iss;
    iss.str( networkData );
    
    std::getline(iss, sLine); // Get a line
    parser << sLine; // Feed the line to the parser
    size_t columnCount = 0;
    std::map< int, std::vector< std::string > > csvDataMap;
    
    m_partNumberColumn = 0;
    m_promiseDateColumn = 0;
    m_hasPromiseDate = false;
    while( parser.getPos() < sLine.size() )
    {
        parser >> sCol1; // Feed the line to the parser
        std::vector< std::string > data;
        if( sCol1.empty() )
        {
            std::ostringstream headerTemp;
            headerTemp << "N/A " << columnCount;
            sCol1 = headerTemp.str();
        }
        ReplaceSpacesCharacters( sCol1 );
        data.push_back( sCol1 );
        csvDataMap[ columnCount ] = data;
        if( "Part_Number" == sCol1 )
        {
            m_partNumberColumn = columnCount;
        }
        
        //if( boost::algorithm::ifind_first( sCol1, "promise_date" ) )
        if( boost::algorithm::iequals( sCol1, "promise_date" ) )
        {
            m_hasPromiseDate = true;
            m_promiseDateColumn = columnCount;
        }
        columnCount += 1;
    }

    std::cout << "Part Number Column = " << m_partNumberColumn 
        << " Promise Date Column = " << m_promiseDateColumn << std::endl;

    while( iss.good() )
    {
        std::getline(iss, sLine); // Get a line

        if( !iss.good() )
        {
            break;
        }
        
        if (sLine == "")
            continue;
        
        boost::algorithm::replace_all( sLine, "'", "" );
        boost::algorithm::replace_all( sLine, "%", "" );

        parser << sLine; // Feed the line to the parser
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            StripDollarCharacters( sCol1 );
            boost::algorithm::trim( sCol1 );
            if( m_hasPromiseDate && (i == m_promiseDateColumn) )
            {
                //convert date to uniform string
                try
                {
                    int timeDifValue = 0;
                    std::string fmt = "%e-%b-%y";
                    Poco::DateTime tempDateTime = Poco::DateTimeParser::parse( fmt, sCol1, timeDifValue );
                    std::string newDateTime = Poco::DateTimeFormatter::format( tempDateTime, "%Y.%m.%d" );
                    sCol1 = newDateTime;
                }
                catch( Poco::SyntaxException& ex )
                {
                    std::cout << ex.displayText() << std::endl
                        << "The actual string is " << sCol1 << std::endl;
                }
            }
            csvDataMap[ i ].push_back( sCol1 );
        }
    }
    //iss.close();
    m_csvDataMap = csvDataMap;
    mLoadedPartNumbers = csvDataMap[ m_partNumberColumn ];
    //Erase the previous map before we create the new one
    m_dataMap.clear();
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
    }
    
    
    if( !mAddingParts )
    {
        transparentEnable( mDCS.get(), 0.3f );

        mAddingParts = true;
    }
    
    //Open DB
    //Add data
    //close connection
    CreateDB();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::RenderTextualDisplay( bool onOff )
{
    if( onOff )
    {
        //add 3d blocks
        if( !mModelText.valid() )
        {
            mModelText = new ves::xplorer::scenegraph::TextTexture();
            float textColor[ 4 ] = { 0.0, 0.0, 0.0, 1.0 };
            mModelText->SetTextColor( textColor );
            m_textTrans->addChild( mModelText.get() );
        }
        else
        {
            mModelText->setNodeMask( 1 );
        }
        bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
        boost::ignore_unused_variable_warning( removed );

        ves::open::xml::DataValuePairPtr stringDvp = 
            m_currentCommand->GetDataValuePair( "DISPLAY_TEXT_FIELDS" );
        
        std::vector<std::string> stringArray = 
            boost::static_pointer_cast<ves::open::xml::OneDStringArray>( 
            stringDvp->GetDataXMLObject() )->GetArray();
        
        //std::string displayString;
        //std::pair< std::string, std::string > displayPair;
        //std::vector< std::pair< std::string, std::string > > displayVector;
        //std::map< std::string, std::vector< std::pair< std::string, std::string > > >::iterator iter;
        //iter = m_dataMap.find( m_lastPartNumber );
        //if( iter == m_dataMap.end() )
        //{
        //    return;
        //}
        //displayVector = iter->second;
        //for( size_t i = 0; i < displayVector.size(); ++i )
        //{
        //    displayPair = displayVector.at( i );
        //    displayString = displayString + displayPair.first + " " +  displayPair.second + "\n";
        //}
        ////////////////////////////////////////////////////////////////////////////////
        mCommunicationHandler->SendConductorMessage( "Creating DB query..." );

        Poco::Data::Session session("SQLite", m_dbFilename );
        Statement select( session );
        try
        {
            std::string queryString = "SELECT ";
            for( size_t i = 0; i < stringArray.size(); ++i )
            {
                queryString += "\"" + stringArray.at( i ) + "\"" + ", ";
            }
            queryString += "\"Part_Number\" ";
            queryString += "FROM Parts WHERE ";
            
            // a simple query
            //std::string queryString = "SELECT * FROM Parts WHERE \"Part_Number\" = '" + m_lastPartNumber +"'";
            queryString +=  "\"Part_Number\" = '" + m_lastPartNumber +"'";
            select << queryString.c_str(),now;
            //select.execute();
        }
        catch( Poco::Data::DataException& ex )
        {
            std::cout << ex.displayText() << std::endl;
            return;
        }
        catch( ... )
        {
            std::cout << "Query is bad." << std::endl;
            return;
        }
        
        // create a RecordSet 
        Poco::Data::RecordSet rs(select);
        std::size_t cols = rs.columnCount();
        size_t numQueries = rs.rowCount();
        if( numQueries == 0 )
        {
            mCommunicationHandler->SendConductorMessage( "No parts found." );
            return;
        }
    
        // iterate over all rows and columns
        bool more = false;
        try
        {
            more = rs.moveFirst();
        }
        catch( ... )
        {
            return;
        }

        std::ostringstream tempTextData;
        std::string partNumber;
        std::string partNumberHeader;
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName(col);
            boost::algorithm::to_lower( partNumberHeader );
            if( partNumberHeader == "part_number" )
            {
                partNumber = rs[col].convert<std::string>();
                m_assemblyPartNumbers.push_back( partNumber );
            }
            else
            {
                tempTextData << rs.columnName(col) << ": " 
                    << rs[col].convert<std::string>() << "\n";
            }
        }
        const std::string partText = tempTextData.str();
        mModelText->UpdateText( partText );
        
        mCommunicationHandler->SendConductorMessage( "Finished DB query..." );
            
        mModelText->SetTitle( m_lastPartNumber );
    }
    else
    {
        if( mModelText.valid() )
        {
            mModelText->setNodeMask( 0 );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateDB()
{
    std::cout << "WarrantyToolGP::CreateDB" << std::endl << std::flush;
    mCommunicationHandler->SendConductorMessage( "Creating DB..." );

    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();

    // create a session
    Poco::Data::Session session("SQLite", m_dbFilename );
    //manage open and closing the session our self so that the population of the
    //db is faster
    session.begin();
    // drop sample table, if it exists
    session << "DROP TABLE IF EXISTS Parts", now;
    //SELECT * FROM sqlite_master WHERE tbl_name LIKE 'User_Table_%'
    
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

            if( m_hasPromiseDate && (i == m_promiseDateColumn) )
            {
                isString = false;
                isDate = true;
            }
        }
        
        if( isString || (i == m_partNumberColumn) )
        {
            createCommand << "'" << tempData.at( i ).first << "' VARCHAR";
        }
        else if( isDate )
        {
            //We cannot use the date data type because DATE is not supported
            //in sqlite
            //createCommand << "'" << tempData.at( i ).first << "' DATE";
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
        session << createCommand.str(), now;
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
                if( i == m_partNumberColumn )
                {
                    isString = true;
                }
            }
            catch( boost::bad_lexical_cast& ex )
            {
                //std::cout << "Bad Field " << tempData.at( i ).first << std::endl;
                //std::cout << ex.what() << std::endl;
                isString = true;
                
                if( m_hasPromiseDate && (i == m_promiseDateColumn) )
                {
                    isString = false;
                    isDate = true;
                }
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
            Statement insert( session );
            try
            {
                insert << insertCommand.str(), now;
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

    //insert << "INSERT INTO Parts VALUES(?, ?, ?, ?, ?, ?, ?)",
    //    use(assem), now;
    mCommunicationHandler->SendConductorMessage( "Finished creating DB..." );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateTextTextures()
{
    m_textTrans = new ves::xplorer::scenegraph::DCS();
    m_textTrans->getOrCreateStateSet()->addUniform(
        new osg::Uniform( "glowColor", osg::Vec3( 0.0, 0.0, 0.0 ) ) );
    
    osg::ref_ptr< osg::Group > viewCameraGroup;
    /*if( mSceneManager->IsDesktopMode() )
    {
        viewCameraGroup = mEnvironmentHandler->GetHeadsUpDisplay()->GetCamera();
    }
    else*/
    {
        viewCameraGroup = mDCS.get();
        //mModelText->setUpdateCallback( 
        //    new ves::xplorer::environment::TextTextureCallback( mModelText.get() ) );
        m_textTrans->setUpdateCallback( 
            new ves::xplorer::scenegraph::HeadPositionCallback() );
        //static_cast< osg::PositionAttitudeTransform* >( 
        //    mModelText->getParent( 0 ) )->setPosition( osg::Vec3d( 0, 0, 0 ) );        
    }
    
    viewCameraGroup->addChild( m_textTrans.get() );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateDBQuery( ves::open::xml::DataValuePairPtr dvp )
{
    std::cout << "WarrantyToolGP::CreateDBQuery" << std::endl << std::flush;
    mCommunicationHandler->SendConductorMessage( "Creating DB query..." );

    //Clear and reset the data containers for the user defined queries
    m_assemblyPartNumbers.clear();
    m_joinedPartNumbers.clear();
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( m_cadRootNode, "", false, true );
        
        transparentEnable( m_cadRootNode, 0.3f );
    }

    RenderTextualDisplay( false );
    bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
    boost::ignore_unused_variable_warning( removed );
    m_groupedTextTextures = 0;

    //Now lets query things
    const std::string queryString = dvp->GetDataString();

    if( !boost::find_first( queryString, "INNER JOIN" ) )
    {
        QueryUserDefinedAndHighlightParts( queryString );
    }
    else
    {
        HighlightPartsInJoinedTabled( queryString );
    }
        
    mCommunicationHandler->SendConductorMessage( "Finished DB query..." );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();
    //m_keyboard->SetProcessSelection( true );
    try
    {
        Poco::Data::SQLite::Connector::unregisterConnector();
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
    }
    catch( Poco::AssertionViolationException& ex )
    {
        std::cout << ex.displayText() << std::endl;
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::StripDollarCharacters( std::string& data )
{
    char firstChar = data[ 0 ];
    char dollarChar( '$' );
    if( firstChar != dollarChar )
    {
        return;
    }

    boost::algorithm::replace_all( data, "$", "" );
    /*for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( '$', index );
        if ( index != std::string::npos )
        {
            //data.replace( index, 1, "" );
            data.erase( index, 1 );
        }
    }*/

    boost::algorithm::replace_all( data, ",", "" );
    /*for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( ',', index );
        if ( index != std::string::npos )
        {
            //data.replace( index, 1, "" );
            data.erase( index, 1 );
        }
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::ReplaceSpacesCharacters( std::string& data )
{
    boost::algorithm::trim( data );
    boost::algorithm::replace_all( data, " ", "_" );

/*
    for ( size_t index = 0; index < data.length(); )
    {
        index = data.find( ' ', index );
        if ( index != std::string::npos )
        {
            data.replace( index, 1, "_" );
            //data.erase( index, 1 );
        }
    }
*/
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::ParseDataBase( const std::string& csvFilename )
{
    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();

    m_dbFilename = csvFilename;
    
    if( !mAddingParts )
    {
        transparentEnable( mDCS.get(), 0.3f );
        mAddingParts = true;
    }    
}
////////////////////////////////////////////////////////////////////////////////
bool WarrantyToolGP::FindPartNodeAndHighlightNode()
{
    if( !m_cadRootNode )
    {
        return false;
    }

    osg::ref_ptr< osgUtil::LineSegmentIntersector > intersectorSegment;// = 
    //    m_keyboard->GetLineSegmentIntersector();

    osgUtil::IntersectionVisitor intersectionVisitor( intersectorSegment.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    m_cadRootNode->accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        intersectorSegment->getIntersections();
    if( intersections.empty() )
    {
        return false;
    }
    
    ///Do we already have active parts
    bool activeQuery = true;
    if( m_assemblyPartNumbers.size() == 0 )
    {
        activeQuery = false;
    }
    
    //Find the part numbers of the nodes we hit
    osg::Node* objectHit = 0;
    osg::Node* tempParent = 0;
    std::string pickedPartNumbers;
    for( osgUtil::LineSegmentIntersector::Intersections::iterator itr =
        intersections.begin(); itr != intersections.end(); ++itr )
    {
        objectHit = *( itr->nodePath.rbegin() );
        //std::cout << "Top Node " << objectHit->getName() << std::endl;
        //First we see if the name has prt in the part name
        const std::string prtname = ".PRT";
        ves::xplorer::scenegraph::FindParentWithNameVisitor findPRT( objectHit, prtname, false );
        tempParent = findPRT.GetParentNode();
        std::string nodeName;
        
        if( !tempParent )
        {
            //Then we see if it has asm in the part name
            std::string asmname(".ASM");
            ves::xplorer::scenegraph::FindParentWithNameVisitor findASM( 
                objectHit, asmname, false );
            tempParent = findASM.GetParentNode();
            if( tempParent )
            {
                nodeName = tempParent->getName();
                GetPartNumberFromNodeName( nodeName );
            }
        }
        else
        {
            nodeName = tempParent->getName();
            GetPartNumberFromNodeName( nodeName );
        }
        
        //Then we just get the base part and start traversing up the node path
        //to find a node with a name and go with that
        if( !tempParent )
        {
            size_t increment = 1;
            while( nodeName.empty() )
            {
                tempParent = *( itr->nodePath.rbegin() + increment );
                nodeName = tempParent->getName();
                ++increment;
            }
            
            GetPartNumberFromNodeName( nodeName );
        }

        if( !nodeName.empty() )
        {
            std::vector< std::string >::const_iterator iter = 
            std::find( m_assemblyPartNumbers.begin(), 
                      m_assemblyPartNumbers.end(), nodeName );
            if( activeQuery )
            {
                if( iter != m_assemblyPartNumbers.end() )
                {
                    pickedPartNumbers = *iter;
                    break;
                }
                else
                {
                    std::string tempNodeName;
                    std::string tempActiveName;
                    for( size_t i = 0; i < itr->nodePath.size(); ++i )
                    {
                        tempNodeName = itr->nodePath.at( i )->getName();
                        boost::algorithm::to_lower( tempNodeName );
                        for( size_t j = 0; j < m_assemblyPartNumbers.size(); ++j )
                        {
                            tempActiveName = m_assemblyPartNumbers.at( j );
                            boost::algorithm::to_lower( tempActiveName );

                            size_t found = tempNodeName.find( tempActiveName );
                            if( found != std::string::npos )
                            {
                                pickedPartNumbers = m_assemblyPartNumbers.at( j );
                                break;
                            }
                        }
                        if( !pickedPartNumbers.empty() )
                        {
                            break;
                        }
                    }
                }
            }
            else
            {
                if( iter == m_assemblyPartNumbers.end() )
                {
                    m_assemblyPartNumbers.push_back( nodeName );
                }
            }
        }

        /*for( size_t i = 0; i < itr->nodePath.size(); ++i )
        {
            std::cout << itr->nodePath.at( i )->getName() << std::endl;
        }*/
    }

    bool failedLoad = false;
    if( !activeQuery )
    {
        ///Now we will setup the textual displays for the list of part numbers found
        RenderTextualDisplay( false );
        bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
        boost::ignore_unused_variable_warning( removed );
        
        m_groupedTextTextures = new ves::xplorer::scenegraph::GroupedTextTextures();
        
        std::ostringstream outString;
        outString << "Number of parts found " << m_assemblyPartNumbers.size();
        mCommunicationHandler->SendConductorMessage( outString.str() );
        
        float textColor[ 4 ] = { 0.0, 0.0, 0.0, 1.0 };
        std::string partNumber;
        std::string partNumberHeader;
        std::set< std::string > setOfPartNumbers;
        for( size_t i = 0; i < m_assemblyPartNumbers.size(); ++i )
        {
            ves::xplorer::scenegraph::TextTexture* tempText = 0;
            try
            {
                tempText = new ves::xplorer::scenegraph::TextTexture();
            }
            catch(...)
            {
                m_groupedTextTextures = 0;
                failedLoad = true;
                break;
            }
            partNumber = m_assemblyPartNumbers.at( i );
            tempText->SetTextColor( textColor );
            tempText->SetTitle( partNumber );
            
            //Now lets create the db query
            //SELECT * FROM Parts WHERE Part_Number = "AH116104"
            Poco::Data::Session session("SQLite", m_dbFilename );
            Statement select( session );
            std::ostringstream queryString;
            try
            {
                queryString << "SELECT * FROM Parts WHERE Part_Number = \"" << partNumber <<"\"";
                select << queryString.str().c_str(),now;
            }
            catch( Poco::Data::DataException& ex )
            {
                std::cout << ex.displayText() << std::endl;
                continue;
            }
            catch( ... )
            {
                mCommunicationHandler->SendConductorMessage( "Query is bad." );
                continue;
            }
            
            // create a RecordSet 
            Poco::Data::RecordSet rs(select);
            size_t numQueries = rs.rowCount();
            if( numQueries > 0 )
            {
                std::size_t cols = rs.columnCount();
                //iterate over all rows and columns
                bool more = false;
                more = rs.moveFirst();
                
                std::ostringstream tempTextData;
                for (std::size_t col = 0; col < cols; ++col)
                {
                    partNumberHeader = rs.columnName(col);
                    boost::algorithm::to_lower( partNumberHeader );
                    if( partNumberHeader != "part_number" )
                    {
                        tempTextData << rs.columnName(col) << ": " 
                            << rs[col].convert<std::string>() << "\n";
                    }
                }
                
                const std::string partText = tempTextData.str();
                tempText->UpdateText( partText );
            }
            
            m_groupedTextTextures->AddTextTexture( partNumber, tempText );
            
           setOfPartNumbers.insert( partNumber );
            //ves::xplorer::scenegraph::HighlightNodeByNameVisitor highlight( 
            //    m_cadRootNode, partNumber, true, true, 
            //    osg::Vec3( 0.57255, 0.34118, 1.0 ) );
        }
        ves::xplorer::scenegraph::HighlightNodesByNameVisitor highlightNodes( 
            m_cadRootNode, setOfPartNumbers, true, true, 
            osg::Vec3( 0.57255, 0.34118, 1.0 ) );
        
        if( !failedLoad && (m_assemblyPartNumbers.size() > 0) )
        {
            m_groupedTextTextures->UpdateListPositions();
            
            m_textTrans->addChild( m_groupedTextTextures.get() );
            
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor highlight( 
                m_cadRootNode, m_assemblyPartNumbers.at( 0 ), true, true );
        }
    }
    else
    {
        if( !pickedPartNumbers.empty() )
        {
            std::set< std::string > setOfPartNumbers;
            for( size_t i = 0; i < m_assemblyPartNumbers.size(); ++i )
            {
                setOfPartNumbers.insert( m_assemblyPartNumbers.at( i ) );

                //ves::xplorer::scenegraph::HighlightNodeByNameVisitor highlight( 
                //    m_cadRootNode, m_assemblyPartNumbers.at( i ), true, true, 
                //    osg::Vec3( 0.57255, 0.34118, 1.0 ) );
            }
            ves::xplorer::scenegraph::HighlightNodesByNameVisitor highlightNodes( 
               m_cadRootNode, setOfPartNumbers, true, true, 
               osg::Vec3( 0.57255, 0.34118, 1.0 ) );

            m_groupedTextTextures->SetTextureUpdateAnimationOn( false );
            m_groupedTextTextures->MakeTextureActive( pickedPartNumbers );
            m_groupedTextTextures->SetTextureUpdateAnimationOn( true );
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor highlight( 
                m_cadRootNode, pickedPartNumbers, true, true );
        }
    }
    mCommunicationHandler->SendConductorMessage( "Finished DB query..." );
    
    return (m_assemblyPartNumbers.size() && !failedLoad);
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::GetPartNumberFromNodeName( std::string& nodeName )
{
    {
        size_t index = nodeName.find( '_' );
        if ( index != std::string::npos )
        {
            nodeName.erase( index, nodeName.length() - index  );
        }
    }
    
    {
        size_t index = nodeName.find( '.' );
        if ( index != std::string::npos )
        {
            nodeName.erase( index, nodeName.length() - index  );
        }
    }

    {
        size_t index = nodeName.find( ' ' );
        if ( index != std::string::npos )
        {
            nodeName.erase( index, nodeName.length() - index  );
        }
    }
    
    boost::algorithm::to_upper( nodeName );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::PickTextTextures()
{
    //Get the intersection visitor from keyboard mouse or the wand
    osg::ref_ptr< osgUtil::LineSegmentIntersector > intersectorSegment;// = 
    //    m_keyboard->GetLineSegmentIntersector();
    
    osgUtil::IntersectionVisitor intersectionVisitor(
                                                     intersectorSegment.get() );
    
    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    m_textTrans->accept( intersectionVisitor );
    
    osgUtil::LineSegmentIntersector::Intersections& intersections =
    intersectorSegment->getIntersections();
    //figure out which text texutre we found
    osg::Node* objectHit = 0;
    bool foundMatch = false;
    osg::Node* tempParent = 0;
    for( osgUtil::LineSegmentIntersector::Intersections::iterator itr =
        intersections.begin(); itr != intersections.end(); ++itr )
    {
        objectHit = *( itr->nodePath.rbegin() );
        
        ves::xplorer::scenegraph::FindParentWithNameVisitor 
        findParent( objectHit, "VES_TextTexture", false );
        
        tempParent = findParent.GetParentNode();
        
        //size_t found = objectName.find( "VES_TextTexture" );
        if( tempParent )
        {
            //std::string objectName = tempParent->getName();
            //std::cout << "name " << objectName << std::endl;
            //std::cout << "found " << objectName << std::endl;
            //tempParent = objectHit;
            foundMatch = true;
            break;
        }
    }
    //Update which one is in front
    if( foundMatch )
    {
        ves::xplorer::scenegraph::DCS* tempKey = 
            static_cast< ves::xplorer::scenegraph::DCS* >( tempParent );
        m_groupedTextTextures->SetTextureUpdateAnimationOn( false );
        m_groupedTextTextures->MakeTextureActive( tempKey );
        m_groupedTextTextures->SetTextureUpdateAnimationOn( true );
        const std::string partName = 
            m_groupedTextTextures->GetKeyForTexture( tempKey );
        ves::xplorer::scenegraph::DCS* tempModelNodes = 
            mModel->GetModelCADHandler()->
            GetAssembly( mModel->GetModelCADHandler()->GetRootCADNodeID() );
        std::set< std::string > setOfParts;
        for( std::vector< std::string >::const_iterator 
            it = m_assemblyPartNumbers.begin(); 
            it != m_assemblyPartNumbers.end(); ++it)
        {
            setOfParts.insert( *it );
        }
        ves::xplorer::scenegraph::HighlightNodesByNameVisitor
            highlightNodes( tempModelNodes, setOfParts, true, true, osg::Vec3( 0.57255, 0.34118, 1.0 ) );
        
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor
        highlight( tempModelNodes, partName, true, true );//,
        //osg::Vec3( 0.34118, 1.0, 0.57255, 1.0 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::ClearDatabaseUserTables()
{
    Poco::Data::Session session("SQLite", m_dbFilename );
    
    Statement select( session );
    try
    {
        std::string queryString = 
            "SELECT name FROM sqlite_master WHERE tbl_name LIKE 'User_Table_%'";
        select << queryString.c_str(),now;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        std::cout << "UI Part Number query is bad." << std::endl;
        return;
    }
    
    // create a RecordSet 
    Poco::Data::RecordSet tableRS(select);
    
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = tableRS.moveFirst();
    }
    catch( ... )
    {
        return;
    }
    
    while (more)
    {
        std::string tableName = tableRS[0].convert<std::string>();
        tableName = "DROP TABLE " + tableName;
        session << tableName, now;
        more = tableRS.moveNext();
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::HighlightPartsInJoinedTabled( const std::string& queryString )
{
    //Work with the first table
    std::string table = m_tableNames.first;
    osg::Vec3 glowColor( 0.34118, 1.0, 0.57255 );
    mCommunicationHandler->SendConductorMessage( "Creating DB query..." );

    QueryTableAndHighlightParts( table, glowColor );
    
    mCommunicationHandler->SendConductorMessage( "Finished DB query..." );
    
    //Work with the first table
    table = m_tableNames.second;
    glowColor = osg::Vec3( 0.57255, 0.34118, 1.0 );
    mCommunicationHandler->SendConductorMessage( "Creating DB query..." );
    
    QueryTableAndHighlightParts( table, glowColor );
    
    mCommunicationHandler->SendConductorMessage( "Finished DB query..." );
    
    //Now if we are joining tables we are going to need to color the left 
    //and the right table
    mCommunicationHandler->SendConductorMessage( "Creating DB query..." );
    
    QueryInnerJoinAndHighlightParts( queryString );
    
    mCommunicationHandler->SendConductorMessage( "Finished DB query..." );    
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::QueryTableAndHighlightParts( 
    const std::string& tableName, osg::Vec3& glowColor )
{
    //ves::open::xml::DataValuePairPtr stringDvp = 
    //m_currentCommand->GetDataValuePair( "DISPLAY_TEXT_FIELDS" );
    
    //std::vector<std::string> stringArray;
    
    //select << "SELECT Part_Number, Description, Claims FROM Parts",
    //select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
    Poco::Data::Session session( "SQLite", m_dbFilename );
    Statement select( session );
    std::string table = tableName;
    try
    {
        table = "SELECT * FROM " + table;
        select << table, now;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        mCommunicationHandler->SendConductorMessage( "Query is bad." );
        return;
    }
    
    //RenderTextualDisplay( false );
    //bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
    //boost::ignore_unused_variable_warning( removed );
    
    //m_groupedTextTextures = 
    //new ves::xplorer::scenegraph::GroupedTextTextures();
    
    // create a RecordSet 
    Poco::Data::RecordSet rs(select);
    std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    std::ostringstream outString;
    outString << "Number of parts found " << numQueries;
    mCommunicationHandler->SendConductorMessage( outString.str() );
    if( numQueries == 0 )
    {
        mCommunicationHandler->SendConductorMessage( "No parts found." );
        return;
    }
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }
    
    std::string partNumber;
    std::string partNumberHeader;
    std::set< std::string > setOfParts;
    while (more)
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName(col);
            boost::algorithm::to_lower( partNumberHeader );
            if( partNumberHeader == "part_number" )
            {
                partNumber = rs[col].convert<std::string>();
                //tempText->SetTitle( partNumber );
                m_assemblyPartNumbers.push_back( partNumber );
            }
        }

        setOfParts.insert( partNumber );
        more = rs.moveNext();
    }
    ves::xplorer::scenegraph::HighlightNodesByNameVisitor 
        highlight( m_cadRootNode, setOfParts, true, true, glowColor );

    if( m_currentStatement )
    {
        delete m_currentStatement;
    }
    m_currentStatement = new Statement( select );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::QueryInnerJoinAndHighlightParts( const std::string& queryString )
{
    //select << "SELECT Part_Number, Description, Claims FROM Parts",
    //select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
    Poco::Data::Session session("SQLite", m_dbFilename );
    Statement select( session );
    try
    {
        //select << queryString.c_str(),
        //into( m_selectedAssembly ),
        //now;
        // a simple query
        select << queryString.c_str(),now;
        //select.execute();
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        mCommunicationHandler->SendConductorMessage( "Query is bad." );
        return;
    }
    
    //RenderTextualDisplay( false );
    //bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
    //boost::ignore_unused_variable_warning( removed );
    
    //m_groupedTextTextures = 
    //    new ves::xplorer::scenegraph::GroupedTextTextures();
    
    // create a RecordSet 
    Poco::Data::RecordSet rs(select);
    std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    std::ostringstream outString;
    outString << "Number of parts found " << numQueries;
    mCommunicationHandler->SendConductorMessage( outString.str() );
    if( numQueries == 0 )
    {
        mCommunicationHandler->SendConductorMessage( "No parts found." );
        return;
    }
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }
    
    std::string partNumber;
    std::string partNumberHeader;
    std::set< std::string > setOfParts;
    while (more)
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName(col);
            boost::algorithm::to_lower( partNumberHeader );
            if( partNumberHeader == "part_number" )
            {
                partNumber = rs[col].convert<std::string>();
                //tempText->SetTitle( partNumber );
                m_assemblyPartNumbers.push_back( partNumber );
                m_joinedPartNumbers.push_back( partNumber );
            }
        }
        setOfParts.insert( partNumber );
        more = rs.moveNext();
    }
    ves::xplorer::scenegraph::HighlightNodesByNameVisitor 
        highlight( m_cadRootNode, setOfParts, true, true );
    
    if( m_currentStatement )
    {
        delete m_currentStatement;
    }
    m_currentStatement = new Statement( select );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::QueryUserDefinedAndHighlightParts( const std::string& queryString )
{
    std::cout << "WarrantyToolGP::QueryUserDefinedAndHighlightParts " << queryString << std::endl << std::flush;
    //select << "SELECT Part_Number, Description, Claims FROM Parts",
    //select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
    Poco::Data::Session session("SQLite", m_dbFilename );
    Statement select( session );
    try
    {
        select << queryString , now;
    }
    catch( Poco::Data::DataException& ex )
    {
        std::cout << "WarrantyToolGP::QueryUserDefinedAndHighlightParts: exception A " << std::endl << std::flush;
        std::cout << ex.displayText() << std::endl;
        return;
    }
    catch( ... )
    {
        mCommunicationHandler->SendConductorMessage( "Query is bad." );
        return;
    }
    
    if( !queryString.compare( 0, 12, "CREATE TABLE" ) )
    {
        //boost::find_first( queryString, " AS " );
        boost::iterator_range<std::string::const_iterator>::const_iterator stringIter = boost::find_first( queryString, " AS " ).begin();
        //Get first position for string above
        std::size_t numEntries = ( stringIter - queryString.begin() ) + 4;
        std::string tempString = queryString;
        //Remove from begining to position of string above
        tempString.erase( 0, numEntries );
        //Now rerun select query and color by new color
        mCommunicationHandler->SendConductorMessage( "Created the table." );
        //return;
        try
        {
            Statement select2( session );
            select2 << tempString.c_str(),now;
            select.swap( select2 );
        }
        catch( Poco::Data::DataException& ex )
        {
            std::cout << "WarrantyToolGP::QueryUserDefinedAndHighlightParts: exception B " << std::endl << std::flush;
            std::cout << ex.displayText() << std::endl;
            return;
        }       
    }
    // create a RecordSet
    Poco::Data::RecordSet rs(select);
    std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    std::ostringstream outString;
    outString << "Number of parts found " << numQueries;
    mCommunicationHandler->SendConductorMessage( outString.str() );
    if( numQueries == 0 )
    {
        mCommunicationHandler->SendConductorMessage( "No parts found." );
        return;
    }
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }
    
    bool failedLoad = false;
    std::string partNumber;
    std::string partNumberHeader;
    std::string partText;
    osg::Vec3 nullGlowColor( 0.57255, 0.34118, 1.0 );
    std::set< std::string > setOfParts;
    while (more)
    {
        for (std::size_t col = 0; col < cols; ++col)
        {
            partNumberHeader = rs.columnName(col);
            boost::algorithm::to_lower( partNumberHeader );
            if( partNumberHeader == "part_number" )
            {
                partNumber = rs[col].convert<std::string>();
//                tempText->SetTitle( partNumber );
                m_assemblyPartNumbers.push_back( partNumber );
            }
        }
        setOfParts.insert( partNumber );
        more = rs.moveNext();
    }
    ves::xplorer::scenegraph::HighlightNodesByNameVisitor 
        highlightNodes( m_cadRootNode, setOfParts, true, true, nullGlowColor );
    
    if( m_currentStatement )
    {
        delete m_currentStatement;
    }
    m_currentStatement = new Statement( select );
    
    if( !failedLoad )
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor
            highlight( m_cadRootNode, m_assemblyPartNumbers.at( 0 ), true, true );//,
            //osg::Vec3( 0.34118, 1.0, 0.57255 ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::SaveCurrentQuery( const std::string& filename )
{
    if( !m_currentStatement )
    {
        return;
    }
    // create a RecordSet 
    Poco::Data::RecordSet rs(*m_currentStatement);
    std::size_t cols = rs.columnCount();
    size_t numQueries = rs.rowCount();
    std::ostringstream outString;
    outString << "Number of parts found " << numQueries;
    mCommunicationHandler->SendConductorMessage( outString.str() );
    if( numQueries == 0 )
    {
        mCommunicationHandler->SendConductorMessage( "No parts found." );
        return;
    }
    // iterate over all rows and columns
    bool more = false;
    try
    {
        more = rs.moveFirst();
    }
    catch( ... )
    {
        return;
    }
    
    std::ofstream statementExport( filename.c_str() );
    
    for( size_t i = 0; i < cols; ++i )
    {
        statementExport << rs.columnName( i );
        if( i != (cols-1) )
        {
            statementExport << ",";
        }
    }
    statementExport << std::endl;
    
    while( more )
    {        
        for( std::size_t col = 0; col < cols; ++col )
        {
            statementExport << rs[col].convert<std::string>();
            if( col != (cols-1) )
            {
                statementExport << ",";
            }
        }
        more = rs.moveNext();
        statementExport << std::endl;
    }
    statementExport.close();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::SetMouseSelection( bool const& checked )
{
    m_mouseSelection = checked;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::ToggleUnselected( bool const& checked )
{
    m_cadRootNode = mModel->GetModelCADHandler()->
        GetAssembly( mModel->GetModelCADHandler()->GetRootCADNodeID() );
    if( !m_cadRootNode )
    {
        std::cout << "m_cadRootNode is invalid" << std::endl << std::flush;
        return;
    }

    std::vector< std::string > lowerCasePartNumbers;
    std::string partNum;
    if( m_joinedPartNumbers.size() == 0)
    {
        for( size_t i = 0; i < m_assemblyPartNumbers.size(); ++i )
        {
            partNum = m_assemblyPartNumbers.at( i );
            boost::algorithm::to_lower( partNum );
            lowerCasePartNumbers.push_back( partNum );
        }
    }
    else
    {
        for( size_t i = 0; i < m_joinedPartNumbers.size(); ++i )
        {
            partNum = m_joinedPartNumbers.at( i );
            boost::algorithm::to_lower( partNum );
            lowerCasePartNumbers.push_back( partNum );
        }
    }

    size_t numChildren = m_cadRootNode->getNumChildren();
    for( size_t i = 0; i < numChildren; ++i )
    {
        osg::Group* childNode = m_cadRootNode->getChild( i )->asGroup();
        unsigned int nodeMask = childNode->getNodeMask();
        if( nodeMask )
        {
            //We know there is only 1 child because we are dealing 
            //with CADEntity
            osg::Node* tempChild = childNode->getChild( 0 );
            if( checked )
            {                
                ves::xplorer::scenegraph::util::ToggleNodesVisitor
                    toggleNodes( tempChild, false, lowerCasePartNumbers );
            }
            else
            {
                ves::xplorer::scenegraph::util::ToggleNodesVisitor
                    toggleNodes( tempChild, true, lowerCasePartNumbers );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::HighlightPart( const std::string& partNumber )
{
    m_assemblyPartNumbers.clear();
    m_joinedPartNumbers.clear();
    //Highlight the respective node
    //Make a user specified part glow
    transparentEnable( m_cadRootNode, 0.3f );

    mAddingParts = false;
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor
        highlight2( m_cadRootNode, "", false, true );
    //Highlight part
    m_lastPartNumber = partNumber;
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor
        highlight( m_cadRootNode, m_lastPartNumber, true, true );
    //RenderTextualDisplay( true );
}
////////////////////////////////////////////////////////////////////////////////
