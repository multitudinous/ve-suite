/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

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

#include <ves/xplorer/scenegraph/util/OpacityVisitor.h>
#include <ves/xplorer/scenegraph/util/MaterialInitializer.h>
#include <ves/xplorer/scenegraph/util/FindChildWithNameVisitor.h>
#include <ves/xplorer/scenegraph/HighlightNodeByNameVisitor.h>
#include <ves/xplorer/scenegraph/FindParentWithNameVisitor.h>

#include <ves/xplorer/scenegraph/CADEntity.h>
#include <ves/xplorer/scenegraph/TextTexture.h>
#include <ves/xplorer/scenegraph/GroupedTextTextures.h>

#include <ves/xplorer/environment/TextTextureCallback.h>
#include <ves/xplorer/environment/HeadPositionCallback.h>

#include <ves/xplorer/device/KeyboardMouse.h>

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
#include <Poco/Data/SessionFactory.h>
#include <Poco/Data/Session.h>
#include <Poco/Data/RecordSet.h>
#include <Poco/Data/SQLite/Connector.h>
#include <vector>

#include <boost/lexical_cast.hpp>
#include <boost/concept_check.hpp>
#include <boost/algorithm/string/trim.hpp>
#include <boost/algorithm/string/replace.hpp>

using namespace Poco::Data;

////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::WarrantyToolGP()
:
PluginBase(),
mAddingParts( false ),
m_keyboard( 0 ),
m_groupedTextTextures( 0 )
{
    //Needs to match inherited UIPluginBase class name
    mObjectName = "WarrantyToolUI";

    mEventHandlerMap[ "WARRANTY_TOOL_PART_TOOLS" ] = this;
    mEventHandlerMap[ "WARRANTY_TOOL_DB_TOOLS" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::~WarrantyToolGP()
{
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::InitializeNode(
    osg::Group* veworldDCS )
{
    PluginBase::InitializeNode( veworldDCS );
    
    m_keyboard = 
        dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );
    //Load model
    /*cadEntity = new CADEntity( "Models/test2_head_osg26-share.ive",
              mDCS.get(), false, false, NULL );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList = cadEntity->GetDCS()->getDescriptions();
    descriptorsList.push_back( "Part" );
    cadEntity->GetDCS()->setDescriptions( descriptorsList );*/
    //Make transparent
    //ves::xplorer::scenegraph::util::MaterialInitializer 
    //    material_initializer( cadEntity->GetDCS() );
   /* ves::xplorer::scenegraph::util::OpacityVisitor 
        opVisitor( cadEntity->GetDCS(), true, true, 1.0f );
    ves::xplorer::scenegraph::util::OpacityVisitor 
        opVisitor1( cadEntity->GetDCS(), false, true, 0.4f );
    //Highlight part
    ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
        highlight( cadEntity->GetDCS(), "AN220959", mDCS.get() );*/

    //ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
    //    highlight2( cadEntity->GetDCS(), "AN220959", mDCS.get(), false );
    //ves::xplorer::scenegraph::util::OpacityVisitor 
    //    opVisitor2( cadEntity->GetDCS(), false, true, 1.0f );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::PreFrameUpdate()
{
    //If the keymbaord mouse selected something
    //std::cout << " here 1 " << std::endl;
    //ves::xplorer::device::KeyboardMouse* kbMouse = dynamic_cast< ves::xplorer::device::KeyboardMouse* >( mDevice );    
    if( !m_keyboard )
    {
        return;
    }
    
    if( !m_groupedTextTextures.valid() )
    {
        return;
    }

    if( !m_groupedTextTextures->AnimationComplete() )
    {
        m_groupedTextTextures->UpdateTexturePosition();
    }

    if( !m_keyboard->GetMousePickEvent() )
    {
        return;
    }

    //if( m_groupedTextTextures.valid() )
    {
        //Get the intersection visitor from keyboard mouse or the wand
        osg::ref_ptr< osgUtil::LineSegmentIntersector > intersectorSegment = 
            m_keyboard->GetLineSegmentIntersector();
        
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
                std::string objectName = tempParent->getName();
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
            //ves::xplorer::scenegraph::DCS* tempKey = static_cast< ves::xplorer::scenegraph::DCS* >( static_cast< osg::Group* >( objectHit )->getParent( 0 ) );
            ves::xplorer::scenegraph::DCS* tempKey = static_cast< ves::xplorer::scenegraph::DCS* >( tempParent );
            m_groupedTextTextures->MakeTextureActive( tempKey );
            const std::string partName = m_groupedTextTextures->GetKeyForTexture( tempKey );
            ves::xplorer::scenegraph::DCS* tempModelNodes = this->mModel->GetModelCADHandler()->GetAssembly( mModel->GetModelCADHandler()->GetRootCADNodeID() );

            for( std::vector< std::string >::const_iterator it = m_assemblyPartNumbers.begin(); it != m_assemblyPartNumbers.end(); ++it)
            {
                /*std::ostringstream tempTextData;
                tempTextData
                    << "Part Number: " << it->get<0>() << "\n"
                    << "Description: " << it->get<1>() << "\n"
                    << "Claims: " << it->get<2>() << "\n"
                    << "FPM: " << it->get<4>();
                */
                //ves::xplorer::scenegraph::TextTexture* tempText = new ves::xplorer::scenegraph::TextTexture();
                //std::string tempKey = "test_" + it->get<0>();
                //boost::lexical_cast<std::string>( std::distance( assem.begin(), it) );
                //std::string partText = tempTextData.str();
                //std::cout << " here 1 " << partText << std::endl;
                //tempText->UpdateText( partText );
                //m_groupedTextTextures->AddTextTexture( it->get<0>(), tempText );

                ves::xplorer::scenegraph::HighlightNodeByNameVisitor
                    highlight( tempModelNodes, (*it), true, true, osg::Vec4( 0.57255, 0.34118, 1.0, 1.0 ) );
            }

            ves::xplorer::scenegraph::HighlightNodeByNameVisitor
                highlight( tempModelNodes, partName, true, true,
                          osg::Vec4( 0.34118, 1.0, 0.57255, 1.0 ) );
        }
    }
    //If we are in interactive mode to mouse over things
        //Find part we are over
        //active text texture with the info
        //highlight all associated nodes
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::SetCurrentCommand( ves::open::xml::CommandPtr command )
{
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
        if( dvp->GetDataName() == "RESET" )
        {
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false, true );
            //Make everything opaque
            ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor( mDCS.get(), false, false, 1.0f );
            mAddingParts = false;
        }
        else if( dvp->GetDataName() == "ADD" )
        {
            //Highlight the respective node
            //Make a user specified part glow
            if( !mAddingParts )
            {
                ves::xplorer::scenegraph::util::OpacityVisitor 
                opVisitor1( mDCS.get(), false, true, 0.3f );
                mAddingParts = true;
            }
            //Highlight part
            m_lastPartNumber = dvp->GetDataString();
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
                highlight( mDCS.get(), m_lastPartNumber, true, true );
            RenderTextualDisplay( true );
        }
        else if( dvp->GetDataName() == "CLEAR" )
        {
            ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor1( mDCS.get(), false, true, 0.3f );
            
            ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false, true );
        }
        else if( dvp->GetDataName() == "WARRANTY_FILE" )
        {
            ParseDataFile( dvp->GetDataString() );
            for( size_t i = 1; i < mLoadedPartNumbers.size(); ++i )
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
            }
            m_keyboard->SetProcessSelection( false );
        }
    }
    else if( commandName == "WARRANTY_TOOL_DB_TOOLS" )
    {
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
    std::string sLine;
    std::string sCol1, sCol3, sCol4;
    //double fCol2;
    //int iCol5, iCol6;
    
    CSVParser parser;
    
    std::ifstream infile( csvFilename.c_str() );
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
    
    //std::cout << networkData << std::endl;
    //build network information
    //CreateNetworkInformation( networkData );
    std::istringstream iss;
    iss.str( networkData );
    
    std::getline(iss, sLine); // Get a line
    parser << sLine; // Feed the line to the parser
    size_t columnCount = 0;
    std::map< int, std::vector< std::string > > csvDataMap;
    
    size_t partNumberColumn = 0;
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
        ReplaceSpacesCharacters( sCol1 );
        data.push_back( sCol1 );
        //std::vector< std::string > data;
        csvDataMap[ columnCount ] = data;
        if( "Part_Number" == sCol1 )
        {
            partNumberColumn = columnCount;
        }
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
        
        boost::algorithm::replace_all( sLine, "'", "" );

        parser << sLine; // Feed the line to the parser
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            StripDollarCharacters( sCol1 );
            csvDataMap[ i ].push_back( sCol1 );
        }
    }
    //iss.close();

    mLoadedPartNumbers = csvDataMap[ partNumberColumn ];
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
        ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor1( mDCS.get(), false, true, 0.3f );
        mAddingParts = true;
    }
    
    //Open DB
    //Add data
    //close connection
    CreateTextTextures();
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

        std::string displayString;
        std::pair< std::string, std::string > displayPair;
        std::vector< std::pair< std::string, std::string > > displayVector;
        std::map< std::string, std::vector< std::pair< std::string, std::string > > >::iterator iter;
        iter = m_dataMap.find( m_lastPartNumber );
        if( iter == m_dataMap.end() )
        {
            return;
        }
        displayVector = iter->second;
        for( size_t i = 0; i < displayVector.size(); ++i )
        {
            displayPair = displayVector.at( i );
            displayString = displayString + displayPair.first + " " +  displayPair.second + "\n";
        }
        mModelText->UpdateText( displayString );
        mModelText->SetTitle( m_lastPartNumber );

        //std::cout << displayString << std::endl;
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
    // register SQLite connector
    Poco::Data::SQLite::Connector::registerConnector();

    // create a session
    Poco::Data::Session session("SQLite", "sample.db");
    
    // drop sample table, if it exists
    session << "DROP TABLE IF EXISTS Parts", now;

    // (re)create table
    //session << "CREATE TABLE Parts (Part_Number VARCHAR, Description VARCHAR, Claims INT, Claim_Cost DOUBLE, FPM DOUBLE, CCPM DOUBLE, By VARCHAR)", now;
    
    std::ostringstream createCommand;
    createCommand << "CREATE TABLE Parts (";
    std::vector< std::pair< std::string, std::string > > tempData = m_dataMap.begin()->second;
    for( size_t i = 0; i < tempData.size(); ++i )
    {
        bool isString = false;
        try
        {
            double test = boost::lexical_cast<double>( tempData.at( i ).second );
            boost::ignore_unused_variable_warning( test );   
        }
        catch( boost::bad_lexical_cast& ex )
        {
            std::cout << "Is string data " << tempData.at( i ).first << std::endl;
            std::cout << "Data is " << tempData.at( i ).second << std::endl;
            std::cout << ex.what() << std::endl;
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
        Statement insert( session );
        try
        {
            insert << insertCommand.str(), now;
        }
        catch( Poco::Data::DataException& ex )
        {
            std::cout << ex.displayText() << std::endl;
            //std::string ses = insert.toString();
            //std::cout << ses << std::endl;
        }
        insertCommand.str("");
    }
    
    
	//insert << "INSERT INTO Parts VALUES(?, ?, ?, ?, ?, ?, ?)",
    //    use(assem), now;
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateTextTextures()
{

    m_textTrans = 
        new ves::xplorer::scenegraph::DCS();
    m_textTrans->getOrCreateStateSet()->addUniform(
        new osg::Uniform( "glowColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) ) );
        
    mDCS->addChild( m_textTrans.get() );

    //mModelText->setUpdateCallback( 
    //    new ves::xplorer::environment::TextTextureCallback( mModelText.get() ) );
    m_textTrans->setUpdateCallback( 
        new ves::xplorer::environment::HeadPositionCallback() );
    //static_cast< osg::PositionAttitudeTransform* >( 
    //    mModelText->getParent( 0 ) )->setPosition( osg::Vec3d( 0, 0, 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateDBQuery( ves::open::xml::DataValuePairPtr dvp )
{
    ves::open::xml::DataValuePairPtr stringDvp = 
        m_currentCommand->GetDataValuePair( "DISPLAY_TEXT_FIELDS" );
    
    std::vector<std::string> stringArray = 
        boost::static_pointer_cast<ves::open::xml::OneDStringArray>( 
        stringDvp->GetDataXMLObject() )->GetArray();
    
    // a simple query
    std::string queryString = dvp->GetDataString();
    //m_selectedAssembly.clear();
    m_assemblyPartNumbers.clear();
    
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false, true );
        
        ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor1( mDCS.get(), false, true, 0.3f );
    }

    //select << "SELECT Part_Number, Description, Claims FROM Parts",
    //select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
    Poco::Data::Session session("SQLite", "sample.db");
    Statement select( session );
    try
    {
        //select << queryString.c_str(),
        //into( m_selectedAssembly ),
        //now;
        select << queryString.c_str(),now;
        select.execute();
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

    RenderTextualDisplay( false );
    bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );
    boost::ignore_unused_variable_warning( removed );

    m_groupedTextTextures = 
        new ves::xplorer::scenegraph::GroupedTextTextures();
        
	// create a RecordSet 
	Poco::Data::RecordSet rs(select);
	std::size_t cols = rs.columnCount();
	// iterate over all rows and columns
	bool more = rs.moveFirst();
	while (more)
	{
        ves::xplorer::scenegraph::TextTexture* tempText = 
            new ves::xplorer::scenegraph::TextTexture();
        float textColor[ 4 ] = { 0.0, 0.0, 0.0, 1.0 };
        tempText->SetTextColor( textColor );

        std::ostringstream tempTextData;
        std::string partNumber;
		for (std::size_t col = 0; col < cols; ++col)
		{
            std::string partNumberHeader = rs.columnName(col);
            //std::cout << rs.columnName(col) << std::endl;
            if( partNumberHeader == "Part_Number" )
            {
                partNumber = rs[col].convert<std::string>();
                tempText->SetTitle( partNumber );
                m_assemblyPartNumbers.push_back( partNumber );
            }

			//std::cout << rs[col].convert<std::string>() << " ";
            std::vector< std::string >::const_iterator iter = 
                std::find( stringArray.begin(), stringArray.end(), rs.columnName(col) );
            
            if( iter != stringArray.end() )
            {
                tempTextData << rs.columnName(col) << ": " 
                    << rs[col].convert<std::string>() << "\n";
            }
		}
        std::string partText = tempTextData.str();
        tempText->UpdateText( partText );
        m_groupedTextTextures->AddTextTexture( partNumber, tempText );
        
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight( mDCS.get(), partNumber, true, true, 
            osg::Vec4( 0.57255, 0.34118, 1.0, 1.0 ) );

		more = rs.moveNext();
	}
    m_textTrans->addChild( m_groupedTextTextures.get() );

/*
    //ves::xplorer::scenegraph::util::OpacityVisitor 
    //    opVisitor1( mDCS.get(), false, true, 0.3f );
    //mAddingParts = true;
    bool removed = m_textTrans->removeChild( m_groupedTextTextures.get() );

    m_groupedTextTextures = 
        new ves::xplorer::scenegraph::GroupedTextTextures();

    for( Assembly::const_iterator it = m_selectedAssembly.begin(); it != m_selectedAssembly.end(); ++it )
    {
        std::cout
            << "Part Number: " << it->get<0>() 
            << ", Description: " << it->get<1>() 
            << ", Claims: " << it->get<2>()
            << ", Claim Cost: " << it->get<3>()
            << ", FPM: " << it->get<4>() 
            << ", CCPM: " << it->get<5>()
            << ", By: " << it->get<6>() << std::endl;
        
        std::ostringstream tempTextData;
        tempTextData
            << "Part Number: " << it->get<0>() << "\n"
            << "Description: " << it->get<1>() << "\n"
            << "Claims: " << it->get<2>() << "\n"
            << "Claim Cost: " << it->get<3>() << "\n"
            << "FPM: " << it->get<4>() << "\n"
            << "CCPM: " << it->get<5>() << "\n"
            << "By: " << it->get<6>();
        
        ves::xplorer::scenegraph::TextTexture* tempText = 
            new ves::xplorer::scenegraph::TextTexture();
        //std::string tempKey = "test_" + it->get<0>(); 
        //boost::lexical_cast<std::string>( std::distance( assem.begin(), it) );
        std::string partText = tempTextData.str();
        //std::cout << " here 1 " << partText << std::endl;
        tempText->UpdateText( partText );
        tempText->SetTitle( it->get<0>() );
        m_groupedTextTextures->AddTextTexture( it->get<0>(), tempText );
        
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight( mDCS.get(), it->get<0>(), true, osg::Vec4( 0.57255, 0.34118, 1.0, 1.0 ) );
    }
    m_textTrans->addChild( m_groupedTextTextures.get() );
    //m_textTrans->getOrCreateStateSet()->setAttributeAndModes(
    //                               new osg::Depth( osg::Depth::ALWAYS ),
    //                               osg::StateAttribute::ON | osg::StateAttribute::PROTECTED );
    
    //ves::xplorer::scenegraph::util::OpacityVisitor 
    //    opVisitor1( m_textTrans.get(), false, true, 0.3f );
*/
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::RemoveSelfFromSG()
{
    PluginBase::RemoveSelfFromSG();
    m_keyboard->SetProcessSelection( true );
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
