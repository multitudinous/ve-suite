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

// --- VE-Suite Includes --- //
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

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
#include <Poco/Data/SQLite/Connector.h>
#include <vector>

#include <boost/lexical_cast.hpp>

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

    mEventHandlerMap[ "CAMERA_GEOMETRY_ON_OFF" ] = this;
}
////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::~WarrantyToolGP()
{
    ;
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
    if( m_keyboard )
    {
        //Get the intersection visitor from keyboard mouse or the wand
        osg::ref_ptr< osgUtil::LineSegmentIntersector > intersectorSegment = 
            m_keyboard->GetLineSegmentIntersector();
        
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
                std::cout << "name " << objectName << std::endl;
                std::cout << "found " << objectName << std::endl;
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
    const std::string commandName = command->GetCommandName();
    ves::open::xml::DataValuePairPtr dvp = command->GetDataValuePair( 0 );
    //Before anything else remove the glow if there is glow
    
    if( dvp->GetDataName() == "RESET" )
    {
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false );
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
            highlight( mDCS.get(), m_lastPartNumber );
        RenderTextualDisplay( true );
    }
    else if( dvp->GetDataName() == "CLEAR" )
    {
        ves::xplorer::scenegraph::util::OpacityVisitor 
            opVisitor1( mDCS.get(), false, true, 0.3f );

        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight2( mDCS.get(), "", false );
    }
    else if( dvp->GetDataName() == "WARRANTY_FILE" )
    {
        //std::vector< std::string > prts;
        ParseDataFile( dvp->GetDataString() );
        for( size_t i = 0; i < mLoadedPartNumbers.size(); ++i )
        {
            ves::xplorer::scenegraph::util::FindChildWithNameVisitor 
                childVisitor( mDCS.get(), mLoadedPartNumbers.at( i ), false );
            if( childVisitor.FoundChild() )
            {
                std::cout << "Found match for " << mLoadedPartNumbers.at( i ) << std::endl;
            }
        }
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
    double fCol2;
    int iCol5, iCol6;
    
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
        data.push_back( sCol1 );
        //std::vector< std::string > data;
        csvDataMap[ columnCount ] = data;
        columnCount += 1;
    }
    
    while( !iss.eof() ) 
    {
        std::getline(iss, sLine); // Get a line
        if (sLine == "")
            continue;
        
        parser << sLine; // Feed the line to the parser
        std::cout << sLine << std::endl;
        for( size_t i = 0; i < columnCount; ++i )
        {
            parser >> sCol1;
            csvDataMap[ i ].push_back( sCol1 );
        }
    }
    //iss.close();
    //std::vector< std::string > prtnumbers = csvDataMap[ 2 ];
    mPartNumberDescriptions = csvDataMap[ 3 ];
    mLoadedPartNumbers = csvDataMap[ 2 ];
    
    for( size_t i = 1; i < mLoadedPartNumbers.size(); ++i )
    {
        std::vector< std::pair< std::string, std::string > > partData;
        for( size_t j = 0; j < columnCount; ++j )
        {
            partData.push_back( std::pair< std::string, std::string >( csvDataMap[ j ].at( 0 ), csvDataMap[ j ].at( i ) ) );
            std::cout << csvDataMap[ j ].at( 0 ) << " " <<  csvDataMap[ j ].at( i ) << std::endl;
        }
        m_dataMap[ csvDataMap[ 2 ].at( i ) ] = partData;
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
            CreateTextTextures();
        }
        else
        {
            mModelText->setNodeMask( 1 );
        }
        
        std::string displayString;
        std::pair< std::string, std::string > displayPair;
        std::vector< std::pair< std::string, std::string > > displayVector;
        std::map< std::string, std::vector< std::pair< std::string, std::string > > >::iterator iter;
        iter = m_dataMap.find( m_lastPartNumber );
        displayVector = ( iter->second );
        for( size_t i = 0; i < displayVector.size(); ++i )
        {
            displayPair = displayVector.at( i );
            displayString = displayString + displayPair.first + " " +  displayPair.second + "\n";
        }
        mModelText->UpdateText( displayString );
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
	typedef Poco::Tuple< std::string, std::string, int, std::string, double, std::string > Part;
	typedef std::vector<Part> Assembly;
    
	// register SQLite connector
	Poco::Data::SQLite::Connector::registerConnector();
	
	// create a session
	Session session("SQLite", "sample.db");
    
	// drop sample table, if it exists
	session << "DROP TABLE IF EXISTS Parts", now;

	// (re)create table
	session << "CREATE TABLE Parts (Part_Number VARCHAR, Description VARCHAR, Claims INT, Claims_Cost VARCHAR, FPM DOUBLE, CCPM VARCHAR)", now;
	
	// insert some rows
	Assembly assem;

    std::map< std::string, std::vector< std::pair< std::string, std::string > > >::iterator iter;
    for( iter = m_dataMap.begin(); iter != m_dataMap.end(); ++iter )
    {
        std::vector< std::pair< std::string, std::string > > tempData;
        tempData = iter->second;
        Part tempPart;
        //for( size_t i = 0; i < tempData.size(); ++i )
        {
            tempPart.set< 0 >( tempData.at( 2 ).second );
            tempPart.set< 1 >( tempData.at( 3 ).second );
            tempPart.set< 2 >( boost::lexical_cast<int>( tempData.at( 4 ).second ) );
            tempPart.set< 3 >( tempData.at( 5 ).second );
            tempPart.set< 4 >( boost::lexical_cast<double>( tempData.at( 6 ).second ) );
            tempPart.set< 5 >( tempData.at( 7 ).second );
        }
        assem.push_back(tempPart);
    }
    
    
	Statement insert(session);
	insert << "INSERT INTO Parts VALUES(?, ?, ?, ?, ?, ?)",
    use(assem), now;
	std::cout << "create table 3  " << std::endl;

	assem.clear();
    
	// a simple query
	//select << "SELECT Part_Number, Description, Claims FROM Parts",
	//select << "SELECT Part_Number, Description, Claims FROM Parts WHERE Claims > 10 AND Claims_Cost > 1000",
	Statement select(session);
	select << "SELECT * FROM Parts WHERE Claims > 10",// AND FPM > 0.1",
    into(assem),
    now;
    
    //ves::xplorer::scenegraph::util::OpacityVisitor 
    //    opVisitor1( mDCS.get(), false, true, 0.3f );
    //mAddingParts = true;
    
    m_groupedTextTextures = 
        new ves::xplorer::scenegraph::GroupedTextTextures();

	for (Assembly::const_iterator it = assem.begin(); it != assem.end(); ++it)
	{
		std::cout 
            << "Part Number: " << it->get<0>() 
            << ", Description: " << it->get<1>() 
            << ", Claims: " << it->get<2>()
            << ", FPM: " << it->get<4>() << std::endl;
            
        std::ostringstream tempTextData;
        tempTextData
            << "Part Number: " << it->get<0>() << "\n"
            << "Description: " << it->get<1>() << "\n"
            << "Claims: " << it->get<2>() << "\n"
            << "FPM: " << it->get<4>();

        ves::xplorer::scenegraph::TextTexture* tempText = new ves::xplorer::scenegraph::TextTexture();
        //std::string tempKey = "test_" + it->get<0>(); 
        //boost::lexical_cast<std::string>( std::distance( assem.begin(), it) );
        std::string partText = tempTextData.str();
        //std::cout << " here 1 " << partText << std::endl;
        tempText->UpdateText( partText );
        m_groupedTextTextures->AddTextTexture( it->get<0>(), tempText );
        
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight( mDCS.get(), it->get<0>() );
	}
    m_textTrans->addChild( m_groupedTextTextures );

    Poco::Data::SQLite::Connector::unregisterConnector();
}
////////////////////////////////////////////////////////////////////////////////
void WarrantyToolGP::CreateTextTextures()
{

    m_textTrans = 
    new ves::xplorer::scenegraph::DCS();
    m_textTrans->getOrCreateStateSet()->addUniform(
        new osg::Uniform( "glowColor", osg::Vec4( 0.0, 0.0, 0.0, 1.0 ) ) );
    
    mModelText = new ves::xplorer::scenegraph::TextTexture();
    //m_textTrans->addChild( mModelText.get() );
    
    mDCS->addChild( m_textTrans.get() );

    //mModelText->setUpdateCallback( 
    //    new ves::xplorer::environment::TextTextureCallback( mModelText.get() ) );
    m_textTrans->setUpdateCallback( 
        new ves::xplorer::environment::HeadPositionCallback() );
    //static_cast< osg::PositionAttitudeTransform* >( 
    //    mModelText->getParent( 0 ) )->setPosition( osg::Vec3d( 0, 0, 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
