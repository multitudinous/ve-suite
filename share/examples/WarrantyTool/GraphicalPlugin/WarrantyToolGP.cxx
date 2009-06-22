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

#include <ves/xplorer/scenegraph/CADEntity.h>

#include <sstream>
#include <iostream>
#include <fstream>

using namespace ves::xplorer::scenegraph;
using namespace warrantytool;

////////////////////////////////////////////////////////////////////////////////
WarrantyToolGP::WarrantyToolGP()
:
PluginBase(),
mAddingParts( false )
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
    ;
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
        ves::xplorer::scenegraph::HighlightNodeByNameVisitor 
            highlight( mDCS.get(), dvp->GetDataString() );
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
    std::vector< std::string > prtnumbers = csvDataMap[ 2 ];
    mPartNumberDescriptions = csvDataMap[ 3 ];
    mLoadedPartNumbers = csvDataMap[ 2 ];
}
////////////////////////////////////////////////////////////////////////////////
