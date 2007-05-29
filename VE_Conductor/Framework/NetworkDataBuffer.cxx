/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/NetworkDataBuffer.h"

#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"
#include "VE_Conductor/GUIPlugin/UIPluginBase.h"
#include "VE_Conductor/GUIPlugin/UIDialog.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"
#include "VE_Conductor/Utilities/CORBAServiceList.h"

#include "VE_Conductor/DefaultPlugin/DefaultPlugin.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/Network.h"
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/StateInfo.h"

#include <sstream>

using namespace VE_XML;
using namespace VE_Conductor;

////////////////////////////////////////////////////////////////////////////////
NetworkDatabuffer::NetworkDatabuffer( void )
{ 
   VE_XML::Command nullCommand;
   nullCommand.SetCommandName( "NULL" );
   m_commandMap[ "NULL" ] = nullCommand;
}
////////////////////////////////////////////////////////////////////////////////
NetworkDatabuffer::~NetworkDatabuffer( void )
{
   m_commandMap.clear();
}
////////////////////////////////////////////////////////////////////////////////
VE_XML::Command& NetworkDatabuffer::GetCommand( std::string commandKey )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    std::map< std::string, VE_XML::Command >::iterator iter;
    iter = m_commandMap.find( commandKey );
    if ( iter == m_commandMap.end() )
    {
        return m_commandMap[ "NULL" ];
    }
    return iter->second;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkDatabuffer::SetCommand( std::string commandKey, 
                                      VE_XML::Command& command )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap[ commandKey ] = command;
}
////////////////////////////////////////////////////////////////////////////////
std::map< std::string, VE_XML::Command >& 
NetworkDatabuffer::GetCommandMap( void )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    return m_commandMap;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkDatabuffer::SetCommandMap( std::map< std::string, 
                                         VE_XML::Command >& tempMap )
{
    vpr::Guard<vpr::Mutex> val_guard( m_commandMapLock );
    m_commandMap = tempMap;
}
////////////////////////////////////////////////////////////////////////////////
void NetworkDatabuffer::LoadVESData( std::string xmlNetwork )
{
    if( xmlNetwork.empty() )
    {
        return;
    }
    
    // we are expecting that a network will be found
    VE_XML::VE_Model::Network veNetwork = 
        VE_Conductor::XMLDataBufferEngine::instance()->GetXMLNetworkDataObject( "Network" );
    
    for ( size_t i = 0; i < veNetwork.GetNumberOfLinks(); ++i )
    {
        //links.push_back( VE_Conductor::GUI_Utilities::Link( this ) );
        
        links.at( i ).SetFromPort( *(veNetwork.GetLink( i )->GetFromPort()) );
        links.at( i ).SetToPort( *(veNetwork.GetLink( i )->GetToPort()) );
        
        long moduleID;
        veNetwork.GetLink( i )->GetFromModule()->GetData( moduleID );
        links.at( i ).SetFromModule( moduleID );
        veNetwork.GetLink( i )->GetToModule()->GetData( moduleID );
        links.at( i ).SetToModule( moduleID );
        
        size_t numberOfPoints = veNetwork.GetLink( i )->GetNumberOfLinkPoints();
        for ( size_t j = 0; j < numberOfPoints; ++j )
        {
            std::pair< unsigned int, unsigned int > rawPoint = veNetwork.GetLink( i )->GetLinkPoint( j )->GetPoint();
            wxPoint point;
            point.x = rawPoint.first;
            point.y = rawPoint.second;
            links.at( i ).SetPoint( &point );
        }
        // Create the polygon for links
        links.at( i ).CalcLinkPoly();
    }

    // now lets create a list of them
    std::vector< VE_XML::VE_Model::Model > objectVector;
    std::vector< VE_XML::VE_Model::Model >::iterator objectIter;
    
    //objectVector = VE_Conductor::XMLDataBufferEngine::instance()->GetXMLModelDataObject( "Model" );
    size_t i = 0;
    for( objectIter = objectVector.begin(); objectIter != objectVector.end(); )
    {
        ++i;
        VE_XML::VE_Model::Model model = *objectIter;

        wxClassInfo* cls = wxClassInfo::FindClass( wxString(model.GetModelName().c_str(),wxConvUTF8) );
        // If the class has not had a custom module been created
        UIPluginBase* tempPlugin = 0;
        if ( cls == 0 )
        {
            tempPlugin = new DefaultPlugin();
        }
        else
        {
            tempPlugin = dynamic_cast< UIPluginBase* >( cls->CreateObject() );
        }
        //tempPlugin->SetNetworkFrame( this );
        tempPlugin->SetName( wxString(model.GetModelName().c_str(),wxConvUTF8) );
        tempPlugin->SetCORBAService( VE_Conductor::CORBAServiceList::instance() );
        if ( model.GetIconFilename() != "DefaultPlugin" )
        {   
            tempPlugin->SetImageIcon( model.GetIconFilename(), 
                                      model.GetIconRotation(), 
                                      model.GetIconMirror(), 
                                      model.GetIconScale() );
        }
        
        VE_Conductor::GUI_Utilities::Module temp_mod;
        unsigned int num = model.GetModelID();
        modules[ num ] = temp_mod;
        modules[ num ].SetPlugin( tempPlugin );
        modules[ num ].GetPlugin()->SetID( num );
        modules[ num ].SetClassName( model.GetModelName() );
        modules[ num ].GetPlugin()->SetVEModel( &model );
        //Second, calculate the polyes
        wxRect bbox = modules[ num ].GetPlugin()->GetBBox();
        int polynum = modules[ num ].GetPlugin()->GetNumPoly();
        POLY tmpPoly;
        tmpPoly.resize( polynum );
        modules[ num ].GetPlugin()->GetPoly(tmpPoly);
        VE_Conductor::GUI_Utilities::Polygon tempPoly;
        *(tempPoly.GetPolygon()) = tmpPoly;
        tempPoly.TransPoly( bbox.x, bbox.y, *(modules[ num ].GetPolygon()) ); //Make the network recognize its polygon 
    }
    
    //if ( !objectVector.empty() )
    {
        //backgroundColor.clear();
        VE_XML::User* userColor = new VE_XML::User();//dynamic_cast< VE_XML::User* >( objectVector.at( 0 ) );
        //Set user preferences
        std::vector< VE_XML::Command* > tempStates = userColor->GetUserStateInfo()->GetStateVector();
        std::map< std::string, VE_XML::Command > tempMap;
        for ( size_t i = 0; i < tempStates.size(); ++i )
        {
            VE_XML::Command* tempCommand = tempStates.at( i );
            tempMap[ tempCommand->GetCommandName() ] = (*tempCommand); 
        }
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
    }
    //else
    {
        //Set the user preferences to nothing since this 
        //ves file does not have anything
        std::map< std::string, VE_XML::Command > tempMap;
        UserPreferencesDataBuffer::instance()->SetCommandMap( tempMap );
        
        std::vector< double > backgroundColor;
        backgroundColor.clear();
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 0.0f );
        backgroundColor.push_back( 1.0f );
        
        VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( );
        dataValuePair->SetData(std::string("Background Color"),backgroundColor);
        VE_XML::Command* veCommand = new VE_XML::Command();
        veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
        veCommand->AddDataValuePair(dataValuePair);
        UserPreferencesDataBuffer::instance()->SetCommand( std::string("CHANGE_BACKGROUND_COLOR"), *veCommand );
        delete veCommand;
    }
    // Create the command and data value pairs
    
    VE_XML::Command colorCommand = UserPreferencesDataBuffer::instance()->
        GetCommand( "CHANGE_BACKGROUND_COLOR" );
    
    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( &colorCommand );
    /*
    // do this for tags
    DOMNodeList* subElements = doc->getDocumentElement()->getElementsByTagName( xercesString("veTag") );
    unsigned int numTags = subElements->getLength();
    // now lets create a list of them
    for ( unsigned int i = 0; i < numCommands; ++i )
    {
        VE_Model::Tag* temp = new VE_Model::Tag( doc );
        temp->SetObjectFromXMLData( dynamic_cast< DOMElement* >( subElements->item(i) ) );
        veTagVector.push_back( temp );
        tags.push_back( TAG );
        tags.back().text = wxString( veTagVector.back()->GetTagText().c_str() );
        tags.back().cons[0].x = veTagVector.back()->GetTagPoint( 0 )->GetPoint().first;
        tags.back().cons[0].y = veTagVector.back()->GetTagPoint( 0 )->GetPoint().second;
        tags.back().cons[1].x = veTagVector.back()->GetTagPoint( 1 )->GetPoint().first;
        tags.back().cons[1].y = veTagVector.back()->GetTagPoint( 1 )->GetPoint().second;
        tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().first;
        tags.back().box.x = veTagVector.back()->GetTagPoint( 2 )->GetPoint().second;
        // Create the polygon for tags
        tags.back().CalcTagPoly();
    }
    */
}
////////////////////////////////////////////////////////////////////////////////
void NetworkDatabuffer::NewVESData( bool promptClearXplorer )
{
    // Just clear the design canvas        
    if ( ( promptClearXplorer ) )
    {
        std::map< int, VE_Conductor::GUI_Utilities::Module >::iterator iter;
        for ( iter=modules.begin(); iter!=modules.end(); ++iter )
        {
            VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair(  std::string("UNSIGNED INT") );
            dataValuePair->SetDataName( "Object ID" );
            dataValuePair->SetDataValue( static_cast< unsigned int >( iter->first ) );
            VE_XML::Command* veCommand = new VE_XML::Command();
            veCommand->SetCommandName( std::string("DELETE_OBJECT_FROM_NETWORK") );
            veCommand->AddDataValuePair( dataValuePair );
            bool connected = VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
            //Clean up memory
            delete veCommand;
        }
    }
    
    links.clear();
    
    modules.clear();
    
    tags.clear();
}
////////////////////////////////////////////////////////////////////////////////
void NetworkDatabuffer::SetRenderWindow( wxWindow* renderWindow )
{
    this->renderWindow = renderWindow;
}

