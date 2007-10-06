/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 * Date modified: $Date: 2007-09-25 11:23:45 -0400 (Tue, 25 Sep 2007) $
 * Version:       $Rev: 9149 $
 * Author:        $Author: tjordan $
 * Id:            $Id: Network.cxx 9149 2007-09-25 15:23:45Z tjordan $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Framework/Canvas.h"
#include "VE_Open/XML/Model/System.h"
#include "VE_Open/XML/Model/SystemStrongPtr.h"
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"

#include <wx/dcbuffer.h>

using namespace VE_Conductor;

BEGIN_EVENT_TABLE(Canvas, wxScrolledWindow)
    EVT_PAINT( Canvas::OnPaint )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE)
{
   userScale.first=1;
   userScale.second=1;
   SetScrollRate( 10, 10 );
   SetVirtualSize( 7000, 7000 );
   //this->parent = parent;
   SetBackgroundColour(*wxWHITE);
   //This is for the paint buffer
   SetBackgroundStyle(wxBG_STYLE_CUSTOM);
   activeId = "Default";
   networks[ "Default" ] = new Network( this );
   this->previousId.assign("-1");
   Refresh(true);
}
///////////////////////////////////////////////////////////////////////////////
Canvas::~Canvas()
{
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::PopulateNetworks( std::string xmlNetwork )
{
	//load
	VE_Conductor::XMLDataBufferEngine::instance()->LoadVESData( xmlNetwork );
	
	//get the map count
	std::map< std::string, VE_XML::VE_Model::SystemStrongPtr>::iterator iter;
	std::map< std::string, VE_XML::VE_Model::SystemStrongPtr> systems =
		VE_Conductor::XMLDataBufferEngine::instance()->GetXMLSystemDataMap();

	// iterate through the systems
	for( iter = systems.begin(); iter != systems.end(); iter++ )
	{
		Network* tempNetwork = new Network( this );
		tempNetwork->LoadSystem(iter->second, this);
		networks[iter->first] = tempNetwork;
	}
	this->SetActiveNetwork( VE_Conductor::XMLDataBufferEngine::instance()->
		GetTopSystemId() );
    Refresh(true);
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::OnPaint(wxPaintEvent& paintEvent)
{
    wxAutoBufferedPaintDC dc(this);
    dc.Clear();

    dc.SetUserScale( userScale.first, userScale.second );
    int xpix, ypix;
    GetScrollPixelsPerUnit( &xpix, &ypix );
    int x, y;
    GetViewStart( &x, &y );
    // account for the horz and vert scrollbar offset
    dc.SetDeviceOrigin( -x * xpix, -y * ypix );
    dc.SetFont( GetFont() );
	
	if( !networks.empty() )
	{
		DrawNetwork(dc, this->activeId);
	}
}
///////////////////////////////////////////////////////////////////////////////
Network* Canvas::GetActiveNetwork()
{
    std::map < std::string, Network * >::iterator iter;
    iter = networks.find( activeId );

    if( iter != networks.end() )
	{
        Refresh(true);
		return networks[this->activeId];
	}
    return 0;
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::SetActiveNetwork(std::string id)
{
	this->activeId = id;
	if(this->previousId.compare(this->activeId) != 0)
	{
		if(this->previousId.compare("-1") != 0)
		{
			RemoveEventHandler( networks[this->previousId] );
			networks[this->previousId]->RemoveAllEvents();
		}
		PushEventHandler( networks[this->activeId] );
		networks[this->activeId]->PushAllEvents();
		this->previousId = this->activeId;
		Refresh(true);
	}
}
//////////////////////////////////////////////////////////////////////////////
void Canvas::DrawNetwork(wxDC &dc, std::string id)
{
	networks[id]->DrawNetwork(dc);
}
//////////////////////////////////////////////////////////////////////////////
std::string Canvas::Save( std::string fileName )
{
   /* // Here we wshould loop over all of the following
    std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
    ///Create the system
    VE_XML::VE_Model::SystemStrongPtr tempSystem = 
        new VE_XML::VE_Model::System();
    //Setup the network to populate
    VE_XML::VE_Model::NetworkWeakPtr tempNetwork = 
        new VE_XML::VE_Model::Network();
    tempSystem->AddNetwork( tempNetwork );

    tempNetwork->GetDataValuePair( -1 )->
        SetData( "m_xUserScale", userScale.first );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "m_yUserScale", userScale.second );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "nPixX", static_cast< long int >( numPix.first ) );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "nPixY", static_cast< long int >( numPix.second ) );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "nUnitY", static_cast< long int >( numUnit.second ) );

    //Update the links
    for( size_t i = 0; i < links.size(); ++i )
    {
        VE_XML::VE_Model::Link* xmlLink = tempNetwork->GetLink( -1 );
        xmlLink->GetFromModule()->SetData( modules[ links[i].GetFromModule() ].
            GetClassName(), static_cast< long int >( links[i].GetFromModule() ) );
        xmlLink->GetToModule()->SetData( modules[ links[i].GetToModule() ].
            GetClassName(), static_cast< long int >( links[i].GetToModule() ) );
        *(xmlLink->GetFromPort()) = static_cast< long int >( links[i].GetFromPort() );
        *(xmlLink->GetToPort()) = static_cast< long int >( links[i].GetToPort() );
        xmlLink->SetLinkName( ConvertUnicode( links.at( i ).GetName().c_str() ) );
        xmlLink->SetID( links.at( i ).GetUUID() );

        //Try to store link cons,
        //link cons are (x,y) wxpoint
        //here I store x in one vector and y in the other
        for ( size_t j = 0; j < links[ i ].GetNumberOfPoints(); ++j )
        {
            xmlLink->GetLinkPoint( j )->SetPoint( 
                std::pair< unsigned int, unsigned int >( 
                links[ i ].GetPoint( j )->x, links[ i ].GetPoint( j )->y ) );
        }
    }

    //Update the tags
    for( size_t i = 0; i < tags.size(); ++i )
    {
        tempNetwork->AddTag( tags.at( i ).GetVETagPtr() );
    }
       
    //Models
    for( std::map< int, Module >::iterator iter = modules.begin(); 
         iter!=modules.end(); ++iter )
    {
        iter->second.GetPlugin()->SetID( iter->first );
        //nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        //    iter->second.GetPlugin()->GetVEModel(), "veModel" ) );
        tempSystem->AddModel( new VE_XML::VE_Model::Model( 
            *(iter->second.GetPlugin()->GetVEModel()) ) );
    }
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &(*tempSystem), "veSystem" ) );
    
    //Write out the veUser info for the local user
    VE_XML::UserPtr userInfo = new VE_XML::User();
    userInfo->SetUserId( "User" );
    userInfo->SetControlStatus( VE_XML::User::VEControlStatus( "MASTER" ) );
    VE_XML::StateInfoWeakPtr colorState = new VE_XML::StateInfo();
    ///Load the current preferences from the data buffer
    std::map< std::string, VE_XML::CommandWeakPtr > tempMap = 
    UserPreferencesDataBuffer::instance()->GetCommandMap();
    for( std::map< std::string, VE_XML::CommandWeakPtr >::iterator prefIter = 
        tempMap.begin(); prefIter != tempMap.end(); ++prefIter )
    {
        colorState->AddState( prefIter->second );
    }
    userInfo->SetStateInfo( colorState );
    nodes.push_back( std::pair< VE_XML::XMLObject*, std::string >( 
        &(*userInfo), "User" ) );
        
    VE_XML::XMLReaderWriter netowrkWriter;
    netowrkWriter.UseStandaloneDOMDocumentManager();
    netowrkWriter.WriteXMLDocument( nodes, fileName, "Network" );

    return fileName;*/
	return "temp";
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::New( bool promptClearXplorer )
{
	this->previousId = "-1";
	RemoveEventHandler( networks[this->activeId] );
	networks[this->activeId]->RemoveAllEvents();
	networks.clear();
	Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
wxRect Canvas::GetAppropriateSubDialogSize()
{
    int displayWidth= 0;
    int displayHeight = 0;
    
    ::wxDisplaySize(&displayWidth,&displayHeight);
    /*if( GetDisplayMode() == std::string( "Desktop" ) )
    {
        wxRect bbox = GetRect();
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3.0f * (displayHeight-bbox.GetBottomRight().y)/4.0f );
        return wxRect( xStart, bbox.GetBottomRight().y, width, height );
    }
    else*/
    {
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3*displayHeight/4.0f );
        //int height = lrint( 3.0f * (displayHeight-bbox.GetBottomRight().y)/4.0f );
        return wxRect( xStart, 0, width, height );
    }
}
