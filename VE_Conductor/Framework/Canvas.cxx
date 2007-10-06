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
#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/System.h"
#include "VE_Open/XML/Model/SystemStrongPtr.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/StateInfo.h"
#include "VE_Conductor/GUIPlugin/XMLDataBufferEngine.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"
#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#include "VE_Conductor/Framework/Network.h"
#include "VE_Open/XML/Model/Tag.h"
#include "VE_Open/XML/Model/TagPtr.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/UserPtr.h"


#include <wx/dcbuffer.h>
#ifdef WIN32
#include <cmath>
/* Win32 doesn't seem to have these functions.
** Therefore implement inline versions of these functions here.
*/
__inline long int
lrint( double flt )
{
    int intgr;
    _asm
    {
        fld flt
        fistp intgr
    };

    return intgr ;
}

__inline long int
lrintf( float flt )
{
    int intgr;
    _asm
    {
        fld flt
        fistp intgr
    };

    return intgr ;
}
#endif
using namespace VE_Conductor;

BEGIN_EVENT_TABLE(Canvas, wxScrolledWindow)
    EVT_PAINT( Canvas::OnPaint )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE),
previousId("-1")
{
    userScale.first = 1;
    userScale.second = 1;
    std::pair< long int, long int > numPix;
    numPix.first = 7000;
    numPix.second = 7000;
    std::pair< long int, long int > numUnit;
    numUnit.first = 10;
    numUnit.second = 10;
    SetScrollRate( numUnit.first, numUnit.second );
    SetVirtualSize( numPix.first, numPix.second );

    SetBackgroundColour(*wxWHITE);
    //This is for the paint buffer
    SetBackgroundStyle(wxBG_STYLE_CUSTOM);

    ///Initialize tope level network
    VE_XML::VE_Model::NetworkWeakPtr tempNetwork = 
        new VE_XML::VE_Model::Network();
    XMLDataBufferEngine::instance()->GetXMLSystemDataObject( 
        XMLDataBufferEngine::instance()->GetTopSystemId() )->
        AddNetwork( tempNetwork );
    
    ///Set the default network
    activeId = XMLDataBufferEngine::instance()->GetTopSystemId();
    networks[ activeId ] = new Network( this );
    ///Now set it active
	this->SetActiveNetwork( VE_Conductor::XMLDataBufferEngine::instance()->
        GetTopSystemId() );

    ///Set canvas parameters
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

    Refresh(true);
}
///////////////////////////////////////////////////////////////////////////////
Canvas::~Canvas()
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::PopulateNetworks( std::string xmlNetwork )
{
    //Clean up the old
    CleanUpNetworks();

	//load
	VE_Conductor::XMLDataBufferEngine::instance()->LoadVESData( xmlNetwork );
	
	//get the map count
	std::map< std::string, VE_XML::VE_Model::SystemStrongPtr> systems =
		VE_Conductor::XMLDataBufferEngine::instance()->GetXMLSystemDataMap();

	// iterate through the systems
	for( std::map< std::string, VE_XML::VE_Model::SystemStrongPtr>::iterator 
        iter = systems.begin(); iter != systems.end(); iter++ )
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
	if( this->previousId != this->activeId )
	{
		if( this->previousId != "-1" )
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
////////////////////////////////////////////////////////////////////////////////
void Canvas::CleanUpNetworks()
{
    for( std::map < std::string, Network* >::iterator iter = networks.begin();
        iter != networks.end(); ++iter )
    {
        RemoveEventHandler( iter->second );
        iter->second->RemoveAllEvents();
        delete iter->second;
    }
    networks.clear();
}
