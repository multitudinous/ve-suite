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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/util/CORBAServiceList.h>
#include "Canvas.h"
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/open/xml/DOMDocumentManager.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include "Network.h"
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/UserPtr.h>


#include <wx/dcbuffer.h>
#include <wx/msgdlg.h>
using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

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
//using namespace VE_Conductor;

BEGIN_EVENT_TABLE(Canvas, wxScrolledWindow)
    EVT_PAINT( Canvas::OnPaint )
	EVT_MENU( UIPluginBase::DEL_MOD, Canvas::OnDelMod )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas(wxWindow* parent, int id)
  :wxScrolledWindow(parent, id, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE),
previousId("-1"),
m_treeView( 0 )
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

    ///Create default network for the user to work with
    CreateDefaultNetwork();
    
	this->parent = parent;

    Refresh(true);
}
///////////////////////////////////////////////////////////////////////////////
Canvas::~Canvas()
{
    //CleanUpNetworks();
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::PopulateNetworks( std::string xmlNetwork, bool clearXplorer )
{
    if( xmlNetwork.empty() )
    {
        std::cout << 
            " Canvas::PopulateNetworks network string is empty" << std::endl;
        return;
    }
    
    //Clean up the old
    New( true );

	//load
	XMLDataBufferEngine::instance()->LoadVESData( xmlNetwork );
	
	//get the map count
	std::map< std::string, model::SystemPtr> systems =
		XMLDataBufferEngine::instance()->GetXMLSystemDataMap();

	// iterate through the systems
	for( std::map< std::string, model::SystemPtr>::iterator 
        iter = systems.begin(); iter != systems.end(); iter++ )
	{
		Network* tempNetwork = new Network( this );
		tempNetwork->LoadSystem(iter->second, this);
		networks[iter->first] = tempNetwork;
	}
	this->SetActiveNetwork( XMLDataBufferEngine::instance()->
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
    //std::cout << activeId << " " << previousId << " " << id << std::endl;

    if( id == activeId )
    {
        return;
    }
    
	this->activeId = id;

    if( this->previousId != "-1" )
    {
        RemoveEventHandler( networks[this->previousId] );
        networks[this->previousId]->RemoveAllEvents();
    }
    
    PushEventHandler( networks[this->activeId] );
    networks[this->activeId]->PushAllEvents();
    this->previousId = this->activeId;
    SetVirtualSize( networks[this->activeId]->GetMaxX(), networks[this->activeId]->GetMaxY() );
    Refresh(true);
}
//////////////////////////////////////////////////////////////////////////////
void Canvas::DrawNetwork(wxDC &dc, std::string id)
{
	networks[id]->DrawNetwork(dc);
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::New( bool promptClearXplorer )
{
    if( this->activeId == "NULL" )
    {
        return;
    }
    
    int answer = wxID_NO;
    if( !promptClearXplorer )
    {
        wxMessageDialog promptDlg( this, 
            _("Do you want to reset Xplorer?"), 
            _("Reset Xplorer Warning"), 
            wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
            wxDefaultPosition);
        answer = promptDlg.ShowModal();
    }

    if( ( answer == wxID_OK ) || ( promptClearXplorer ) )
    {
        for( std::map < std::string, Network* >::iterator iter = 
            networks.begin(); iter != networks.end(); ++iter )
        {
            networks[this->activeId]->ClearXplorer();
        }
    }
   
	RemoveEventHandler( networks[this->activeId] );
    networks[this->activeId]->RemoveAllEvents();
    CleanUpNetworks();

	Refresh( true );
}
////////////////////////////////////////////////////////////////////////////////
wxRect Canvas::GetAppropriateSubDialogSize()
{
    int displayWidth= 0;
    int displayHeight = 0;
    
    ::wxDisplaySize(&displayWidth,&displayHeight);
    wxRect tempRect = m_treeView->GetScreenRect();

    //if( GetDisplayMode() == std::string( "Desktop" ) )
    {
        wxRect bbox = GetRect();
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3.0f * (displayHeight-tempRect.GetTopLeft().y )/4.0f );
        return wxRect( xStart, tempRect.GetTopLeft().y , width, height );
    }
    /*else
    {
        int xStart = lrint( 2.0f*displayWidth/3.0f );
        int width = lrint( displayWidth/3.0f );
        int height = lrint( 3*displayHeight/4.0f );
        //int height = lrint( 3.0f * (displayHeight-bbox.GetBottomRight().y)/4.0f );
        return wxRect( xStart, 0, width, height );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::SetTreeViewWindow( wxWindow* treeView )
{
    m_treeView = treeView;
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::CleanUpNetworks()
{
    for( std::map < std::string, Network* >::iterator iter = networks.begin();
        iter != networks.end(); ++iter )
    {
        //RemoveEventHandler( iter->second );
        //iter->second->RemoveAllEvents();
        delete iter->second;
    }
    networks.clear();
    activeId = "NULL";
    previousId = "-1";
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::CreateDefaultNetwork()
{
    XMLDataBufferEngine::instance()->NewVESData( true );
    ///Initialize tope level network
    model::NetworkWeakPtr tempNetwork = 
        new model::Network();
    XMLDataBufferEngine::instance()->GetXMLSystemDataObject( 
        XMLDataBufferEngine::instance()->GetTopSystemId() )->
        AddNetwork( tempNetwork );
    
    ///Set the default network
    activeId = "NULL";
    networks[ XMLDataBufferEngine::instance()->GetTopSystemId() ] = 
        new Network( this );
    ///Now set it active
    this->SetActiveNetwork( XMLDataBufferEngine::instance()->GetTopSystemId() );
    
    ///Set canvas parameters
    std::pair< long int, long int > numPix;
    numPix.first = 7000;
    numPix.second = 7000;
    std::pair< long int, long int > numUnit;
    numUnit.first = 10;
    numUnit.second = 10;
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
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::Update()
{
    for( std::map < std::string, Network* >::iterator iter = networks.begin();
         iter != networks.end(); ++iter )
    {
        iter->second->Update();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::OnDelMod(wxCommandEvent& event )
{
    int* selMod = static_cast< int* >( event.GetClientData() );   
    ::wxPostEvent( parent, event );
}