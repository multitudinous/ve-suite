/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#include <ves/conductor/IconChooser.h>

#include "Network.h"
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/TagPtr.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <wx/dcbuffer.h>
#include <wx/msgdlg.h>
#include <wx/defs.h>
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

BEGIN_EVENT_TABLE( Canvas, wxScrolledWindow )
    EVT_PAINT( Canvas::OnPaint )
    EVT_MENU( UIPluginBase::DEL_MOD, Canvas::OnDelMod )
    EVT_UPDATE_UI( Network::DELETE_NETWORK, Canvas::OnDelNetwork )
    EVT_CHAR( Canvas::OnZoom )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas( wxWindow* parent, int id )
        : wxScrolledWindow( parent, id, wxDefaultPosition, wxDefaultSize,
                            wxHSCROLL | wxVSCROLL | wxNO_FULL_REPAINT_ON_RESIZE ),
        previousId( "-1" ),
        m_treeView( 0 )
{
    std::pair< long int, long int > numPix;
    numPix.first = 1;
    numPix.second = 1;
    std::pair< long int, long int > numUnit;
    numUnit.first = 1;
    numUnit.second = 1;
    userScale.first = 1.0;
    userScale.second = 1.0;

    //initalize canvas size to 1x1
    SetVirtualSize( numPix.first, numPix.second );
    //SetCanvasSize( numPix.first, numPix.second );
    
    SetScrollRate( numUnit.first, numUnit.second );
    
    SetBackgroundColour( *wxWHITE );
    
    //This is for the paint buffer
    SetBackgroundStyle( wxBG_STYLE_CUSTOM );

    ///Create default network for the user to work with
    CreateDefaultNetwork();

    this->parent = parent;
    
    cleanEvent.SetId( UPDATE_NETWORK_DATA );
    
    Refresh( true );
}
///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas()
{
}
///////////////////////////////////////////////////////////////////////////////
Canvas::~Canvas()
{
    // Must first remove the plugin event handlers
    // as these cause problems on shutdown on windows for some reason.
    // Various shutdown processes were tried but the only way
    // to get a clean shutdown on windows is to remove the eventhandlers
    // first then manually clean up the memory.
    
    // We do not need to remove any of the children dialogs on destruction 
    // because all the children are destroyed in AppFrame. Please see
    // the AppFrame destructor
    //CleanUpNetworks();

    for( std::map< std::string, Network* >::iterator iter = 
        networks.begin(); iter != networks.end(); ++iter )
    {
        //iter->second->RemoveAllEvents();
        delete iter->second;
    }
    networks.clear();
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::PopulateNetworks( std::string xmlNetwork, bool clearXplorer )
{
    if( xmlNetwork.empty() || ( xmlNetwork == "NULL" ) )
    {
        std::cout <<
            " Canvas::PopulateNetworks network string is empty" << std::endl;
        return;
    }

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
        tempNetwork->LoadSystem( iter->second, this );
        networks[iter->first] = tempNetwork;
        tempNetwork->SetNetworkID( iter->first );
    }
    
    SetActiveNetwork( XMLDataBufferEngine::instance()->GetTopSystemId() );
    Refresh( true );
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::OnPaint( wxPaintEvent& paintEvent )
{
    wxAutoBufferedPaintDC dc( this );
    DoPrepareDC( dc );

    //set scale
    dc.SetUserScale( userScale.first, userScale.second );
    
    //int xpix, ypix;
    //GetScrollPixelsPerUnit( &xpix, &ypix );
    //int x, y;
    //GetViewStart( &x, &y );
    // account for the horz and vert scrollbar offset
    //dc.SetDeviceOrigin( -x * xpix, -y * ypix );
    dc.SetFont( GetFont() );
    dc.Clear();
    
    if( activeId != "NULL" )
    {
        DrawNetwork( dc, this->activeId );
    }

    //Set the scale back after using it
    //-necessary for zooming functionality
    dc.SetUserScale( 1.0f, 1.0f );
}
///////////////////////////////////////////////////////////////////////////////
Network* Canvas::GetActiveNetwork()
{
    std::map < std::string, Network * >::iterator iter;
    iter = networks.find( activeId );

    if( iter != networks.end() )
    {
        Refresh( true );
        return networks[this->activeId];
    }
    return 0;
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::SetActiveNetwork( std::string id )
{
    if( id == activeId )
    {
        return;
    }

    this->activeId = id;

    //update all event handlers
    if( this->previousId != "-1" )
    {
        RemoveEventHandler( networks[this->previousId] );
        networks[this->previousId]->RemoveAllEvents();
        //set scroll position for previous network
        int x, y;
        GetViewStart( &x, &y );
        networks[this->previousId]->SetScrollPosition( x, y );
    }

    PushEventHandler( networks[this->activeId] );
    networks[this->activeId]->PushAllEvents();

    //update the current id
    this->previousId = this->activeId;
    
    double tempScaleX = networks[this->activeId]->GetUserScale()->first;
    double tempScaleY = networks[this->activeId]->GetUserScale()->second;

    //scale the canvas according to the network view
    SetUserScale( tempScaleX, tempScaleY );

    //set the canvas size to the initial size of the network
    SetVirtualSize(
        networks[this->activeId]->GetNetworkSize().first * tempScaleX,
        networks[this->activeId]->GetNetworkSize().second * tempScaleY);

    //scroll to the stored location
    Scroll(
        networks[this->activeId]->GetScrollPosition().first,
        networks[this->activeId]->GetScrollPosition().second );

    //update network
    Refresh( true );
}
///////////////////////////////////////////////////////////////////////////////
std::string Canvas::GetActiveNetworkID( )
{
    return this->activeId;
}
//////////////////////////////////////////////////////////////////////////////
void Canvas::DrawNetwork( wxDC &dc, std::string id )
{
    networks[id]->DrawNetwork( dc );
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
                                   _( "Do you want to reset Xplorer?" ),
                                   _( "Reset Xplorer Warning" ),
                                   wxYES_NO | wxNO_DEFAULT | wxICON_QUESTION,
                                   wxDefaultPosition );
        answer = promptDlg.ShowModal();
    }

    if( ( answer == wxID_OK ) || ( promptClearXplorer ) )
    {
        for( std::map < std::string, Network* >::iterator iter =
                    networks.begin(); iter != networks.end(); ++iter )
        {
            iter->second->ClearXplorer();
        }
    }

    //clear the network system view
    //DataValuePairPtr dataValuePair( new DataValuePair( "UNSIGNED INT" ) );
    //dataValuePair->SetDataName( "Object ID" );
    //dataValuePair->SetDataValue( static_cast< unsigned int >( iter->first ) );
    DataValuePairPtr dataValuePair( new DataValuePair( "STRING" ) );
    dataValuePair->SetData( "NETWORK_SYSTEM_VIEW", "DELETE" );
    CommandPtr veCommand( new Command() );
    veCommand->SetCommandName( std::string( "DELETE_NETWORK_SYSTEM_VIEW" ) );
    veCommand->AddDataValuePair( dataValuePair );
    bool connected = CORBAServiceList::instance()->
        SendCommandStringToXplorer( veCommand );

    CleanUpNetworks();
}
////////////////////////////////////////////////////////////////////////////////
wxRect Canvas::GetAppropriateSubDialogSize()
{
    int displayWidth = 0;
    int displayHeight = 0;

    ::wxDisplaySize( &displayWidth, &displayHeight );
    wxRect tempRect = m_treeView->GetScreenRect();

    //if( GetDisplayMode() == std::string( "Desktop" ) )
    {
        wxRect bbox = GetRect();
        int xStart = lrint( 2.0f * displayWidth / 3.0f );
        int width = lrint( displayWidth / 3.0f );
        int height = lrint( 3.0f * ( displayHeight - tempRect.GetTopLeft().y ) / 4.0f );
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
    //std::cout << this->GetChildren().size() << std::endl;
    RemoveEventHandler( networks[this->activeId] );
    networks[this->activeId]->RemoveAllEvents();        

    size_t numChild = this->GetChildren().size();
    for( size_t i = 0; i < numChild;)
    {
        //std::cout << ConvertUnicode( GetChildren().Item( i )->GetData()->GetName().c_str() ) << std::endl;
        //std::cout << ConvertUnicode( GetChildren().Item( i )->GetData()->GetLabel().c_str() ) << std::endl;
        if( dynamic_cast< wxDialog* >( GetChildren().Item( i )->GetData() ) )
        {
            //GetChildren().Item( i )->GetData()->Destroy();
            GetChildren().Erase( GetChildren().Item( i ) );
            numChild -= 1;
        }
        else
        {
            ++i;
        }
    }
    
    for( std::map < std::string, Network* >::iterator iter = networks.begin();
            iter != networks.end(); ++iter )
    {
        //we have to do this because the canvas is not destroyed when
        //new or open is selected
        iter->second->RemovePluginDialogs();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::CreateDefaultNetwork()
{
    XMLDataBufferEngine::instance()->NewVESData( true );
    ///Initialize tope level network
    model::NetworkPtr tempNetwork( new model::Network() );

    XMLDataBufferEngine::instance()->GetXMLSystemDataObject(
        XMLDataBufferEngine::instance()->GetTopSystemId() )->
    AddNetwork( tempNetwork );

    ///Set the default network
    activeId = "NULL";
    std::string tempUUID = XMLDataBufferEngine::instance()->GetTopSystemId();
    networks[ tempUUID ] = new Network( this );
    networks[ tempUUID ]->SetNetworkID( tempUUID );
    ///Now set it active
    this->SetActiveNetwork( tempUUID );

    ///Set canvas parameters
    std::pair< long int, long int > numPix;
    numPix.first = 1;
    numPix.second = 1;
    std::pair< long int, long int > numUnit;
    numUnit.first = 1;
    numUnit.second = 1;
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "m_xUserScale", 1.0f );
    tempNetwork->GetDataValuePair( -1 )->
        SetData( "m_yUserScale", 1.0f );
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
    for( std::map< std::string, Network* >::iterator iter = networks.begin();
            iter != networks.end(); ++iter )
    {
        iter->second->Update();
    }
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::OnDelMod( wxCommandEvent& event )
{
    ::wxPostEvent( mainFrame, event );
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::OnDelNetwork( wxUpdateUIEvent& event )
{
    std::string* networkID = 
        static_cast< std::string* >( event.GetClientData() );

    //::wxMilliSleep( 100 );

    std::map< std::string, Network* >::iterator iter = 
        networks.find( *networkID );

    networks.erase( iter );
    activeId = "NULL";
    previousId = "-1";

    std::cout << " erasing the network" << std::endl;
    
    if( networks.empty() )
    {
        mainFrame->AddPendingEvent( cleanEvent );    
        
        //Refresh( true );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::SetUserScale(double x, double y)
{
    userScale.first = x;
    userScale.second = y;
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::OnZoom( wxKeyEvent &event )
{
    //pass zooming key presses to appframe
    if( event.GetModifiers() == wxMOD_CONTROL)
    {
        if( event.GetKeyCode() == WXK_UP ||
            event.GetKeyCode() == WXK_DOWN )
        {
             ::wxPostEvent( mainFrame, event );
        }
    }
    else
    {
        event.Skip();
    }
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::SetMainFrame(wxWindow *window)
{
    this->mainFrame = window;
}