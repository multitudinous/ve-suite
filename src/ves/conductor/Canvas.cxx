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
#include <ves/conductor/util/CORBAServiceList.h>
#include <ves/conductor/Canvas.h>
#include <ves/conductor/ConductorLibEnums.h>
#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/User.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/StateInfo.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/UserPreferencesDataBuffer.h>
#include <ves/open/xml/DOMDocumentManager.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/conductor/IconChooser.h>

#include <ves/conductor/Network.h>
#include <ves/open/xml/model/Tag.h>
#include <ves/open/xml/model/TagPtr.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <wx/dcbuffer.h>
#include <wx/msgdlg.h>
#include <wx/defs.h>

#include <boost/version.hpp>
#if BOOST_VERSION >= 103600 
#include <boost/math/special_functions/round.hpp>
#endif

#include <vtkCompositeDataPipeline.h>
#include <vtkAlgorithm.h>

using namespace ves::open::xml;
using namespace ves::conductor;
using namespace ves::conductor::util;

/*#ifdef WIN32
#include <cmath>
// Win32 doesn't seem to have these functions.
// Therefore implement inline versions of these functions here.
//
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
*/
BEGIN_EVENT_TABLE( Canvas, wxScrolledWindow )
    EVT_PAINT( Canvas::OnPaint )
    EVT_MENU( UIPLUGINBASE_DEL_MOD, Canvas::OnDelMod )
    EVT_MENU( UIPLUGINBASE_MAKE_HIER, Canvas::CreateNewSystem )
    EVT_MENU( UIPLUGINBASE_SET_UI_PLUGIN_NAME, Canvas::SetTreeItemName )
    EVT_UPDATE_UI( NETWORK_DELETE_NETWORK, Canvas::OnDelNetwork )
    EVT_CHAR( Canvas::OnZoom )
END_EVENT_TABLE()

///////////////////////////////////////////////////////////////////////////////
Canvas::Canvas( wxWindow* parent, int id )
        : wxScrolledWindow( parent, id, wxDefaultPosition, wxDefaultSize,
                            wxHSCROLL | wxVSCROLL | wxFULL_REPAINT_ON_RESIZE ),
        previousId( "-1" ),
        m_treeView( 0 ),
        mDataBufferEngine( XMLDataBufferEngine::instance() ),
        mUserPrefBuffer( UserPreferencesDataBuffer::instance() ),
        mServiceList( CORBAServiceList::instance() )
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
    
    //This event is the key that causes appframe to load a new ves file
    //This event is sent in OnDelNetwork if everything is empty on the canvas
    //Essentially before any new ves file or data is loaded the canvas is
    //cleared therefore we know that afterwards we are loading a new ves file
    cleanEvent.SetId( CANVAS_UPDATE_NETWORK_DATA );
    
    Refresh( true );

    //vtkCompositeDataPipeline* prototype = vtkCompositeDataPipeline::New();
    //vtkAlgorithm::SetDefaultExecutivePrototype( prototype );
    //prototype->Delete();
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
        RemoveEventHandler( iter->second );
        iter->second->RemoveAllEvents();
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
    mDataBufferEngine->LoadVESData( xmlNetwork );

    //get the map count
    const std::map< std::string, model::SystemPtr> systems =
        mDataBufferEngine->GetXMLSystemDataMap();

    // iterate through the systems
    for( std::map< std::string, model::SystemPtr>::const_iterator
            iter = systems.begin(); iter != systems.end(); iter++ )
    {
        Network* tempNetwork = new Network( this, mServiceList, mDataBufferEngine, mUserPrefBuffer );
        tempNetwork->LoadSystem( iter->second, this );
        networks[iter->first] = tempNetwork;
        tempNetwork->SetNetworkID( iter->first );
    }
    
    SetActiveNetwork( mDataBufferEngine->GetTopSystemId() );

    //Finally tell the canvas to redraw
    Refresh( true );
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::AddSubNetworks( )
{
    //get the map count
    const std::map< std::string, model::SystemPtr> systems =
        mDataBufferEngine->GetXMLSystemDataMap();

    // iterate through the systems
    for( std::map< std::string, model::SystemPtr>::const_iterator
            iter = systems.begin(); iter != systems.end(); iter++ )
    {
        if( networks.find( iter->first ) == networks.end() )
        {
            Network* tempNetwork = new Network( this, mServiceList, mDataBufferEngine, mUserPrefBuffer );
            tempNetwork->LoadSystem( iter->second, this );
            networks[iter->first] = tempNetwork;
            tempNetwork->SetNetworkID( iter->first );
        }
    }

    //Finally tell the canvas to redraw
    Refresh( true );
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::OnPaint( wxPaintEvent& paintEvent )
{
    wxAutoBufferedPaintDC dc( this );
    DoPrepareDC( dc );

    //set scale
    dc.SetUserScale( userScale.first, userScale.second );
    dc.SetFont( GetFont() );
    dc.Clear();

    //initialize
    dc.SetPen( *wxBLACK_PEN );
    dc.SetBrush( *wxWHITE_BRUSH );
    dc.SetBackground( *wxWHITE_BRUSH );
    dc.SetBackgroundMode( wxTRANSPARENT );

    if( activeId != "NULL" )
    {
        DrawNetwork( dc, this->activeId );
    }

    //Set the scale back after using it
    //-necessary for zooming functionality
    dc.SetUserScale( 1.0f, 1.0f );
}
////////////////////////////////////////////////////////////////////////////////
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
////////////////////////////////////////////////////////////////////////////////
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
        static_cast< int >( networks[this->activeId]->GetNetworkSize().first * tempScaleX ),
        static_cast< int >( networks[this->activeId]->GetNetworkSize().second * tempScaleY ) );

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

    //Delete the old network view
    {
        DataValuePairPtr dataValuePair( new DataValuePair( "STRING" ) );
        dataValuePair->SetData( "NETWORK_SYSTEM_VIEW", "DELETE" );
        CommandPtr veCommand( new Command() );
        veCommand->SetCommandName( std::string( "DELETE_NETWORK_SYSTEM_VIEW" ) );
        veCommand->AddDataValuePair( dataValuePair );
        bool connected = mServiceList->SendCommandStringToXplorer( veCommand );
    }
    
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
#if BOOST_VERSION >= 103600 
        int xStart = boost::math::iround( 2.0f * displayWidth / 3.0f );
        int width = boost::math::iround( displayWidth / 3.0f );
        int height = boost::math::iround( 3.0f * ( displayHeight - tempRect.GetTopLeft().y ) / 4.0f );
#else
        int xStart = lrint( 2.0f * displayWidth / 3.0f );
        int width = lrint( displayWidth / 3.0f );
        int height = lrint( 3.0f * ( displayHeight - tempRect.GetTopLeft().y ) / 4.0f );
#endif
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
    mDataBufferEngine->NewVESData( true );
    ///Initialize tope level network
    model::NetworkPtr tempNetwork( new model::Network() );

    mDataBufferEngine->GetXMLSystemDataObject( 
        mDataBufferEngine->GetTopSystemId() )->AddNetwork( tempNetwork );

    ///Set the default network
    activeId = "NULL";
    std::string tempUUID = mDataBufferEngine->GetTopSystemId();
    networks[ tempUUID ] = new Network( this, mServiceList, mDataBufferEngine, mUserPrefBuffer );
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

    ves::open::xml::DataValuePairPtr dvpPtr;
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_xUserScale", 1.0 );
    tempNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "m_yUserScale", 1.0 );
    tempNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixX", static_cast< long int >( numPix.first ) );
    tempNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nPixY", static_cast< long int >( numPix.second ) );
    tempNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitX", static_cast< long int >( numUnit.first ) );
    tempNetwork->AddDataValuePair( dvpPtr );
    dvpPtr = ves::open::xml::DataValuePairPtr( new ves::open::xml::DataValuePair() );
    dvpPtr->SetData( "nUnitY", static_cast< long int >( numUnit.second ) );
    tempNetwork->AddDataValuePair( dvpPtr );
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
///////////////////////////////////////////////////////////////////////////////
void Canvas::CreateNewSystem( wxCommandEvent& event )
{   
    unsigned int id = wxNewId();
    
    std::stringstream ssId;
    ssId << id;
    std::string sId = ssId.str();

    //create conductor graphics
    Network* tempNetwork = new Network( this, mServiceList, mDataBufferEngine, mUserPrefBuffer );
    //tempNetwork->CreateSystem( system, this );
    std::string name = ConvertUnicode( event.GetString().c_str() );
    model::SystemPtr system = mDataBufferEngine->GetXMLSystemDataObject( name );
    tempNetwork->SetSystem( system );
    tempNetwork->CreateSystem( this, id );
    tempNetwork->SetNetworkID( system->GetID() );
    networks[ system->GetID() ] = tempNetwork;

    ///Now set it active
    SetActiveNetwork( system->GetID() );

    unsigned int* parentID = static_cast< unsigned int* >( event.GetClientData() );
    ids.first = *parentID;
    ids.second = id;
    event.SetClientData( &ids );
    //pass event to app frame
    ::wxPostEvent( mainFrame, event );
}
///////////////////////////////////////////////////////////////////////////////
void Canvas::SetTreeItemName( wxCommandEvent& event )
{
    ::wxPostEvent( mainFrame, event );
}
////////////////////////////////////////////////////////////////////////////////
void Canvas::OnDelNetwork( wxUpdateUIEvent& event )
{
    std::string* networkID = 
        static_cast< std::string* >( event.GetClientData() );

    std::map< std::string, Network* >::iterator iter = 
        networks.find( *networkID );

    networks.erase( iter );
    activeId = "NULL";
    previousId = "-1";
    
    if( networks.empty() )
    {
#if wxCHECK_VERSION( 2, 9, 0 )
        mainFrame->GetEventHandler()->AddPendingEvent( cleanEvent );
#else
        mainFrame->AddPendingEvent( cleanEvent );
#endif
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
