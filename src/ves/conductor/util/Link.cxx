/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#include <ves/conductor/util/Link.h>
#include <ves/conductor/util/OrbThread.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <ves/conductor/util/ParamsDlg.h>
#include <ves/conductor/util/VarDialog.h>

#include <ves/open/xml/model/Link.h>
#include <ves/open/xml/model/Point.h>
#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/ParameterBlock.h>
#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/StateInfo.h>

#include <wx/window.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dcclient.h>
#include <wx/msgdlg.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>

#include <iostream>

using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( Link, wxEvtHandler )
    EVT_RIGHT_DOWN( Link::OnMRightDown )
    EVT_MENU( LINK_ADD_CON, Link::OnAddLinkCon )
    EVT_MENU( LINK_DEL, Link::OnDelLink )
    EVT_MENU( LINK_DEL_CON, Link::OnDelLinkCon )
    EVT_MENU( LINK_SHOW_CONT, Link::OnShowLinkContent )
    EVT_MENU( LINK_SET_NAME, Link::OnSetLinkName )
    //Aspen Menu
    EVT_MENU( LINK_ASPEN_PLUS_SHOW_NAME, Link::OnShowAspenName )
    EVT_MENU( LINK_ASPEN_PLUS_INPUTS, Link::OnQueryStreamInputs )
    EVT_MENU( LINK_ASPEN_PLUS_RESULTS, Link::OnQueryStreamOutputs )
    EVT_MENU( LINK_ASPEN_DYN_SHOW_NAME, Link::OnShowAspenDynName )
    EVT_MENU( LINK_ASPEN_DYN_ALL_VARS, Link::OnQueryStreamAllVars )
    EVT_UPDATE_UI( LINK_SET_ACTIVE, Link::OnSetActiveLinkID )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
Link::Link( wxScrolledWindow* designCanvas, wxEvtHandler* handler )
        :
        Fr_mod( 1000000 ),
        To_mod( 1000000 ),
        Fr_port( 1000000 ),
        To_port( 1000000 ),
        networkFrame( designCanvas ),
        userScale( 0 )
{
    double a = atan( 3.0 / 10.0 );
    double b = -a;
    sinb = sin( b );
    cosb = cos( b );
    sina = sin( a );
    cosa = cos( a );
    linkName = wxString( "untitled", wxConvUTF8 );
    highlightFlag = false;
    m_veLink = ves::open::xml::model::LinkPtr( new ves::open::xml::model::Link() );
    m_uuid = m_veLink->GetID();
    mPostHandler = handler;
}
////////////////////////////////////////////////////////////////////////////////
Link::~Link( void )
{
    cons.clear();
}
////////////////////////////////////////////////////////////////////////////////
Link::Link( const Link& input )
{
    Fr_mod = input.Fr_mod;
    To_mod = input.To_mod;
    Fr_port = input.Fr_port;
    To_port = input.To_port;
    sinb = input.sinb;
    cosb = input.cosb;
    sina = input.sina;
    cosa = input.cosa;
    if( input.m_veLink )
    {
        m_veLink = ves::open::xml::model::LinkPtr( new ves::open::xml::model::Link( *( input.m_veLink ) ) );
    }
    cons = input.cons;
    poly = input.poly;
    networkFrame = input.networkFrame;
    mPostHandler = input.mPostHandler;
    linkName = input.linkName;
    userScale = input.userScale;
    action_point = input.action_point;
    m_uuid = input.m_uuid;
    highlightFlag = false;
    maxPointX = 0;
    maxPointY = 0;
}
////////////////////////////////////////////////////////////////////////////////
Link& Link::operator= ( const Link& input )
{
    if( this != &input )
    {
        Fr_mod = input.Fr_mod;
        To_mod = input.To_mod;
        Fr_port = input.Fr_port;
        To_port = input.To_port;
        sinb = input.sinb;
        cosb = input.cosb;
        sina = input.sina;
        cosa = input.cosa;
        if( input.m_veLink )
        {
            m_veLink = ves::open::xml::model::LinkPtr( new ves::open::xml::model::Link( *( input.m_veLink ) ) );
        }
        cons.clear();
        cons = input.cons;
        poly = input.poly;
        networkFrame = input.networkFrame;
        mPostHandler = input.mPostHandler;
        linkName = input.linkName;
        userScale = input.userScale;
        action_point = input.action_point;
        m_uuid = input.m_uuid;
        highlightFlag = false;

        maxPointX = input.maxPointX;
        maxPointY = input.maxPointY;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
wxPoint* Link::GetPoint( size_t i )
{
    try
    {
        return &( cons.at( i ) );
    }
    catch ( ... )
    {
        cons.push_back( wxPoint() );
        return &( cons.at( i ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetPoint( wxPoint* pnt )
{
    cons.push_back( wxPoint() );
    cons.back().x = pnt->x;
    cons.back().y = pnt->y;

    if( pnt->x > maxPointX )
    {
        maxPointX = pnt->x;
    }
    if( pnt->y > maxPointY )
    {
        maxPointY = pnt->y;
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t Link::GetNumberOfPoints( void )
{
    return cons.size();
}
////////////////////////////////////////////////////////////////////////////////
std::vector< wxPoint >* Link::GetPoints( void )
{
    return &( cons );
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Link::GetFromPort( void )
{
    return Fr_port;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int Link::GetToPort( void )
{
    return To_port;
}
////////////////////////////////////////////////////////////////////////////////
unsigned long Link::GetFromModule( void )
{
    return Fr_mod;
}
////////////////////////////////////////////////////////////////////////////////
unsigned long Link::GetToModule( void )
{
    return To_mod;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetFromPort( unsigned int input )
{
    Fr_port = input;
    *( m_veLink->GetFromPort() ) = static_cast< long int >( input );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetToPort( unsigned int input )
{
    To_port = input;
    *( m_veLink->GetToPort() ) = static_cast< long int >( input );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetFromModule( unsigned long input )
{
    Fr_mod = input;
    m_veLink->GetFromModule()->SetData( "FromModule",
                                        static_cast< long int >( Fr_mod ) );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetToModule( unsigned long input )
{
    To_mod = input;
    m_veLink->GetToModule()->SetData( "ToModule",
                                      static_cast< long int >( To_mod ) );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetName( wxString name )
{
    linkName = name;
    m_veLink->SetLinkName( ConvertUnicode( name.c_str() ) );
}
////////////////////////////////////////////////////////////////////////////////
wxString Link::GetName()
{
    return linkName;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetType( int type )
{
    linkType = type;
}
////////////////////////////////////////////////////////////////////////////////
int Link::GetType()
{
    return linkType;
}
////////////////////////////////////////////////////////////////////////////////
ves::conductor::util::Polygon* Link::GetPolygon( void )
{
    return &( poly );
}
////////////////////////////////////////////////////////////////////////////////
void Link::DrawLinkCon( wxDC* dc )
{
    wxBrush old_brush = dc->GetBrush();
    wxPen old_pen = dc->GetPen();
    dc->SetBrush( *wxGREEN_BRUSH );
    dc->SetPen( *wxBLACK_PEN );

    wxPoint bport[4];
    bport[ 0 ] = wxPoint( 0, 0 );
    bport[ 1 ] = wxPoint( 6, 0 );
    bport[ 2 ] = wxPoint( 6, 6 );
    bport[ 3 ] = wxPoint( 0, 6 );

    //Draw the connectors for the particular link
    wxCoord xoff;
    wxCoord yoff;
    for( size_t i = 0; i < cons.size(); ++i )
    {
        xoff = cons[ i ].x - 3;
        yoff = cons[ i ].y - 3;

        dc->DrawPolygon( 4, bport, xoff, yoff );
    }

    dc->SetBrush( old_brush );
    dc->SetPen( old_pen );
}
////////////////////////////////////////////////////////////////////////////////
void Link::CalcLinkPoly()
{
    // -3 so that we end up getting a 6 point wide line
    poly.clear();
    for( size_t i = 0; i < cons.size(); i++ )
    {
        int x = cons[i].x;
        int y = cons[i].y - 3;
        poly.SetPoint( wxPoint( x, y ) );
    }

    // +3 so that we end up getting a 6 point wide line
    for( int j = ( cons.size() - 1 ); j >= 0 ; j-- )
    {
        int x = cons[j].x;
        int y = cons[j].y + 3;
        poly.SetPoint( wxPoint( x, y ) );
    }
    /*   std::vector< wxPoint >::iterator iter;
       for(iter = cons.end()-1; iter >= cons.begin(); --iter )
       {
      int x = iter->x;
      int y = iter->y+3;
        poly.SetPoint( wxPoint( iter->x, iter->y+3 ) );
       }
       */
}
////////////////////////////////////////////////////////////////////////////////
void Link::DrawLinkLine( wxDC* dc )
{
    wxBrush old_brush = dc->GetBrush();
    wxPen old_pen = dc->GetPen();

    wxPoint* points = new wxPoint[ cons.size()];

    //std::cout << Fr_mod << " " <<  To_mod << " " << Fr_port << " " <<  To_port <<std::endl;
    //reverse the order of the points
    size_t maxSize = cons.size() - 1;
    for( size_t i = 0; i < cons.size(); i++ )
    {
        points[ i ] = cons[ maxSize - i ];
        //std::cout << j << " " << points[ j ].x << " " <<  points[ j ].y << std::endl;
    }

    if(linkType == 0)
    {
    dc->SetPen( *wxBLACK_PEN );
    dc->SetBrush( *wxWHITE_BRUSH );
    }
    else if(linkType == 1)
    {
    dc->SetPen( *wxRED_PEN );
    dc->SetBrush( *wxWHITE_BRUSH );
    }
    else if(linkType == 2)
    {
    dc->SetPen( *wxGREEN_PEN );
    dc->SetBrush( *wxWHITE_BRUSH );
    }
    else
    {
    dc->SetPen( *wxBLACK_PEN );
    dc->SetBrush( *wxWHITE_BRUSH );
    }

    dc->DrawLines( cons.size(), points );

    //Now draw the arrow head
    if(linkType == 0)
    {
    dc->SetPen( *wxBLACK_PEN );
    dc->SetBrush( *wxBLACK_BRUSH );
    }
    else if(linkType == 1)
    {
    dc->SetPen( *wxCYAN_PEN );
    dc->SetBrush( *wxCYAN_BRUSH );
    }
    else if(linkType == 2)
    {
    dc->SetPen( *wxGREEN_PEN );
    dc->SetBrush( *wxGREEN_BRUSH );
    }
    else
    {
    dc->SetPen( *wxBLACK_PEN );
    dc->SetBrush( *wxBLACK_BRUSH );
    }

    wxPoint arrow[ 3 ];
    arrow[0] = points[0];
    double dist = sqrt( double(( points[1].y - points[0].y ) *
                               ( points[1].y - points[0].y ) + ( points[1].x - points[0].x ) *
                               ( points[1].x - points[0].x ) ) );
    //Make sure we do not divide by zero
    if( dist <= 0.0001f )
    {
        dist = 1;
    }

    arrow[1].x = ( int )( cosa * 12.0 / dist * ( points[1].x - points[0].x ) -
                          sina * 12.0 / dist * ( points[1].y - points[0].y ) + points[0].x );
    arrow[1].y = ( int )( sina * 12.0 / dist * ( points[1].x - points[0].x ) +
                          cosa * 12.0 / dist * ( points[1].y - points[0].y ) + points[0].y );

    arrow[2].x = ( int )( cosb * 12.0 / dist * ( points[1].x - points[0].x ) -
                          sinb * 12.0 / dist * ( points[1].y - points[0].y ) + points[0].x );
    arrow[2].y = ( int )( sinb * 12.0 / dist * ( points[1].x - points[0].x ) +
                          cosb * 12.0 / dist * ( points[1].y - points[0].y ) + points[0].y );

    dc->DrawPolygon( 3, arrow );
    dc->SetPen( old_pen );
    dc->SetBrush( old_brush );
    delete [] points;
}
/////////////////////////////////////////////////////////////////////////////
void Link::DrawName( wxDC* dc )
{
    if(cons.size() != 2)
    {
        wxPoint middle = cons[ cons.size()/2 ];
        //dc->GetTextExtent( linkName, &w, &h );
        dc->DrawText( linkName, middle.x, middle.y );
    }
    else
    {
        wxPoint middle = cons[0] + cons[1];
        dc->DrawText( linkName, middle.x/2, middle.y/2 );
    }
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnShowLinkContent( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    char* linkresult = 0;
    //The to Mod are actually the from module for the data flow
    int mod = GetFromModule();
    int port = GetFromPort();

    if( !ves::conductor::util::CORBAServiceList::instance()->IsConnectedToCE() )
    {
        return;
    }

    try
    {
        //linkresult = exec->GetExportData(mod, port);
    }
    catch ( CORBA::Exception& )
    {
        //ves::conductor::util::CORBAServiceList::instance()->GetMessageLog()->SetMessage( "Maybe Engine is down\n" );
        return;
    }

    if( std::string( linkresult ) != std::string( "" ) )
    {
        /* Package p;
        p.SetSysId("linkresult.xml");
        p.Load( linkresult, strlen(linkresult) );

        // In the new code this would pass in a datavalue pair
        UIDialog* port_dlg = 0;//modules[mod].GetPlugin()->PortData( NULL, &(p.intfs[0]) );

        if(port_dlg != NULL )
        port_dlg->Show();*/
    }
}
///////////////////////////////////////////////////////////////////////////////
void Link::OnSetLinkName( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    wxTextEntryDialog linkNameDlg( networkFrame, 
        _("Set Link Name"), _("Link Name"), GetName(),
        wxOK|wxCANCEL|wxCENTRE, wxDefaultPosition );

    int showLinkDlg = linkNameDlg.ShowModal();
    if( showLinkDlg != wxID_OK )
    {
        return;
    }

    SetName( linkNameDlg.GetValue() );
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnShowAspenName( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    //CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
    //for( int i = 0; i < links.size(); i++)
    //{
    // serviceList->GetMessageLog()->SetMessage( "link:_ " );
    // serviceList->GetMessageLog()->SetMessage( ConvertUnicode( links[i].GetName().c_str() ).c_str() );
    // serviceList->GetMessageLog()->SetMessage( "_\n" );
    //}

    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc = GetName();
    wxMessageDialog( networkFrame, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnQueryStreamInputs( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    std::string compName = ConvertUnicode( GetName().c_str() );
    compName = "Data.Streams." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetPluginName() +".Data.Blocks." + compName;
        compName = "Data.Streams." + parentTraverser->GetPluginName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getStreamInputModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    CORBAServiceList* serviceList = ves::conductor::util::CORBAServiceList::instance();
    std::string nw_str = serviceList->Query( status );
    wxString title( compName.c_str(), wxConvUTF8 );

    ParamsDlg* params = new ParamsDlg( networkFrame );
    params->SetIsBlock( false );
    //params->SetSize( dialogSize );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();

    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();

    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "input" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
    {
        params->AppendList( temp_vector[i].c_str() );
    }
    params->ShowModal();
    params->Destroy();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnQueryStreamOutputs( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    CORBAServiceList* serviceList = ves::conductor::util::CORBAServiceList::instance();
    //serviceList->GetMessageLog()->SetMessage( "QueryOutput" );

    //UIPLUGIN_CHECKID( event )
    std::string compName = ConvertUnicode( GetName().c_str() );
    compName = "Data.Streams." + compName;

    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    //while( parentTraverser != NULL )
    while( parentTraverser->GetParentModel() != NULL )
    {
        //compName = parentTraverser->GetPluginName() +".Data.Blocks." + compName;
        compName = "Data.Streams." + parentTraverser->GetPluginName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getStreamOutputModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    wxString title( compName.c_str(), wxConvUTF8 );

    ParamsDlg* params = new ParamsDlg( networkFrame );
    params->SetIsBlock( false );
    //params->SetSize( dialogSize );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( 0 );
    std::vector< std::string > temp_vector;
    pair->GetData( temp_vector );

    params->SetCompName( compName.c_str() );
    params->SetServiceList( serviceList );
    params->SetDialogType( "output" );
    for( size_t i = 0; i < temp_vector.size(); i++ )
        params->AppendList( temp_vector[i].c_str() );
    params->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnShowAspenDynName( wxCommandEvent& event )
{
    UILINK_CHECKID( event )
    wxString title;
    title << wxT( "Aspen Name" );
    wxString desc = GetName();
    wxMessageDialog( networkFrame, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnQueryStreamAllVars( wxCommandEvent &event )
{
    UILINK_CHECKID( event )
        
    CORBAServiceList* serviceList = ves::conductor::util::CORBAServiceList::instance();
    std::string compName = ConvertUnicode( GetName().c_str() );
    //compName = "Data.Streams." + compName;
   
    //generate hierarchical name if necessary
    ves::open::xml::model::ModelPtr parentTraverser = parentModel.lock();
    while( parentTraverser != NULL )
    {
        compName = parentTraverser->GetPluginName() + "." + compName;
        parentTraverser = parentTraverser->GetParentModel();
    }

    ves::open::xml::CommandPtr returnState( new ves::open::xml::Command() );
    returnState->SetCommandName( "getStreamModuleParamList" );
    ves::open::xml::DataValuePairPtr data( new ves::open::xml::DataValuePair() );
    data->SetData( std::string( "ModuleName" ), compName );
    returnState->AddDataValuePair( data );

    std::vector< std::pair< ves::open::xml::XMLObjectPtr, std::string > > nodes;
    nodes.push_back( std::pair< ves::open::xml::XMLObjectPtr, std::string >( returnState, "vecommand" ) );

    ves::open::xml::XMLReaderWriter commandWriter;
    std::string status = "returnString";
    commandWriter.UseStandaloneDOMDocumentManager();
    commandWriter.WriteXMLDocument( nodes, status, "Command" );

    //Get results
    std::string nw_str = serviceList->Query( status );
    //std::ofstream packet("packet.txt");
    //packet<<nw_str;
    //packet.close();
    wxString title( compName.c_str(), wxConvUTF8 );
    ves::open::xml::XMLReaderWriter networkReader;
    networkReader.UseStandaloneDOMDocumentManager();
    networkReader.ReadFromString();
    networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
    std::vector< ves::open::xml::XMLObjectPtr > objectVector = networkReader.GetLoadedXMLObjects();
    ves::open::xml::CommandPtr cmd = boost::dynamic_pointer_cast<ves::open::xml::Command>( objectVector.at( 0 ) );
    VarDialog* params = new VarDialog( networkFrame );
    params->SetComponentName( wxString( compName.c_str(), wxConvUTF8 ) );
    params->SetServiceList( serviceList );
    int numdvps = cmd->GetNumberOfDataValuePairs();
    for( size_t i = 0; i < numdvps; i++ )
    {
        ves::open::xml::DataValuePairPtr pair = cmd->GetDataValuePair( i );
        std::vector< std::string > temp_vector;
        pair->GetData( temp_vector );
        params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ), wxString( temp_vector[1].c_str(), wxConvUTF8 ),
            wxString( temp_vector[2].c_str(), wxConvUTF8 ), wxString( temp_vector[3].c_str(), wxConvUTF8 ) );
        //params->SetData( wxString( temp_vector[0].c_str(), wxConvUTF8 ) );
    }
    params->UpdateSizes();
    params->ShowModal();
    params->Destroy();
}
/////////////////////////////////////////////////////
void Link::OnDelLinkCon( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    int answer =
        wxMessageBox( _( "Do you really want to delete this link connector?" ),
                      _( "Confirmation" ), wxYES_NO );

    if( answer != wxYES )
    {
        return;
    }

    std::vector<wxPoint>::iterator iter;
    int i;
    for( iter = cons.begin(), i = 0; iter != cons.end(); i++ )
    {
        if( i == m_selLinkCon )
        {
            iter = cons.erase( iter );
            CalcLinkPoly();
            m_selLinkCon = -1;
            break;
        }
        else
        {
            ++iter;
        }
    }

    networkFrame->Refresh( true );

    ///Update the link
    GetLink();
}
/////////////////////////////////////////////////////
void Link::OnDelLink( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    int answer = wxMessageBox( _( "Do you really want to delete this link?" ), _( "Confirmation" ), wxYES_NO );
    if( answer != wxYES )
        return;

    networkFrame->RemoveEventHandler( this );
    networkFrame->Refresh( true );
    //Post this so that network can delete "this" out of the vector of links
    event.SetClientData( &m_uuid );
    ::wxPostEvent( mPostHandler, event );
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnAddLinkCon( wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    ves::conductor::util::Polygon linkline;
    *( linkline.GetPolygon() ) = cons;

    ves::conductor::util::Polygon Near;
    linkline.nearpnt( action_point, Near );

    size_t i;
    for( i = 0; i < linkline.GetNumberOfPoints() - 1; i++ )
    {
        if (( linkline.GetPoint( i )->x <= Near.GetPoint( 0 )->x &&
                linkline.GetPoint( i + 1 )->x >= Near.GetPoint( 0 )->x ) ||
                ( linkline.GetPoint( i )->x >= Near.GetPoint( 0 )->x &&
                  linkline.GetPoint( i + 1 )->x <= Near.GetPoint( 0 )->x )
           )
        {
            break;
        }
    }

    ves::conductor::util::Polygon temp;
    for( size_t j = 0; j < linkline.GetNumberOfPoints(); ++j )
    {
        temp.SetPoint( *( linkline.GetPoint( j ) ) );
        //i can never be the last point
        if( j == i )
        {
            temp.SetPoint( *( Near.GetPoint( 0 ) ) );
        }
    }

    cons = *( temp.GetPolygon() );
    CalcLinkPoly();
    m_selLinkCon = -1;

    networkFrame->Refresh( true );

    ///Update the link
    GetLink();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnMRightDown( wxMouseEvent &event )
{
    // This function opens a plugins dialog when double clicked on the design canvas
    wxClientDC dc( networkFrame );
    networkFrame->DoPrepareDC( dc );
    dc.SetUserScale( userScale->first, userScale->second );
    wxPoint evtpos = event.GetLogicalPosition( dc );
    action_point = evtpos;
    //If this is not the plugin then move on to the next one
    if( !SelectLink( evtpos.x, evtpos.y ) )
    {
        event.Skip();
        return;
    }
    highlightFlag = true;
    networkFrame->Refresh();

    //send the active id so that each plugin knows what to do
    wxUpdateUIEvent setActiveLinkName;
    setActiveLinkName.SetClientData( &m_uuid );
    setActiveLinkName.SetId( LINK_SET_ACTIVE );
    networkFrame->GetEventHandler()->ProcessEvent( setActiveLinkName );

    wxString menuName = linkName + wxString( " Menu", wxConvUTF8 );
    wxMenu the_pop_menu( menuName );

    the_pop_menu.Append( LINK_SHOW_CONT, _( "Show Link Content" ) );
    the_pop_menu.Append( LINK_SET_NAME, _( "Set Link Name" ) );
    the_pop_menu.Append( LINK_ADD_CON, _( "Add Link Connector" ) );
    the_pop_menu.Append( LINK_DEL_CON, _( "Delete Link Connector" ) );
    the_pop_menu.Append( LINK_DEL, _( "Delete Link" ) );

    the_pop_menu.Enable( LINK_ADD_CON, false );
    the_pop_menu.Enable( LINK_DEL_CON, false );
    the_pop_menu.Enable( LINK_DEL, false );

    the_pop_menu.Enable( LINK_SHOW_CONT, false );

    //Aspen Menu
    wxMenu * aspen_plus_menu = new wxMenu();
    aspen_plus_menu->Append( LINK_ASPEN_PLUS_SHOW_NAME, _( "Aspen Name" ) );
    aspen_plus_menu->Enable( LINK_ASPEN_PLUS_SHOW_NAME, true );
    aspen_plus_menu->Append( LINK_ASPEN_PLUS_INPUTS, _( "Inputs" ) );
    aspen_plus_menu->Enable( LINK_ASPEN_PLUS_INPUTS, true );
    aspen_plus_menu->Append( LINK_ASPEN_PLUS_RESULTS, _( "Results" ) );
    aspen_plus_menu->Enable( LINK_ASPEN_PLUS_RESULTS, true );
    the_pop_menu.Append( LINK_MENU,   _( "Aspen Plus" ), aspen_plus_menu, _( "Used in conjunction with Aspen Plus" ) );
    the_pop_menu.Enable( LINK_MENU, false );

    //Aspen Menu
    wxMenu * aspen_dyn_menu = new wxMenu();
    aspen_dyn_menu->Append( LINK_ASPEN_DYN_SHOW_NAME, _( "Aspen Name" ) );
    aspen_dyn_menu->Enable( LINK_ASPEN_DYN_SHOW_NAME, true );
    aspen_dyn_menu->Append( LINK_ASPEN_DYN_ALL_VARS, _( "All Variables" ) );
    aspen_dyn_menu->Enable( LINK_ASPEN_DYN_ALL_VARS, true );
    the_pop_menu.Append( LINK_MENU,   _( "Aspen Dynamics" ), aspen_dyn_menu, _( "Used in conjunction with Aspen Dynamics" ) );
    the_pop_menu.Enable( LINK_MENU, false );

    m_selLinkCon = -1;
    for( size_t i = 0; i < GetPoints()->size(); i++ )
    {
        if( computenorm( evtpos, *( GetPoint( i ) ) ) <= 3 )
        {
            m_selLinkCon = i;
            break;
        }
    }

    the_pop_menu.Enable( LINK_DEL, true );
    //the_pop_menu.Enable(SHOW_LINK_CONT, true);
    if( m_selLinkCon >= 0 )
        the_pop_menu.Enable( LINK_DEL_CON, true );
    else
        the_pop_menu.Enable( LINK_ADD_CON, true );

    the_pop_menu.Enable( LINK_MENU, true );
    //the_pop_menu.SetClientData( &id );
    networkFrame->PopupMenu( &the_pop_menu, event.GetPosition() );
}
////////////////////////////////////////////////////////////////////////////////
bool Link::SelectLink( int x, int y )
{
    wxPoint temp;
    temp.x = x;
    temp.y = y;
    if( GetPolygon()->inside( temp ) )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
double Link::computenorm( wxPoint pt1, wxPoint pt2 )
{
    return sqrt( double(( pt1.x - pt2.x )*( pt1.x - pt2.x ) + ( pt1.y - pt2.y )*( pt1.y - pt2.y ) ) );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetDCScale( std::pair< double, double >* scale )
{
    userScale = scale;
}
////////////////////////////////////////////////////////////////////////////////
bool Link::CheckID()
{
    if( activeUUID == m_uuid )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnSetActiveLinkID( wxUpdateUIEvent& event )
{
    std::string* activeIdTemp = static_cast< std::string* >( event.GetClientData() );
    //std::cout << *activeIdTemp << std::endl;
    activeUUID = *activeIdTemp;
    event.Skip();
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetUUID( std::string uuid )
{
    m_uuid = uuid;
    m_veLink->SetID( m_uuid );
}
////////////////////////////////////////////////////////////////////////////////
std::string Link::GetUUID()
{
    return m_uuid;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetHighlightFlag( bool flag )
{
    highlightFlag = flag;
}
////////////////////////////////////////////////////////////////////////////////
void Link::DrawLink( wxDC* dc )
{
    DrawLinkLine( dc );
    if( highlightFlag )
    {
        DrawLinkCon( dc );
    }
    DrawName( dc );
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetLink( ves::open::xml::model::LinkPtr inputLink )
{
    m_veLink = inputLink;

    SetFromPort( *( m_veLink->GetFromPort() ) );
    SetToPort( *( m_veLink->GetToPort() ) );

    long moduleID;
    m_veLink->GetFromModule()->GetData( moduleID );
    //SetFromModule( moduleID );
    Fr_mod = moduleID;

    m_veLink->GetToModule()->GetData( moduleID );
    //SetToModule( moduleID );
    To_mod = moduleID;

    size_t numberOfPoints = m_veLink->GetNumberOfLinkPoints();
    for( size_t j = 0; j < numberOfPoints; ++j )
    {
        std::pair< unsigned int, unsigned int > rawPoint =
            m_veLink->GetLinkPoint( j )->GetPoint();
        wxPoint point;
        point.x = rawPoint.first;
        point.y = rawPoint.second;
        SetPoint( &point );
        /*if( point.x > maxX )
        { 
            maxX = point.x + 100;
        }

        if( point.y > maxY )
        {
            maxY = point.y + 100;
        }*/
    }
    // Create the polygon for links
    CalcLinkPoly();
    parentModel = m_veLink->GetParentModel();
    SetName( wxString( m_veLink->GetLinkName().c_str(), wxConvUTF8 ) );
    SetType( m_veLink->GetLinkType() );
    SetUUID( m_veLink->GetID() );
}
////////////////////////////////////////////////////////////////////////////////
ves::open::xml::model::LinkPtr Link::GetLink()
{
    //Try to store link cons,
    //link cons are (x,y) wxpoint
    //here I store x in one vector and y in the other
    for( size_t j = 0; j < GetNumberOfPoints(); ++j )
    {
        m_veLink->GetLinkPoint( j )->SetPoint(
            std::pair< unsigned int, unsigned int >(
                GetPoint( j )->x, GetPoint( j )->y ) );
    }
    return m_veLink;
}
