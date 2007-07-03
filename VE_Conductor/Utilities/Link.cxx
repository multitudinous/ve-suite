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
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Utilities/Link.h"
#include "VE_Conductor/Utilities/OrbThread.h"
#include "VE_Conductor/Utilities/ParamsDlg.h"

#include "VE_Open/XML/Model/Link.h"
#include "VE_Open/XML/Model/Point.h"
#include "VE_Open/XML/Model/Model.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/ParameterBlock.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/StateInfo.h"

#include <wx/window.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dcclient.h>
#include <wx/msgdlg.h>
#include <wx/scrolwin.h>

#include <iostream>

using namespace VE_Conductor::GUI_Utilities;

BEGIN_EVENT_TABLE( Link, wxEvtHandler )
    EVT_RIGHT_DOWN( Link::OnMRightDown )
    EVT_MENU( ADD_LINK_CON, Link::OnAddLinkCon )
    EVT_MENU( DEL_LINK, Link::OnDelLink )
    EVT_MENU( DEL_LINK_CON, Link::OnDelLinkCon )
    EVT_MENU( SHOW_LINK_CONT, Link::OnShowLinkContent )
    //Aspen Menu
    EVT_MENU( SHOW_LINK_NAME, Link::OnShowAspenName )
    EVT_MENU( LINK_INPUTS, Link::OnQueryStreamInputs )
    EVT_MENU( LINK_OUTPUTS, Link::OnQueryStreamOutputs )
    EVT_UPDATE_UI( 887, Link::OnSetActiveLinkID )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
Link::Link( wxScrolledWindow* designCanvas ) 
:
Fr_mod( 1000000 ),
To_mod( 1000000 ),
Fr_port( 1000000 ),
To_port( 1000000 ),
networkFrame( designCanvas ),
userScale( 0 )
{
    double a = atan(3.0/10.0);
    double b = -a;
    sinb = sin(b); 
    cosb = cos(b);
    sina = sin(a); 
    cosa = cos(a);
    linkName = wxString( "Link::Link-noname", wxConvUTF8 );
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
    
    cons = input.cons;
    poly = input.poly;
    networkFrame = input.networkFrame;
    linkName = input.linkName;
    userScale = input.userScale;
    action_point = input.action_point;
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
        
        cons.clear();
        cons = input.cons;
        poly = input.poly;
        networkFrame = input.networkFrame;
        linkName = input.linkName;
        userScale = input.userScale;
        action_point = input.action_point;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
wxPoint* Link::GetPoint( size_t i )
{
   try
   {
      return &(cons.at( i ));
   }
   catch( ... )
   {
      cons.push_back( wxPoint() );
      return &(cons.at( i ));
   }
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetPoint( wxPoint* pnt )
{
   cons.push_back( wxPoint() );
   cons.back().x = pnt->x;
   cons.back().y = pnt->y;
}
////////////////////////////////////////////////////////////////////////////////
size_t Link::GetNumberOfPoints( void )
{
   return cons.size();
}
////////////////////////////////////////////////////////////////////////////////
std::vector< wxPoint >* Link::GetPoints( void )
{
   return &(cons);
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
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetToPort( unsigned int input )
{
   To_port = input;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetFromModule( unsigned long input )
{
   Fr_mod = input;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetToModule( unsigned long input )
{
   To_mod = input;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetName(wxString name)
{
	linkName = name;
}
////////////////////////////////////////////////////////////////////////////////
wxString Link::GetName()
{
	return linkName;
}
////////////////////////////////////////////////////////////////////////////////
VE_Conductor::GUI_Utilities::Polygon* Link::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////////////////////////////////////
void Link::DrawLinkCon( bool flag, std::pair< double, double > scale, wxDC &dc )
{
   //wxClientDC dc( networkFrame );
   //networkFrame->PrepareDC( dc );
   dc.SetUserScale( scale.first , scale.second );

   wxBrush old_brush = dc.GetBrush();
   wxPen old_pen = dc.GetPen();

   if( flag )
   {
      dc.SetBrush( *wxGREEN_BRUSH );
      dc.SetPen( *wxBLACK_PEN );
   }
   else
   {
      dc.SetBrush( *wxWHITE_BRUSH );
      dc.SetPen( *wxWHITE_PEN );
   }
  
   wxPoint bport[4];
   bport[ 0 ] = wxPoint( 0, 0 );
   bport[ 1 ] = wxPoint( 6, 0 );
   bport[ 2 ] = wxPoint( 6, 6 );
   bport[ 3 ] = wxPoint( 0, 6 );
  
   //Draw the connectors for the particular link
   for( size_t i = 0; i < cons.size(); ++i )
   { 
      wxCoord xoff = cons[ i ].x - 3;
      wxCoord yoff = cons[ i ].y - 3;

      dc.DrawPolygon( 4, bport, xoff, yoff );      
   }

   dc.SetBrush( old_brush );
   dc.SetPen( old_pen );
}
////////////////////////////////////////////////////////////////////////////////
void Link::CalcLinkPoly()
{
   // -3 so that we end up getting a 6 point wide line
	poly.clear();
   for( size_t i = 0; i < cons.size(); i++ )
   {
	   int x = cons[i].x;
	   int y = cons[i].y-3;
      poly.SetPoint( wxPoint( cons[i].x, cons[i].y-3 ) );
   }

   // +3 so that we end up getting a 6 point wide line
   for( int j = (cons.size()-1); j >=0 ; j-- )
   {
	   int x = cons[j].x;
	   int y = cons[j].y+3;
	   poly.SetPoint( wxPoint( cons[j].x, cons[j].y+3 ) );
   }
/*   std::vector< wxPoint >::iterator iter;
   for ( iter = cons.end()-1; iter >= cons.begin(); --iter )
   {
		int x = iter->x;
		int y = iter->y+3;
	   poly.SetPoint( wxPoint( iter->x, iter->y+3 ) );
   }
   */
}
////////////////////////////////////////////////////////////////////////////////
void Link::DrawLink( bool flag, wxDC& dc, std::pair< double, double > scale )
{ 
    //wxClientDC dc( networkFrame );
    //networkFrame->PrepareDC( dc );
    dc.SetUserScale( scale.first, scale.second );

    wxBrush old_brush = dc.GetBrush();
    wxPen old_pen = dc.GetPen();

    wxPoint* points = new wxPoint[ cons.size() ];

    //std::cout << Fr_mod << " " <<  To_mod << " " << Fr_port << " " <<  To_port <<std::endl;
    //reverse the order of the points

    size_t maxSize = cons.size() - 1;
    for ( size_t i = 0; i < cons.size(); i++ )
	{   
        points[ i ] = cons[ maxSize - i ];
        //std::cout << j << " " << points[ j ].x << " " <<  points[ j ].y << std::endl;
    }

    if( !flag )
    {
        dc.SetPen( *wxWHITE_PEN );
        dc.SetBrush( *wxWHITE_BRUSH );
    }
    else
    {
        dc.SetPen( *wxBLACK_PEN );
        dc.SetBrush( *wxWHITE_BRUSH );
    }
    dc.DrawLines( cons.size(), points );

    //Now draw the arrow head
    if( !flag )
    {
        dc.SetPen( *wxWHITE_PEN );
        dc.SetBrush( *wxWHITE_BRUSH );
    }
    else
    {
        dc.SetPen( *wxBLACK_PEN );
        dc.SetBrush( *wxBLACK_BRUSH );
    }

    wxPoint arrow[ 3 ];
    arrow[0] = points[0];
    double dist=sqrt( double( (points[1].y-points[0].y)*
        (points[1].y-points[0].y) + (points[1].x-points[0].x)*
        (points[1].x-points[0].x) ) );
    //Make sure we do not divide by zero
    if( dist <= 0.0001f )
    {
        dist = 1;
    }
    
    arrow[1].x = (int)( cosa*12.0/dist * (points[1].x-points[0].x)-
        sina*12.0/dist*(points[1].y-points[0].y)+points[0].x );
    arrow[1].y = (int)( sina*12.0/dist * (points[1].x-points[0].x)+
        cosa*12.0/dist*(points[1].y-points[0].y)+points[0].y );

    arrow[2].x = (int)( cosb*12.0/dist * (points[1].x-points[0].x)-
        sinb*12.0/dist*(points[1].y-points[0].y)+points[0].x );
    arrow[2].y = (int)( sinb*12.0/dist * (points[1].x-points[0].x)+
        cosb*12.0/dist*(points[1].y-points[0].y)+points[0].y );

    dc.DrawPolygon(3, arrow);
    dc.SetPen(old_pen);
    dc.SetBrush(old_brush);
    delete [] points;
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnShowLinkContent(wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    char* linkresult = 0;
    //The to Mod are actually the from module for the data flow
    int mod = GetFromModule();
    int port = GetFromPort();
    
    if ( !VE_Conductor::CORBAServiceList::instance()->IsConnectedToCE() )
    {
        return;
    }
    
    try 
    {
        //linkresult = exec->GetExportData(mod, port);
    }
    catch ( CORBA::Exception& ) 
    {
        VE_Conductor::CORBAServiceList::instance()->GetMessageLog()->SetMessage( "Maybe Engine is down\n");
        return;
    }
    
    if ( std::string(linkresult) !=std::string(""))
    {
        /* Package p;
        p.SetSysId("linkresult.xml");
        p.Load( linkresult, strlen(linkresult) );
        
        // In the new code this would pass in a datavalue pair
        UIDialog* port_dlg = 0;//modules[mod].GetPlugin()->PortData( NULL, &(p.intfs[0]) );
        
        if ( port_dlg != NULL )
        port_dlg->Show();*/
    }
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnShowAspenName(wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    //CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
	//for( int i = 0; i < links.size(); i++)
	//{
	//	serviceList->GetMessageLog()->SetMessage( "link:_ " );
	//	serviceList->GetMessageLog()->SetMessage( ConvertUnicode( links[i].GetName().c_str() ).c_str() );
	//	serviceList->GetMessageLog()->SetMessage( "_\n" );
	//}
        
	wxString title;
	title << wxT("Aspen Name");
	wxString desc( GetName().c_str(), wxConvUTF8);
	wxMessageDialog( networkFrame, desc, title).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnQueryStreamInputs(wxCommandEvent& event )
{  
    UILINK_CHECKID( event )

	std::string compName = ConvertUnicode( GetName().c_str() );
    
	VE_XML::Command returnState;
	returnState.SetCommandName("getStreamInputModuleParamList");
	VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
	data->SetData(std::string("ModuleName"), compName);
	
	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
	
	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
	
	//Get results
    CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
	std::string nw_str = serviceList->Query( status );
	wxString title( compName.c_str(),wxConvUTF8);
	
	ParamsDlg* params = new ParamsDlg( networkFrame );
	params->SetIsBlock(false);
    //params->SetSize( dialogSize );
	VE_XML::XMLReaderWriter networkReader;
	networkReader.UseStandaloneDOMDocumentManager();
	networkReader.ReadFromString();
	
	networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
	std::vector< VE_XML::XMLObject* > objectVector = networkReader.GetLoadedXMLObjects();
    
	VE_XML::Command* cmd = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
	VE_XML::DataValuePair * pair = cmd->GetDataValuePair(0);
	std::vector< std::string > temp_vector;
	pair->GetData(temp_vector);
    
	params->SetCompName(compName.c_str());
	params->SetServiceList(serviceList);
	params->SetDialogType("input");
	for (int i=0; i < temp_vector.size(); i++) 
		params->AppendList(temp_vector[i].c_str());
	params->ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnQueryStreamOutputs(wxCommandEvent& event )
{  
    UILINK_CHECKID( event )

	CORBAServiceList* serviceList = VE_Conductor::CORBAServiceList::instance();
	//serviceList->GetMessageLog()->SetMessage( "QueryOutput" );
    
	//UIPLUGIN_CHECKID( event )
	std::string compName = ConvertUnicode( GetName().c_str() );
    
	VE_XML::Command returnState;
	returnState.SetCommandName("getStreamOutputModuleParamList");
	VE_XML::DataValuePair* data = returnState.GetDataValuePair(-1);
	data->SetData(std::string("ModuleName"), compName);
	
	std::vector< std::pair< VE_XML::XMLObject*, std::string > > nodes;
	nodes.push_back(std::pair< VE_XML::XMLObject*, std::string >( &returnState, "vecommand" ));
	
	VE_XML::XMLReaderWriter commandWriter;
	std::string status="returnString";
	commandWriter.UseStandaloneDOMDocumentManager();
	commandWriter.WriteXMLDocument( nodes, status, "Command" );
	
	//Get results
	std::string nw_str = serviceList->Query( status );
	wxString title( compName.c_str(),wxConvUTF8);
    
	ParamsDlg * params = new ParamsDlg( networkFrame );
	params->SetIsBlock(false);
    //params->SetSize( dialogSize );
	VE_XML::XMLReaderWriter networkReader;
	networkReader.UseStandaloneDOMDocumentManager();
	networkReader.ReadFromString();
	networkReader.ReadXMLData( nw_str, "Command", "vecommand" );
	std::vector< VE_XML::XMLObject* > objectVector = networkReader.GetLoadedXMLObjects();
	VE_XML::Command* cmd = dynamic_cast< VE_XML::Command* >( objectVector.at( 0 ) );
	VE_XML::DataValuePair * pair = cmd->GetDataValuePair(0);
	std::vector< std::string > temp_vector;
	pair->GetData(temp_vector);
    
	params->SetCompName(compName.c_str());
	params->SetServiceList(serviceList);
	params->SetDialogType("output");
	for (int i=0; i < temp_vector.size(); i++) 
		params->AppendList(temp_vector[i].c_str());
	params->ShowModal();
}
/////////////////////////////////////////////////////
void Link::OnDelLinkCon(wxCommandEvent& event )
{
    UILINK_CHECKID( event )

    int answer = 
        wxMessageBox( _("Do you really want to delete this link connector?"), 
        _("Confirmation"), wxYES_NO);
        
    if (answer!=wxYES)
    {
        return;
    }
        
    //links[m_selLink].DrawLinkCon( false, userScale );
                
    std::vector<wxPoint>::iterator iter;
    int i;
    for( iter=GetPoints()->begin(), i=0; iter!=GetPoints()->end(); iter++, i++)
    {    
        if ( i == m_selLinkCon )
        {
            iter = GetPoints()->erase( iter );
            CalcLinkPoly();
            m_selLinkCon=-1;
            break;
        }
        else
        {
            ++iter;
        }
    }
            
    //links[m_selLink].DrawLinkCon( true, userScale );
    
    networkFrame->Refresh(true);
    //Update();
}
/////////////////////////////////////////////////////
void Link::OnDelLink(wxCommandEvent& event )
{
    UILINK_CHECKID( event )
    
    int answer=wxMessageBox(_("Do you really want to delete this link?"), _("Confirmation"), wxYES_NO);
    if (answer!=wxYES)
        return;

    networkFrame->RemoveEventHandler( this );
    networkFrame->Refresh(true);
    //Update();
    event.SetClientData( &linkName );
    ::wxPostEvent( networkFrame, event );
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnAddLinkCon(wxCommandEvent& event )
{  
    UILINK_CHECKID( event )
    
    //links[m_selLink].DrawLink( false, userScale );
    
    //int n = links[m_selLink].GetNumberOfPoints()+2;
    //linkline.resize(n);
    
    //unsigned long fromModuleID = links[m_selLink].GetFromModule();
    //unsigned int fromPort = links[m_selLink].GetFromPort();
    //*(linkline.GetPoint( 0 )) = GetPointForSelectedPlugin( fromModuleID, fromPort, "ouput" );
    
    VE_Conductor::GUI_Utilities::Polygon linkline;
    size_t i;
    for ( i=0; i< GetPoints()->size(); i++ )
        *(linkline.GetPoint( i )) = *(GetPoint( i ));
    
    //unsigned long toModuleID = links[m_selLink].GetToModule();
    //unsigned int toPort = links[m_selLink].GetToPort();
    //*(linkline.GetPoint( n-1 )) = GetPointForSelectedPlugin( toModuleID, toPort, "input" );
    VE_Conductor::GUI_Utilities::Polygon Near;
    linkline.nearpnt( action_point, Near );
    
    //size_t i;
    for ( i=0; i < linkline.GetNumberOfPoints()-1; i++)
        if (  ( linkline.GetPoint( i )->x <= Near.GetPoint( 0 )->x && 
                linkline.GetPoint( i+1 )->x >= Near.GetPoint( 0 )->x ) ||
              ( linkline.GetPoint( i )->x >= Near.GetPoint( 0 )->x && 
                linkline.GetPoint( i+1 )->x <= Near.GetPoint( 0 )->x )
              )
            break;
    
    size_t j;
    VE_Conductor::GUI_Utilities::Polygon temp;
    for ( j=1; j< linkline.GetNumberOfPoints()-1; ++j )
    {
        if ( j-1 == i )
            temp.SetPoint( *(Near.GetPoint( 0 )) );
        temp.SetPoint( *(linkline.GetPoint( j )) );
    }
    
    //Between the first link con and the port, and no cons yet
    if (j==1 && i==0) 
        temp.SetPoint( *(Near.GetPoint( 0 )) );
    
    //between the port and the las con
    if (j==linkline.GetNumberOfPoints()-1 && i==(j-1) && i!=0) 
        temp.SetPoint( *(Near.GetPoint( 0 )) );
    
    *(GetPolygon()) = temp;
    //links[m_selLink].DrawLinkCon( true, userScale );
    //m_selLink = -1;
    m_selLinkCon = -1;
    
    networkFrame->Refresh(true);
    //Update();
    
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
    if ( !SelectLink( evtpos.x, evtpos.y ) )
    {
        event.Skip();
        return;
    }

    //send the active id so that each plugin knows what to do
    wxUpdateUIEvent setActiveLinkName;
    setActiveLinkName.SetClientData( &linkName );
    setActiveLinkName.SetId( 887 );
    networkFrame->GetEventHandler()->ProcessEvent( setActiveLinkName );
    
    wxString menuName = linkName + wxString( " Menu", wxConvUTF8 );
    wxMenu the_pop_menu( menuName );
    
    the_pop_menu.Append(SHOW_LINK_CONT, _("Show Link Content") );
    the_pop_menu.Append(ADD_LINK_CON, _("Add Link Connector") );
    the_pop_menu.Append(DEL_LINK_CON, _("Delete Link Connector") );
    the_pop_menu.Append(DEL_LINK, _("Delete Link") );

    the_pop_menu.Enable(ADD_LINK_CON, false);
    the_pop_menu.Enable(DEL_LINK_CON, false);
    the_pop_menu.Enable(DEL_LINK, false);

    the_pop_menu.Enable(SHOW_LINK_CONT, false);

    //Aspen Menu
    wxMenu * aspen_menu = new wxMenu();
    aspen_menu->Append(SHOW_LINK_NAME, _("Aspen Name") );
    aspen_menu->Enable(SHOW_LINK_NAME, true);
    aspen_menu->Append(LINK_INPUTS, _("Query Inputs") );
    aspen_menu->Enable(LINK_INPUTS, true);
    aspen_menu->Append(LINK_OUTPUTS, _("Query Outputs") );
    aspen_menu->Enable(LINK_OUTPUTS, true);
    the_pop_menu.Append( LINK_MENU,   _("Aspen"), aspen_menu, _("Used in conjunction with Aspen") );
    the_pop_menu.Enable(LINK_MENU, false);

    m_selLinkCon = -1;
    for ( size_t i=0; i< GetPoints()->size(); i++)
    {    
        if( computenorm( evtpos, *(GetPoint( i )) )<=3)
        {
            m_selLinkCon=i;
            break;
        }
    }

    the_pop_menu.Enable(DEL_LINK, true);
    //the_pop_menu.Enable(SHOW_LINK_CONT, true);
    if (m_selLinkCon>=0) 
        the_pop_menu.Enable(DEL_LINK_CON, true);
    else
        the_pop_menu.Enable(ADD_LINK_CON, true);
    
    the_pop_menu.Enable(LINK_MENU, true);
    //the_pop_menu.SetClientData( &id );
    networkFrame->PopupMenu(&the_pop_menu, event.GetPosition());
}
////////////////////////////////////////////////////////////////////////////////
bool Link::SelectLink(int x, int y)
{
    wxPoint temp;
    temp.x = x; 
    temp.y = y;
    if( GetPolygon()->inside( temp ) )
    {
        //draw link connectors
        //links[i].DrawLinkCon( true, userScale ); 
        //m_selLink = i;
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
double Link::computenorm( wxPoint pt1, wxPoint pt2 )
{
    return sqrt(double((pt1.x - pt2.x)*(pt1.x - pt2.x) + (pt1.y - pt2.y)*(pt1.y - pt2.y)));
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetDCScale( std::pair< double, double >* scale )
{
    userScale = scale;
}
////////////////////////////////////////////////////////////////////////////////
bool Link::CheckID()
{
    if( activeName == linkName )
    {
        return true;
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void Link::OnSetActiveLinkID( wxUpdateUIEvent& event )
{
    wxString* activeIdTemp = static_cast< wxString* >( event.GetClientData() );
    //std::cout << *activeIdTemp << std::endl;
    activeName = *activeIdTemp;
    event.Skip();
}
