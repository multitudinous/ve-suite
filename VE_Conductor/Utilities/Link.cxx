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
#include "VE_Conductor/Utilities/Link.h"
#include <wx/window.h>
#include <wx/brush.h>
#include <wx/pen.h>
#include <wx/dcclient.h>

#include <iostream>

using namespace VE_Conductor::GUI_Utilities;

BEGIN_EVENT_TABLE(Link, wxEvtHandler)
    /*// The following are rightclick menu options
    EVT_MENU(ADD_LINK_CON, Network::OnAddLinkCon)
    EVT_MENU(DEL_LINK, Network::OnDelLink)
    EVT_MENU(DEL_LINK_CON, Network::OnDelLinkCon)
    EVT_MENU(SHOW_LINK_CONT, Network::OnShowLinkContent)
    //Aspen Menu
    EVT_MENU( SHOW_LINK_NAME, Network::OnShowAspenName )
    EVT_MENU( LINK_INPUTS, Network::OnQueryStreamInputs )
    EVT_MENU( LINK_OUTPUTS, Network::OnQueryStreamOutputs )*/
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
Link::Link( wxWindow* designCanvas )
{
    Fr_mod = 1000000;
    To_mod = 1000000;
    Fr_port = 1000000;
    To_port = 1000000;
    //cons.resize( 2 );
    canvas = designCanvas;
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

    cons = input.cons;
    poly = input.poly;
    canvas = input.canvas;
    linkName = input.linkName;
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

        cons.clear();
        cons = input.cons;
        poly = input.poly;
        canvas = input.canvas;
        linkName = input.linkName;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void Link::SetWxWindow( wxWindow* window )
{
   canvas = window;
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
   //wxClientDC dc( canvas );
   //canvas->PrepareDC( dc );
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
   //wxClientDC dc( canvas );
   //canvas->PrepareDC( dc );
   dc.SetUserScale( scale.first, scale.second );

   wxBrush old_brush = dc.GetBrush();
   wxPen old_pen = dc.GetPen();

   wxPoint* points = new wxPoint[ cons.size() ];


   //std::cout << Fr_mod << " " <<  To_mod << " " << Fr_port << " " <<  To_port <<std::endl;
   //reverse the order of the points
   size_t j = 0;
   std::vector< wxPoint >::iterator iter;
   for ( int i = cons.size()-1; i >= 0; i--, j++ )
   {   
      points[ j ] = cons[i];
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
  
  
   double a = atan(3.0/10.0);
   double b = -a;
   double sinb=sin(b); 
   double cosb = cos(b);
   double sina=sin(a); 
   double cosa = cos(a);
   double dist=sqrt(double((points[1].y-points[0].y)*(points[1].y-points[0].y)
		   +(points[1].x-points[0].x)*(points[1].x-points[0].x)));

  arrow[1].x=(int)( cosa*12.0/dist*(points[1].x-points[0].x)
    -sina*12.0/dist*(points[1].y-points[0].y)+points[0].x );
  arrow[1].y=(int)( sina*12.0/dist*(points[1].x-points[0].x)
    +cosa*12.0/dist*(points[1].y-points[0].y)+points[0].y );

  arrow[2].x=(int)( cosb*12.0/dist*(points[1].x-points[0].x)
    -sinb*12.0/dist*(points[1].y-points[0].y)+points[0].x );
  arrow[2].y=(int)( sinb*12.0/dist*(points[1].x-points[0].x)
    +cosb*12.0/dist*(points[1].y-points[0].y)+points[0].y );
  
  dc.DrawPolygon(3, arrow);
  dc.SetPen(old_pen);
  dc.SetBrush(old_brush);
  delete [] points;
}

