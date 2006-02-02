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
 * File:          $RCSfile: Link.cxx,v $
 * Date modified: $Date: 2006-01-24 23:17:17 -0600 (Tue, 24 Jan 2006) $
 * Version:       $Rev: 3578 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/Link.h"
#include <wx/window.h>

using namespace VE_Conductor::GUI_Utilities;

////////////////////////////////////////////////
Link::Link( void )
{
   Fr_mod = 1000000;
   To_mod = 1000000;
   Fr_port = 1000000;
   To_port = 1000000;

   canvas = 0;
}
////////////////////////////////////////////////
Link::~Link( void )
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Link::Link( const Link& input )
{
   Fr_mod = input.Fr_mod;
   To_mod = input.To_mod;
   Fr_port = input.Fr_port;
   To_port = input.To_port;

   cons = input.cons;
   poly = input.poly;
   canvas = input.canvas;
}
////////////////////////////////////////////////
Link& Link::operator= ( const Link& input )
{
   if ( this != &input )
   {
      Fr_mod = input.Fr_mod;
      To_mod = input.To_mod;
      Fr_port = input.Fr_port;
      To_port = input.To_port;

      cons.clear();
      cons = input.cons;
      poly = input.poly;
      canvas = input.canvas;
   }
   return *this;
}
////////////////////////////////////////////////
void Link::SetWxWindow( wxWindow* window )
{
   canvas = window;
}
////////////////////////////////////////////////
wxPoint* Link::GetPoint( size_t i )
{
   return &(cons.at( i ));
}
////////////////////////////////////////////////
size_t Link::GetNumberOfPoints( void )
{
   return cons.size();
}
////////////////////////////////////////////////
std::vector< wxPoint >* Link::GetPoints( void )
{
   return &(cons);
}
////////////////////////////////////////////////
unsigned int Link::GetFromPort( void )
{
   return Fr_port;
}
////////////////////////////////////////////////
unsigned int Link::GetToPort( void )
{
   return To_port;
}
////////////////////////////////////////////////
unsigned long Link::GetFromModule( void )
{
   return Fr_mod;
}
////////////////////////////////////////////////
unsigned long Link::GetToModule( void )
{
   return To_mod;
}
////////////////////////////////////////////////
Polygon* Link::GetPolygon( void )
{
   return &(poly);
}
/////////////////////////////////////////////////////////
void Link::DrawLinkCon( bool flag )
{
   wxPoint bport[4];
  
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);

  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();

  wxCoord xoff, yoff;
  POLY linkline, temp;
  wxRect bbox;
  POLY ports;  
  int n, i, num;

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(6,0);
  bport[2]=wxPoint(6,6);
  bport[3]=wxPoint(0,6);
  
  if (flag)
    {
      dc.SetBrush(*wxGREEN_BRUSH);
      dc.SetPen(*wxBLACK_PEN);
    }
  else
    {
      dc.SetBrush(*wxWHITE_BRUSH);
      dc.SetPen(*wxWHITE_PEN);
    }
  
  if (m_selLink < 0)
    return;

  n = links[m_selLink]->cons.size()+2;
  linkline.resize(n);

  bbox = modules[links[m_selLink]->Fr_mod].pl_mod->GetBBox();
  
  num= modules[links[m_selLink]->Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[links[m_selLink]->Fr_mod].pl_mod->GetOPorts(ports);

  linkline[0].x = bbox.x+ports[links[m_selLink]->Fr_port].x;
  linkline[0].y = bbox.y+ports[links[m_selLink]->Fr_port].y;

  for (i=0; i<(int)(links[m_selLink]->cons.size()); i++)
    linkline[i+1]=links[m_selLink]->cons[i];

  bbox = modules[links[m_selLink]->To_mod].pl_mod->GetBBox();

  num = modules[links[m_selLink]->To_mod].pl_mod->GetNumIports();
  ports.resize(num);
  modules[links[m_selLink]->To_mod].pl_mod->GetIPorts(ports);
  linkline[n-1].x = bbox.x+ports[links[m_selLink]->To_port].x;
  linkline[n-1].y = bbox.y+ports[links[m_selLink]->To_port].y;

  for (i=0; i<(int)linkline.size(); i++)
    { 
      xoff = linkline[i].x-3;
      yoff = linkline[i].y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }

  dc.SetBrush(old_brush);
  dc.SetPen(old_pen);
}

///////////////////////////////////////
Link::CalcLinkPoly()
{
   wxRect bbox;
   wxPoint pos;
   POLY ports;
   POLY points;
   POLY result;
   int i;
   int num;


  bbox = modules[l.Fr_mod].pl_mod->GetBBox();
  num = modules[l.Fr_mod].pl_mod->GetNumOports();
  ports.resize(num);
  modules[l.Fr_mod].pl_mod->GetOPorts(ports);
   // get initial port
  pos.x = bbox.x+ports[l.Fr_port].x;
  pos.y = bbox.y+ports[l.Fr_port].y;

  points.push_back(pos);
   // Get all the connection in a link
  for (i=0; i<(int)l.cons.size(); i++)
    points.push_back(l.cons[i]);

  bbox = modules[l.To_mod].pl_mod->GetBBox();
  
  num = modules[l.To_mod].pl_mod->GetNumIports();
  ports.resize(num);
  modules[l.To_mod].pl_mod->GetIPorts(ports);
  pos.x = bbox.x+ports[l.To_port].x;
  pos.y = bbox.y+ports[l.To_port].y;

   // get end port
  points.push_back(pos);

   // -3 so that we end up getting a 6 point wide line
  for (i=0; i<(int)points.size(); i++)
    result.push_back(wxPoint(points[i].x, points[i].y-3));

   // +3 so that we end up getting a 6 point wide line
  for (i=(int)points.size()-1; i>=0; i--)
    result.push_back(wxPoint(points[i].x, points[i].y+3));
  
  return result;

}

