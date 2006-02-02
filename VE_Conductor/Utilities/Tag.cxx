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
 * File:          $RCSfile: Tag.cxx,v $
 * Date modified: $Date: 2006-01-24 23:17:17 -0600 (Tue, 24 Jan 2006) $
 * Version:       $Rev: 3578 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/Tag.h"
#include <wx/window.h>

using namespace VE_Conductor::GUI_Utilities;

////////////////////////////////////////////////
Tag::Tag( void )
{
   canvas = 0;
}
////////////////////////////////////////////////
Tag::~Tag( void )
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Tag::Tag( const Tag& input )
{
   cons[0] = input.cons[0];
   cons[1] = input.cons[1];
   text = input.text;
   box = input.box;
   poly = input.poly;
   canvas = input.canvas;
}
////////////////////////////////////////////////
Tag& Tag::operator= ( const Tag& input )
{
   if ( this != &input )
   {
      cons[0] = input.cons[0];
      cons[1] = input.cons[1];
      text = input.text;
      box = input.box;
      poly = input.poly;
      canvas = input.canvas;
   }
   return *this;
}
////////////////////////////////////////////////
void Tag::SetWxWindow( wxWindow* window )
{
   canvas = window;
}
////////////////////////////////////////////////
wxPoint* Tag::GetConnectorsPoint( size_t i )
{
   return &(cons[i]);
}
////////////////////////////////////////////////
wxString* Tag::GetTagText( void )
{
   return &(text);
}
////////////////////////////////////////////////
VE_Conductor::GUI_Utilities::Polygon* Tag::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////
wxRect* Tag::GetBoundingBox( void )
{
   return &(box);
}

////////////////////////////////////////////////////
void Tag::CalcTagPoly( void )
{
/*   // Create a poly based on a tag
   wxPoint endpos;
   POLY result;

   endpos.x = t.box.x;
   endpos.y = t.box.y+t.box.height/2;
   
   // first point of the extension line
   result.push_back(wxPoint(t.cons[0].x, t.cons[0].y-3));
   // second point of the extension line
   result.push_back(wxPoint(t.cons[1].x, t.cons[1].y-3));
   // first corner of the text box
   result.push_back(wxPoint(endpos.x, endpos.y-3));
   result.push_back(wxPoint(t.box.x, t.box.y));
   result.push_back(wxPoint(t.box.x+t.box.width, t.box.y));
   result.push_back(wxPoint(t.box.x+t.box.width, t.box.y+t.box.height));
   result.push_back(wxPoint(t.box.x, t.box.y+t.box.height));
   // and now back around again
   result.push_back(wxPoint(endpos.x, endpos.y+3));
   result.push_back(wxPoint(t.cons[1].x, t.cons[1].y+3));
   result.push_back(wxPoint(t.cons[0].x, t.cons[0].y+3));

   //return result;  */
}

////////////////////////////////////////////////////////////
void Tag::DrawTagCon( bool flag)
{
/*  int i;
  wxPoint bport[4];
  wxClientDC dc(this);
  wxCoord xoff, yoff;

  bport[0]=wxPoint(0,0);
  bport[1]=wxPoint(10,0);
  bport[2]=wxPoint(10,10);
  bport[3]=wxPoint(0,10);
  
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);

  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();

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
  
  for (i=0; i<2; i++)
    {
      xoff = t.cons[i].x-3;
      yoff = t.cons[i].y-3;
      
      dc.DrawPolygon(4, bport, xoff, yoff);      
    }

  dc.SetBrush(old_brush);
  dc.SetPen(old_pen);
 */
}

/////////////////////////////////////////////////////////////
void Tag::DrawTag( bool flag)
{
/*  
  int w, h;
  wxClientDC dc(this);
  PrepareDC(dc);
  dc.SetUserScale(m_xUserScale, m_yUserScale);
  
  wxBrush old_brush = dc.GetBrush();
  wxPen old_pen = dc.GetPen();
  wxPen mypen(old_pen);
  mypen.SetColour("BLUE");
  //  mypen.SetStyle(wxDOT);
  // mypen.SetStyle(wxLONG_DASH);
  wxPoint points[3];

  dc.GetTextExtent(t->text, &w, &h);
  t->box.width = w;
  t->box.height = h;
  t->CalcTagPoly();
  points[0] = t->cons[0];
  points[1] = t->cons[1];
  points[2] = wxPoint(t->box.x, t->box.y+t->box.height/2);
  
  
  if (!flag)
    {
      dc.SetPen(*wxWHITE_PEN);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
  else
    {
      dc.SetPen(mypen);
      dc.SetBrush(*wxWHITE_BRUSH);
    }
 
  fflush(NULL);
  dc.DrawLines(3, points);
  
  
  dc.DrawRectangle(t->box.x-3, t->box.y-3, t->box.width+6, t->box.height+6);


  fflush(NULL);
  if (flag) dc.DrawText(t->text, t->box.x, t->box.y);

  fflush(NULL);
  dc.SetPen(old_pen);
  dc.SetBrush(old_brush);
  */
}

