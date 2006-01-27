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
Polygon* Tag::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////
wxRect* Tag::GetBoundingBox( void )
{
   return &(box);
}
