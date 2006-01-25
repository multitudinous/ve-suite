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

using namespace VE_Conducto::GUI_Utilities;

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
Link::~Link( void );
{
   ;//Do nothing since this class doesn't manage any of the pointers memory
}
////////////////////////////////////////////////
Link::Link( const Link& input );
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
Link::Link& operator= ( const Link& input );
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
   return &(cons.at( i ))
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
