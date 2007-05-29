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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/GUIPlugin/Module.h"

#include "VE_Conductor/GUIPlugin/UIPluginBase.h"

#include <wx/window.h>

using namespace VE_Conductor::GUI_Utilities;

////////////////////////////////////////////////
Module::Module( void )
{
   pl_mod = 0;
   canvas = 0;
}
////////////////////////////////////////////////
Module::~Module( void )
{
   if ( pl_mod )
   {
      delete pl_mod;
      pl_mod = 0;
   }
}
////////////////////////////////////////////////
Module::Module( const Module& input )
{
   pl_mod = 0;
   poly = input.poly; 
   cls_name = input.cls_name;
   canvas = input.canvas;
   //links = input.links;
}
////////////////////////////////////////////////
Module& Module::operator= ( const Module& input )
{
   if ( this != &input )
   {
      pl_mod = 0;
      poly = input.poly; 
      cls_name = input.cls_name;
      canvas = input.canvas;
      // we can do this because it is a vector of actual instances
      //links.clear();
      //links = input.links;
   }
   return *this;
}
////////////////////////////////////////////////
void Module::SetWxWindow( wxWindow* window )
{
   canvas = window;
}
/*////////////////////////////////////////////////
Link* Module::GetLink( size_t i )
{
   return &(links.at( i ));
}
////////////////////////////////////////////////
std::vector< Link >* Module::GetLinks( void )
{
   return &(links);
}
////////////////////////////////////////////////
size_t Module::GetNumberOfLinks( void )
{
   return links.size();
}
////////////////////////////////////////////////
void Module::RemoveLink( size_t i )
{
   std::vector< Link >::iterator iter;
   size_t j;
   for ( iter = links.begin(), j = 0; iter != links.end(); iter++, j++)
   {
      if ( j == i )
      {
         links.erase( iter );
         break;
      }
   }
}*/
////////////////////////////////////////////////
std::string Module::GetClassName( void )
{
   return cls_name;
}
////////////////////////////////////////////////
VE_Conductor::GUI_Utilities::Polygon* Module::GetPolygon( void )
{
   return &(poly);
}
////////////////////////////////////////////////
void Module::SetClassName( std::string newClassname )
{
   cls_name = newClassname;
}
////////////////////////////////////////////////
void Module::SetPlugin( UIPluginBase* newPlugin )
{
   pl_mod = newPlugin;
}
////////////////////////////////////////////////
UIPluginBase* Module::GetPlugin( void )
{
   return pl_mod;
}
