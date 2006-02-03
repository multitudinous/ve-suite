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
 * File:          $RCSfile: Link.h,v $
 * Date modified: $Date: 2006-01-24 23:17:17 -0600 (Tue, 24 Jan 2006) $
 * Version:       $Rev: 3578 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _VE_CONDUCTOR_GUI_UTILITIES_LINK_
#define _VE_CONDUCTOR_GUI_UTILITIES_LINK_
#include <vector>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/Polygon.h"

#include <wx/gdicmn.h>
class wxWindow;

namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS Link
{
public:
   ///Constructor
   Link( wxWindow* designCanvas );
   ///Destructor
   ~Link( void );
   ///Copy Constructor
   Link( const Link& );
   ///equal operator
   Link& operator= ( const Link& );

   ///Set the wxWindow on which you wish to draw a tag
   ///\param window The window on which the tag will be drawn
   void SetWxWindow( wxWindow* window );
   wxPoint* GetPoint( size_t i );
   size_t GetNumberOfPoints( void );
   std::vector< wxPoint >* GetPoints( void );
   unsigned int GetFromPort( void );
   unsigned int GetToPort( void );
   unsigned long GetFromModule( void );
   unsigned long GetToModule( void );

   void SetFromPort( unsigned int );
   void SetToPort( unsigned int );
   void SetFromModule( unsigned long );
   void SetToModule( unsigned long );
   Polygon* GetPolygon( void );

   ///Helper functions
   void DrawLinkCon( bool flag );
   void CalcLinkPoly( void );
   void DrawLink( bool flag );

private:
   unsigned long Fr_mod;
   unsigned long To_mod;
   unsigned int Fr_port;
   unsigned int To_port;

   std::vector< wxPoint > cons; //connectors
   Polygon poly; //Poly is the current poly on the canvas
   wxWindow* canvas;
};
}
}
#endif
