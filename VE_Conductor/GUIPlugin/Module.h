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
#ifndef _VE_CONDUCTOR_GUI_UTILITIES_MODULE_
#define _VE_CONDUCTOR_GUI_UTILITIES_MODULE_
/*!\file Module.h
Module API
*/
/*!\class Module
*
*/
#include <vector>
#include <string>

#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/Polygon.h"
#include "VE_Conductor/Utilities/Link.h"

#include "VE_Installer/include/VEConfig.h"

class wxWindow;
class UIPluginBase;

namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_GUIPLUGINS_EXPORTS Module
{
public:
   ///Constructor
   Module( void );
   ///Destructor
   ~Module( void );
   ///Copy Constructor
   Module( const Module& );
   ///equal operator
   Module& operator= ( const Module& );

   ///Set the wxWindow on which you wish to draw a tag
   ///\param window The window on which the tag will be drawn
   void SetWxWindow( wxWindow* window );
   //Link* GetLink( size_t i );
   //void RemoveLink( size_t i );
   //std::vector< Link >* GetLinks( void );
   //size_t GetNumberOfLinks( void );
   std::string GetClassName( void );
   void SetClassName( std::string newClassname );
   Polygon* GetPolygon( void );
   UIPluginBase* GetPlugin( void );
   void SetPlugin( UIPluginBase* newPlugin );
protected:
	std::string cls_name;

private:
   UIPluginBase* pl_mod;
   Polygon poly; //Poly is the current poly on the canvas
   //std::vector< Link > links; //links connected with me
   wxWindow* canvas;
};
}
}
#endif
