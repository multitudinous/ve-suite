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
#ifndef SELECTION_HANDLER_H
#define SELECTION_HANDLER_H
/*!\file SelectionHandler.h
SelectionHandler API
*/
/*!\class VE_XPlorer::SelectionHandler
* 
*/
#include <osg/ref_ptr>

#include "VE_Installer/include/VEConfig.h"

namespace osg
{
   class LineSegment;
}

namespace VE_Xplorer
{
   class VE_XPLORER_EXPORTS SelectionHandler
   {
      public:
         SelectionHandler();
         ~SelectionHandler();

         virtual osg::ref_ptr< osg::LineSegment > CreateLineSegment() = 0;

         void ActivateSelection();
         void DeactivateSelection();
         void Traverse();

      private:
         bool active;
     
   };
}

#endif