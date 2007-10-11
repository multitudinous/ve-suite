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
#ifndef SEED_POINT_BOUNDS_EVENT_HANDLER_H
#define SEED_POINT_BOUNDS_EVENT_HANDLER_H
/*!\file SPBoundEH.h
  SeedPointBoundsEventHandler API
  */
/*!\class SeedPointBoundsEventHandler
 * Update the SeedPoints Bounding box.
 */

namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdModel;
}
#include <VE_Xplorer/XplorerHandlers/EventHandler.h>
#include <ves/VEConfig.h>

namespace VE_EVENTS{
class VE_XPLORER_EXPORTS SeedPointBoundsEventHandler : public VE_EVENTS::EventHandler
{
public:
   ///Constructor
   SeedPointBoundsEventHandler();

   ///Copy Constructor
   SeedPointBoundsEventHandler(const SeedPointBoundsEventHandler& ceh);
   ///Destructor
   virtual ~SeedPointBoundsEventHandler();

   ///Equal operator
   SeedPointBoundsEventHandler& operator=(const SeedPointBoundsEventHandler& rhs);
   
   ///Update the bounds.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void Execute(VE_XML::XMLObject* veXMLObject);

   ///Set the active cfdModel
   ///\param model The active cfdModel
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model);
protected:
   VE_Xplorer::cfdModel* _activeModel;///<The active model;
};
}
#endif// TEXTURE_BASED_UPDATE_SCALAR_RANGE_EVENT_HANDLER_H
