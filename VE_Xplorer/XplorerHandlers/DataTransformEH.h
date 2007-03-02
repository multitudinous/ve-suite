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
 * Date modified: $Date: 2006-09-29 21:31:32 -0500 (Fri, 29 Sep 2006) $
 * Version:       $Rev: 5598 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_DATA_TRANSFORM_EVENT_HANDLER_H
#define VE_DATA_TRANSFORM_EVENT_HANDLER_H

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
#include "VE_Installer/include/VEConfig.h"

namespace VE_XML
{
   class XMLObject;
}

namespace VE_Xplorer
{
   class cfdModel;
   class cfdGlobalBase;
   class cfdVEBaseClass;
}

namespace VE_EVENTS{
class VE_XPLORER_EXPORTS DataTransformEventHandler: public EventHandler
{
public:
   ///Constructor
   DataTransformEventHandler();

   ///Copy Constructor
   DataTransformEventHandler(const DataTransformEventHandler& rhs);

   ///Destructor
   virtual ~DataTransformEventHandler();

   ///Equal operator
   DataTransformEventHandler& operator=(const DataTransformEventHandler& rhs);

   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute(VE_XML::XMLObject* veXMLObject);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model=0);

protected:
   VE_Xplorer::cfdModel* _activeModel;///<The active cfdModel;

};
}
#endif// VE_EVENT_HANDLER_H
