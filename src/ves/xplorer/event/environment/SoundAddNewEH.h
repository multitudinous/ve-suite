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
#ifndef SOUND_ADD_NEW_EVENT_HANDLER_H
#define SOUND_ADD_NEW_EVENT_HANDLER_H
/*!\file SoundAddNewEH.h
  SoundAddNewEventHandler API
  */
/*!\class SoundAddNew EventHandler
 * Add a new sound.
 */

#include <VE_Xplorer/XplorerHandlers/EventHandler.h>
namespace VE_Xplorer
{
	class cfdModel;
}
namespace VE_XML
{
   class XMLObject;
}
#include <VE_Installer/include/VEConfig.h>

namespace VE_EVENTS{
	class VE_XPLORER_EXPORTS SoundAddNewEventHandler: public VE_EVENTS::EventHandler{
public:
   ///Constructor
   SoundAddNewEventHandler();

   ///Copy Constructor
   SoundAddNewEventHandler(const SoundAddNewEventHandler& ceh);
   ///Destructor
   virtual ~SoundAddNewEventHandler();

   ///Equal operator
   SoundAddNewEventHandler& operator=(const SoundAddNewEventHandler& rhs);

   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   virtual void Execute(VE_XML::XMLObject* xmlObject);

   ///Set the active model
   ///\param baseObject Active model
   virtual void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject);
   
protected:
	VE_Xplorer::cfdModel* _activeModel;///<The active cfdModel
};
}
#endif//SOUND_ADD_NEW_EVENT_HANDLER_H
