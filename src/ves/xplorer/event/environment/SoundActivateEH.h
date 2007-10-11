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
#ifndef SOUND_ACTIVATE_EVENT_HANDLER_H
#define SOUND_ACTIVATE_EVENT_HANDLER_H
/*!\file SoundActivateEH.h
  SoundActivateEventHandler API
  */
/*!\class Sound ActivateEventHandler
 * Turn on/off sound.
 */

#include <ves/xplorer/event/EventHandler.h>
namespace VE_Xplorer
{
	class cfdModel;
}
namespace VE_XML
{
   class XMLObject;
}
#include <ves/VEConfig.h>

namespace VE_EVENTS{
	class VE_XPLORER_EXPORTS SoundActivateEventHandler: public VE_EVENTS::EventHandler{
public:
   ///Constructor
   SoundActivateEventHandler();

   ///Copy Constructor
   SoundActivateEventHandler(const SoundActivateEventHandler& ceh);
   ///Destructor
   virtual ~SoundActivateEventHandler();

   ///Equal operator
   SoundActivateEventHandler& operator=(const SoundActivateEventHandler& rhs);

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
#endif//SOUND_ACTIVATE_EVENT_HANDLER_H
