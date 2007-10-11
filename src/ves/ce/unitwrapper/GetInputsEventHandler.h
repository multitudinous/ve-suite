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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef GET_INPUTS_EVENT_HANDLER_H
#define GET_INPUTS_EVENT_HANDLER_H
/*!\file GetInputsEventHandler.h
  GetInputsEventHandler API
  */
/*!\class VE_CE::GetInputsEventHandler
 * Inputs for event handling.
 */
#include <ves/ce/unitwrapper/EventHandler.h>

namespace VE_XML
{
   class XMLObject;
   namespace VE_Model
   {
      class Model;
   }
}
#include <ves/VEConfig.h>

namespace VE_CE
{
class VE_CE_UNIT_WRAPPER_EXPORTS GetInputsEventHandler : public EventHandler
{
public:
   ///Constructor
   GetInputsEventHandler();

   ///Destructor
   virtual ~GetInputsEventHandler();

   ///The call to handle the event
   ///\param objectToProcess The xml Object to process
   virtual std::string Execute( std::vector< VE_XML::XMLObject* > objectToProcess );

   ///Function to set the xml object to work on
   ///\param baseObject The base object to apply the command to.
   virtual void SetBaseObject(VE_XML::XMLObject* baseObject );
private:
   VE_XML::VE_Model::Model* baseModel;
};
}
#endif// GET_INPUTS_EVENT_HANDLER_H
