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
 * Date modified: $Date: 2006-07-08 22:04:36 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4905 $
 * Author:        $Author: mccdo $
 * Id:            $Id: EventHandler.h 4905 2006-07-09 03:04:36Z mccdo $
 * -----------------------------------------------------------------
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef GET_RESULTS_EVENT_HANDLER_H
#define GET_RESULTS_EVENT_HANDLER_H
/*!\file GetResultsEventHandler.h
  GetResultsEventHandler API
  */
/*!\class VE_CE::GetResultsEventHandler
 * Inputs for event handling.
 */
#include "VE_CE/UnitWrapper/EventHandler.h"

namespace VE_XML
{
   class XMLObject;
   namespace VE_Model
   {
      class Model;
   }
}
//#include "VE_Installer/include/VEConfig.h"

namespace VE_CE
{
class GetResultsEventHandler : public EventHandler
{
public:
   ///Constructor
   GetResultsEventHandler();

   ///Destructor
   virtual ~GetResultsEventHandler();

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
#endif// GET_RESULTS_EVENT_HANDLER_H
