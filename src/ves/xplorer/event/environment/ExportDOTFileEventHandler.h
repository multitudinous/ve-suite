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
 * Date modified: $Date: 2007-06-15 11:02:33 -0500 (Fri, 15 Jun 2007) $
 * Version:       $Rev: 8205 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef EXPORT_DOT_FILE_EVENT_HANDLER_H
#define EXPORT_DOT_FILE_EVENT_HANDLER_H
/*!\file ExportDOTfileEventHandler.h
  ExportDOTfileEventHandler API
  */
/*!\class ExportDOTfileEventHandler
 * load a stored scene.
 */

namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdGlobalBase;
}

#include <ves/xplorer/event/EventHandler.h>

#include <ves/VEConfig.h>

namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS ExportDOTFileEventHandler : public VE_EVENTS::EventHandler
{
public:
   ///Constructor
   ExportDOTFileEventHandler();

   ///Copy Constructor
   ExportDOTFileEventHandler(const ExportDOTFileEventHandler& ceh);
   ///Destructor
   virtual ~ExportDOTFileEventHandler();

   ///Equal operator
   ExportDOTFileEventHandler& operator=(const ExportDOTFileEventHandler& rhs);
   ///\param baseObject The cfdGlobalBase object to apply the command to.
   virtual void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject=0); 
   
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void Execute(VE_XML::XMLObject* veXMLObject);
protected:

};
}
#endif// EXPORT_DOT_FILE_EVENT_HANDLER_H
