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
#ifndef STORED_SCENE_LOAD_FILE_H
#define STORED_SCENE_LOAD_FILE_H
/*!\file StoredSceneLoadEH.h
  StoredSceneLoadEventHandler API
  */
/*!\class StoredSceneLoadEventHandler
 * load a stored scene.
 */

namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdGlobalBase;
   class cfdModel;
}

#include <VE_Xplorer/XplorerHandlers/EventHandler.h>

#include <VE_Installer/include/VEConfig.h>

namespace VE_EVENTS{
   class VE_XPLORER_EXPORTS StoredSceneEventHandler : public VE_EVENTS::EventHandler{
public:
   ///Constructor
   StoredSceneEventHandler();

   ///Copy Constructor
   StoredSceneEventHandler(const StoredSceneEventHandler& ceh);
   ///Destructor
   virtual ~StoredSceneEventHandler();

   ///Equal operator
   StoredSceneEventHandler& operator=(const StoredSceneEventHandler& rhs);
   ///\param baseObject The cfdGlobalBase object to apply the command to.
   virtual void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* baseObject=0); 
   
   ///The internal operation on the CADNode.
   ///\param veXMLObject The veXMLObject to execute.
   virtual void Execute(VE_XML::XMLObject* veXMLObject);
protected:

};
}
#endif// QUAT_CAM_LOAD_FILE_H
