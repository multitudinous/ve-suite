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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef ADD_VTK_DATASET_EVENT_HANDLER_H
#define ADD_VTK_DATASET_EVENT_HANDLER_H
/*!\file AddVTKDataSetEventHandler.h
  AddVTKDataSetEventHandler API
  */
/*!\class AddVTKDataSetEventHandler
 * Class for adding vtk datasets.
 */
#include <string>
namespace VE_XML
{
   class XMLObject;
}
namespace VE_Xplorer
{
   class cfdModel;
   class cfdGlobalBase;
}
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"
namespace VE_EVENTS
{
class VE_XPLORER_EXPORTS AddVTKDataSetEventHandler: public EventHandler
{
public:
   ///Constructor
   AddVTKDataSetEventHandler();

   ///Copy Constructor
   AddVTKDataSetEventHandler(const AddVTKDataSetEventHandler& rhs);

   ///Destructor
   virtual ~AddVTKDataSetEventHandler();

   ///Equal operator
   AddVTKDataSetEventHandler& operator=(const AddVTKDataSetEventHandler& rhs);

   ///Set the cfdModel.
   ///\param model The cfdModel to execute the Command on\n.
   ///Default uses the active cfdModel from cfdModelHandler\n
   ///Otherwise, the cfdModel passed in is used.
   void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* model=0);
   
   ///Exectute the event
   ///\param xmlObject The current xmlObject event.
   void Execute(VE_XML::XMLObject* command); 

protected:
   ///Load surface files from a predefined directory
   ///\param directory to load files from
   void LoadSurfaceFiles( std::string precomputedSurfaceDir );
   ///Directory that contians 3d textures
   ///\param directory to load files from
   void Load3DTextureDirectories( std::string dirToLoad );

   VE_Xplorer::cfdModel* _activeModel;///<The active cfdModel;
};
}
#endif// VE_EVENT_HANDLER_H
