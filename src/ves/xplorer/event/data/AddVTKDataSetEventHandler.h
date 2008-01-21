/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef ADD_VTK_DATASET_EVENT_HANDLER_H
#define ADD_VTK_DATASET_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>

#include <ves/xplorer/ModelPtr.h>

#include <string>


namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file AddVTKDataSetEventHandler.h
  AddVTKDataSetEventHandler API
  */
/*!\class AddVTKDataSetEventHandler
 * Class for adding vtk datasets.
 */
class VE_XPLORER_EXPORTS AddVTKDataSetEventHandler: public EventHandler
{
public:
    ///Constructor
    AddVTKDataSetEventHandler();

    ///Copy Constructor
    AddVTKDataSetEventHandler( const AddVTKDataSetEventHandler& rhs );

    ///Destructor
    virtual ~AddVTKDataSetEventHandler();

    ///Equal operator
    AddVTKDataSetEventHandler& operator=( const AddVTKDataSetEventHandler& rhs );

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( ves::open::xml::XMLObject* command );

protected:
    ///Load surface files from a predefined directory
    ///\param directory to load files from
    void LoadSurfaceFiles( std::string precomputedSurfaceDir );
    ///Directory that contians 3d textures
    ///\param directory to load files from
    void Load3DTextureDirectories( std::string dirToLoad );

    ves::xplorer::Model* _activeModel;///<The active cfdModel;
};

}
}
}

#endif// VE_EVENT_HANDLER_H
