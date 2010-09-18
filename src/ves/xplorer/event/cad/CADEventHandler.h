/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef CAD_EVENT_HANDLER_H
#define CAD_EVENT_HANDLER_H

#include <ves/xplorer/event/EventHandler.h>
#include <ves/xplorer/ModelCADHandlerPtr.h>
#include <ves/xplorer/ModelPtr.h>

#include <ves/open/xml/cad/CADNodePtr.h>

#include <string>

namespace ves
{
namespace xplorer
{
namespace event
{
/*!\file CADEventHandler.h
  CADEventHandler API
  */
/*!\class CADEventHandler
 * Base class for CADNode event handling.
 */
class VE_XPLORER_EXPORTS CADEventHandler : public EventHandler
{
public:
    ///Constructor
    CADEventHandler();

    ///Copy Constructor
    CADEventHandler( const CADEventHandler& ceh );

    ///Destructor
    virtual ~CADEventHandler();

    ///Set the cfdModel.
    ///\param model The cfdModel to execute the Command on\n.
    ///Default uses the active cfdModel from ModelHandler\n
    ///Otherwise, the cfdModel passed in is used.
    void SetGlobalBaseObject( ves::xplorer::GlobalBase* model = 0 );

    ///Set the node descriptors from the xml to the SceneNode
    ///\param nodeID The CADNode to update
    ///\param nodeType The type of CADNode
    ///\param descriptorName The name of the descriptor
    ///\param descriptorValue The value of the descriptor
    void SetNodeDescriptors( std::string nodeID,
                             std::string nodeType,
                             std::string descriptorName,
                             std::string descriptorValue,
                             ves::open::xml::cad::CADNodePtr inputNodePtr );
    ///Exectute the event
    ///\param xmlObject The current xmlObject event.
    void Execute( const ves::open::xml::XMLObjectPtr& command );

    ///Equal operator
    CADEventHandler& operator=( const CADEventHandler& rhs );

protected:
    ///The internal operation on the CADNode.
    ///\param veXMLObject The veXMLObject to execute.
    virtual void _operateOnNode( ves::open::xml::XMLObjectPtr veXMLObject ) = 0;

    ///Internal method to add nodes.
    ///\param parentID The ID of the node to add the new node to.
    ///\param node The new CADNode to add to the model
    void _addNodeToNode( std::string parentID, ves::open::xml::cad::CADNodePtr node );

    ///Internal method to extract attribtutes from CADNodes.
    ///\param node CADNode to extra attributes from.
    void _setAttributesOnNode( ves::open::xml::cad::CADNodePtr node );

    ///Internal method to extract transform from CADNodes.
    ///\param node CADNode to extract transform from.
    void _setTransformOnNode( ves::open::xml::cad::CADNodePtr node );

    ves::xplorer::Model* m_activeModel;///<The active cfdModel;
    ves::xplorer::ModelCADHandler* m_cadHandler;///<The ModelCADHandler;
    ves::open::xml::cad::CADNodePtr m_cadNode;///<The CADNode.
};

}
}
}

#endif// VE_EVENT_HANDLER_H
