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
#ifndef UPDATE_ID_ON_CHILDREN_VISITOR_H
#define UPDATE_ID_ON_CHILDREN_VISITOR_H

/*!\file UpdateIDOnChildrenVisitor.h
*/

/*!\class VE_SceneGraph::UpdateIDOnChildrenVisitor
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

namespace VE_SceneGraph
{
class DCS;
}

// --- OSG Includes --- //
#include <osg/NodeVisitor>

// --- C/C++ Libraries
#include <string>

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS UpdateIDOnChildrenVisitor : public osg::NodeVisitor
{
public:
    ///Constructor
    ///\param node The node to be traversed
    ///\param id The model's GUID
    UpdateIDOnChildrenVisitor( VE_SceneGraph::DCS* node, std::string id );

    ///Destructor
    virtual ~UpdateIDOnChildrenVisitor( void );

    ///Apply function that gets called during the traversal
    ///\param node A child osg::PositionAttitudeTransform w/in the traversed node
    virtual void apply( osg::PositionAttitudeTransform& node );

private:
    std::string modelGUID;///<GUID to identify the VE-Open model

};
}

#endif //UPDATE_ID_ON_CHILDREN_VISITOR_H
