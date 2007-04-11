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
* Date modified: $Date: 2007-04-07 17:33:48 -0500 (Sat, 07 Apr 2007) $
* Version:       $Rev: 7285 $
* Author:        $Author$
* Id:            $Id$
* -----------------------------------------------------------------
*
*************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef UPDATE_ID_ON_CHILDREN_VISITOR_H
#define UPDATE_ID_ON_CHILDREN_VISITOR_H
/*!\file UpdateIDOnChildrenVisitor.h
UpdateIDOnChildrenVisitor API
*/

/*!\class VE_SceneGraph::UpdateIDOnChildrenVisitor
*
*/

#include "VE_Installer/include/VEConfig.h"

#include <osg/NodeVisitor>

#include <string>

namespace VE_SceneGraph
{
class DCS;

class VE_SCENEGRAPH_EXPORTS UpdateIDOnChildrenVisitor : public osg::NodeVisitor
{
public:
   ///Constructor
	UpdateIDOnChildrenVisitor( VE_SceneGraph::DCS* node, std::string ID );
   ///Destructor
   virtual ~UpdateIDOnChildrenVisitor();
   ///Apply function that gets called during the traversal
   virtual void apply( osg::PositionAttitudeTransform& node );
private:
   std::string modelGUID;///<GUID to identify the VE-Open model
};
}

#endif //UPDATE_ID_ON_CHILDREN_VISITOR_H
