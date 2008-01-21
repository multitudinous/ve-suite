/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 * Date modified: $Date: 2007-12-26 14:42:01 -0600 (Wed, 26 Dec 2007) $
 * Version:       $Rev: 10265 $
 * Author:        $Author: biv $
 * Id:            $Id: NURBSNode.h 10265 2007-12-26 20:42:01Z biv $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#ifndef NURBS_NODE_H
#define NURBS_NODE_H

#include <osg/Geode>
namespace osg
{
    class LineSegment;
}
#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSObject.h>
#include <ves/xplorer/scenegraph/nurbs/ControlMeshPointFunctor.h>

/*!\file NURBS.h
  NURBS Object OSG Renderer API
  */
/*!\file NURBS.cxx
  NURBS Object OSG Renderer code
  */
/*!\class ves::xplorer::scenegraph::nurbs::NURBS
 * Class defining the interface between NURBS object and osg::Geometry.
 */

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
namespace nurbs
{
class NURBSControlMesh;
class NURBSDrawable;

class VE_NURBS_EXPORTS NURBS : public osg::Geode
{
public:
    ///Constructor
    NURBS( ves::xplorer::scenegraph::nurbs::NURBSObject* object = 0 );
    ///Copy constructor using CopyOp to manage deep vs shallow copy
    NURBS( const NURBS&,
               const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    META_Node( ves::xplorer::scenegraph::nurbs, NURBS );

    ///Set the ves::xplorer::scenegraph::nurbs::NURBSObject
    ///\param object The ves::xplorer::scenegraph::nurbs::NURBSObject
    void SetNURBSObject( ves::xplorer::scenegraph::nurbs::NURBSObject* object );

    ///Show the triangulated wireframe surface
    ///\param trueFalse turn off/on wireframe
    void ViewWireframe( bool trueFalse );

    ///Reset the selected control point information
    void ReleaseControlPointSelection();

    ///Move the point based on the motion of the mouse
    ///\param currentView The transform down to this node based on the view
    ///\param relativeMotion vector from the mouse
    void MoveSelectedControlPoint( osg::Matrix currentView,
                                   osg::Vec3 relativeMotion );

    ///Check for selected control point
    bool HasSelectedControlPoint();

    ///Select a ControlPoint on the node
    ///\param index The index of the selected ControlPoint
    void SetSelectedControlPoint( unsigned int index);

    ///Get the original surface
    ves::xplorer::scenegraph::nurbs::NURBSObject* GetNURBS();

    ///Equal operator
    ///\param rhs The right hand side of the operator
    NURBS& operator=( const NURBS& rhs );

protected:
    ///Destructor
    virtual ~NURBS();

    bool m_retessellate;///<Update the mesh
    bool m_isSurface;///<Surface flag

    bool m_wireframeView;///<View the wireframe (tessellation)

    int m_selectedControlPointIndex;///<The selected ControlPoint index

    ///Calculate the surface normal at a point
    ///\param index The index of the point to calculate the normal
    osg::Vec3 _calculateSurfaceNormalAtPoint( unsigned int index );

    ves::xplorer::scenegraph::nurbs::NURBSObject* m_nurbsObject;///<The NURBSurface

    osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSControlMesh> m_controlMeshDrawable;///<The control mesh drawable
    osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSDrawable> m_nurbsDrawable;///<The NURBS drawable
    ves::xplorer::scenegraph::nurbs::ControlMeshPointFunctor m_controlPointSelector;

};
}
}
}
}

#endif //NURBS_H

