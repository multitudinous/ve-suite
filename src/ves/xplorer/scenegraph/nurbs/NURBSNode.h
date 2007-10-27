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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NURBS_NODE_H
#define NURBS_NODE_H

#include <osg/Geode>
#include <osg/Node>
#include <osg/Group>
#include <osg/BoundingSphere>

#include <ves/VEConfig.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSObject.h>

/*!\file NURBSNode.h
  NURBS Object OSG Renderer API
  */
/*!\file NURBSNode.cxx
  NURBS Object OSG Renderer code
  */
/*!\class ves::xplorer::scenegraph::nurbs::NURBSNode
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
   class NURBSTessellatedSurface;

///???
class VE_NURBS_EXPORTS NURBSNode : public osg::Group
{
public:
    ///Constructor
    NURBSNode(ves::xplorer::scenegraph::nurbs::NURBSObject* object = 0);
    ///Copy constructor using CopyOp to manage deep vs shallow copy
    NURBSNode( const NURBSNode&, 
        const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY );

    META_Node( ves::xplorer::scenegraph::nurbs, NURBSNode );

protected:
   ///Destructor
   virtual ~NURBSNode();
public:
   ///Set the ves::xplorer::scenegraph::nurbs::NURBSObject
   ///\param object The ves::xplorer::scenegraph::nurbs::NURBSObject
   void SetNURBSObject(ves::xplorer::scenegraph::nurbs::NURBSObject* object);

   ///Show the triangulated wireframe surface
   ///\param trueFalse turn off/on wireframe
   void ViewWireframe(bool trueFalse);

   ///Update the control mesh and the surface
   void UpdateControlMesh();

   ///Turn on/off selection mode
   ///\param trueFalse Flag for on/off of selection
   void SetSelectionStatus(bool trueFalse);

   ///Set the mouse position
   ///\param xPosition The x mouse position
   ///\param yPosition The y mouse position
   void SetMousePosition(float xPosition,float yPosition);

   ///Move a control point
   ///\param dx Change in x position of the control point
   ///\param dy Change in y position of the control point
   ///\param dz Change in z position of the control point
   void MoveSelectedControlPoint(float dx,
                                 float dy,
                                 float dz);
   ///Get the original surface
   ves::xplorer::scenegraph::nurbs::NURBSObject* GetNURBS();

   ///Get the osg::Geometry for the surface
   osg::Geode* GetTriangulatedSurface();

   ///Get the osg::Geometry for the control mesh
   osg::Geode* GetControlMesh();

   ///Get selection status
   bool IsSelecting();

   ///Determine if a control point is currently selected
   bool IsControlPointSelected();

   ///compute the bounding box
   virtual osg::BoundingSphere computeBound()const;
protected:

   bool _isSelecting;///<Selection flag

   bool _retessellate;///<Update the mesh

   bool _wireframeView;///<View the wireframe (tessellation)

   ///Calculate the surface normal at a point
   ///\param index The index of the point to calculate the normal
   osg::Vec3 _calculateSurfaceNormalAtPoint(unsigned int index);

   ves::xplorer::scenegraph::nurbs::NURBSObject* _nurbsObject;///<The NURBSurface
   osg::ref_ptr<osg::Geode> _triangulatedSurfaceGeode;///<The triangulated surface
   osg::ref_ptr<osg::Geode> _controlMeshGeode;///<The control mesh geode

   osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSControlMesh> _controlMeshDrawable;///<The control mesh drawable
   osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSTessellatedSurface> _triangulatedSurfaceDrawable;///<The control mesh drawable

};
}
}
}
}

#endif //NURBS_SURFACE_RENDER_H

