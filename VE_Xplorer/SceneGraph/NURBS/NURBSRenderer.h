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
 * Date modified: $Date:  $
 * Version:       $Rev:  $
 * Author:        $Author:  $
 * Id:            $Id:  $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef NURBS_SURFACE_RENDER_H
#define NURBS_SURFACE_RENDER_H

#include <osg/Geode>
#include "VE_Xplorer/SceneGraph/NURBS/Export.h"
#include "VE_Xplorer/SceneGraph/NURBS/NURBSObject.h"

/*!\file NURBSRenderer.h
  NURBS Object OSG Renderer API
  */
/*!\class NURBS::NURBSRenderer
 * Class defining the interface between NURBS object and osg::Geometry.
 */

namespace NURBS
{


class VE_NURBS_EXPORTS NURBSRenderer
{
public:
   ///Constructor
   NURBSRenderer(NURBS::NURBSObject* object);

   ///Destructor
   virtual ~NURBSRenderer();

   ///Set the NURBS::NURBSObject
   ///\param object The NURBS::NURBSObject
   void SetNURBSObject(NURBS::NURBSObject* object);

   ///Show the triangulated wireframe surface
   ///\param trueFalse turn off/on wireframe
   void ViewWireframe(bool trueFalse);

   ///Get the original surface
   NURBS::NURBSObject* GetNURBS();

   ///Get the osg::Geometry for the surface
   osg::Geode* GetTriangulatedSurface();

   ///Get the osg::Geometry for the control mesh
   osg::Geode* GetControlMesh();
protected:
   bool _retessellate;///<Update the mesh

   bool _wireframeView;///<View the wireframe (tessellation)
   
   ///Tessellate the surface
   void _tessellateSurface();

   ///Tessellate the curve
   void _tessellateCurve();
   
   ///Draw the control mesh
   void _updateControlMesh();

   ///Calculate the surface normal at a point
   ///\param index The index of the point to calculate the normal
   osg::Vec3 _calculateSurfaceNormalAtPoint(unsigned int index);

   NURBS::NURBSObject* _nurbsObject;///<The NURBSurface
   osg::ref_ptr<osg::Geode> _triangulatedSurface;///<The triangulated surface
   osg::ref_ptr<osg::Geode> _controlMesh;///<The control mesh

}
;
}
#endif //NURBS_SURFACE_RENDER_H

