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
 * Date modified: $Date: 2007-03-23 12:23:48 -0500 (Fri, 23 Mar 2007) $
 * Version:       $Rev: 7205 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: PhysicsMesh.h 7205 2007-03-23 17:23:48Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PHYSICS_MESH_H
#define PHYSICS_MESH_H
/*!\file PhysicsMesh.h
PhysicsMesh API
*/
/*!\class PhysicsMesh
* 
*/
// --- VE-Suite Stuff --- //
#include "VE_Installer/include/VEConfig.h"

// --- OSG Stuff --- //
#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>

// --- Bullet Stuff --- //
class btTriangleMesh;
class btCollisionShape;

namespace VE_SceneGraph
{
namespace Utilities
{
class VE_SCENEGRAPH_UTILS_EXPORTS PhysicsMesh : public osg::NodeVisitor
{
public:
	PhysicsMesh( osg::Node* osg_node );
	virtual ~PhysicsMesh();

	virtual void apply( osg::Geode& geode );

   //A box shape created from the osg::BoundingBox of the mesh shape
	void CreateBoundingBoxShape();
   //A concave static-triangle mesh shape with Bounding Volume Hierarchy optimization
   void CreateStaticConcaveShape();
   //Create a convex hull shape from a triangle mesh, mesh can be concave or convex
   void CreateConvexShape();

   //Return the collision shape
	btCollisionShape* GetCollisionShape();

private:
	osg::BoundingBox bb;

	btTriangleMesh* tri_mesh;
	btCollisionShape* collision_shape;
};
}
}

#endif //PHYSICS_MESH_H
