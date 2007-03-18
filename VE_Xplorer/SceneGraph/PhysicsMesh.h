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
#ifndef PHYSICS_MESH_H
#define PHYSICS_MESH_H
/*!\file PhysicsMesh.h
PhysicsMesh API
*/
/*!\class PhysicsMesh
* 
*/
#include <osg/ref_ptr>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>

class btTriangleMesh;
class btCollisionShape;

class PhysicsMesh : public osg::NodeVisitor
{
public:
	PhysicsMesh( osg::Node* osg_node );
	virtual ~PhysicsMesh();

	virtual void apply( osg::Geode& geode );

	void CreateBBMesh();
	void CreateExactMesh();

	btCollisionShape* GetBBMesh();
	btCollisionShape* GetExactMesh();

private:
	osg::BoundingBox bb;
	btTriangleMesh* triMesh;

	btCollisionShape* collision_shape_bb;
	btCollisionShape* collision_shape_exact;
};

#endif //PHYSICS_MESH_H
