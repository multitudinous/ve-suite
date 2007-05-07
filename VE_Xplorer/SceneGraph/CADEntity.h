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
#ifndef CAD_ENTITY_H
#define CAD_ENTITY_H

// --- VE-Suite Stuff --- //
#include "VE_Installer/include/VEConfig.h"

//Should not have to include these here
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Utilities/PhysicsMesh.h"

namespace VE_SceneGraph
{
	class DCS;
	class CADEntityHelper;
   class Utilities::PhysicsMesh;
}

// --- OSG Stuff --- //
#ifdef _OSG
#include <osg/ref_ptr>
namespace osg
{
   class Fog;
}
#include <osg/Fog>
#endif

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

// --- Bullet Stuff --- //
class btRigidBody;
class btCollisionShape;

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
	CADEntity( std::string, VE_SceneGraph::DCS*, bool isStream = false );
   ///Constructor that takes a CADEntityHelper and deep copies the osg
   ///node contained in the CADEntityHelper
	CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, 
              VE_SceneGraph::DCS* worldDCS);
   ~CADEntity();

	VE_SceneGraph::DCS* GetDCS();
   VE_SceneGraph::CADEntityHelper* GetNode();
   btRigidBody* GetRigidBody();
   void SetRigidBody( btRigidBody* btRB );

	void SetMass( float m );
	void SetFriction( float f );
	void SetRestitution( float r );

   void SetCollisionShape( std::string type );

   std::string GetFilename();
   bool GetTransparentFlag();
   void SetGeometryFilename( std::string );
   void SetTransparencyFlag( bool );

private:
	VE_SceneGraph::CADEntityHelper* node;
	osg::ref_ptr< VE_SceneGraph::DCS > dcs;
   btRigidBody* rigid_body;
   osg::ref_ptr< VE_SceneGraph::Utilities::PhysicsMesh > physics_mesh;
   btCollisionShape* collision_shape;

	float mass;
	float friction;
	float restitution;

	bool physics;
   bool concave;

   bool _transparencyFlag;

   std::string fileName;
};
}

#endif //CAD_ENTITY_H
