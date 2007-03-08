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
#ifndef VE_PHYSICS_SIMULATOR_H
#define VE_PHYSICS_SIMULATOR_H
/*!\file PhysicsSimulator.h
PhysicsSimulator API
*/
/*!\class VE_SceneGraph::PhysicsSimulator
* 
*/
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

//PhysicsSimulator only supports OpenSceneGraph
#ifdef _OSG
#include <osg/ref_ptr>

#include <gadget/Type/PositionInterface.h>

#include <LinearMath/btTransform.h>
#include <btBulletDynamicsCommon.h>
#include <btBulletCollisionCommon.h>

//C/C++ Libraries
#include <vector>

namespace VE_SceneGraph
{
	class CADEntity;
}

class	btDynamicsWorld;
class btCollisionDispatcher;
class btOverlappingPairCache;
class btSequentialImpulseConstraintSolver;
class btRigidBody;
class btCollisionShape;

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS PhysicsSimulator                //: public vpr::Singleton< PhysicsSimulator >
{
public:
   void ExitPhysics();                                   //Functions as the destructor

   void UpdatePhysics( float dt );
   void ResetScene();

   void ShootBox( const btVector3& destination );

   void SetPhysicsState( bool state );
   bool GetPhysicsState();

   void SetShootSpeed( float speed );

   btRigidBody* CreateRigidBody( float mass, const btTransform& startTransform, btCollisionShape* shape );

   btDynamicsWorld* GetDynamicsWorld();

private:
   PhysicsSimulator();
   ~PhysicsSimulator(){;}                                //Never gets called, don't implement
   vprSingletonHeader( PhysicsSimulator );

   void InitPhysics();

   bool idle;
   float shoot_speed;

   gadget::PositionInterface head;

	std::vector< VE_SceneGraph::CADEntity* > box_vector;

   //Manages physics objects and constraints and implements update of all objects each frame
   //******************************************//
	btDynamicsWorld* dynamics_world;

   btCollisionDispatcher* dispatcher;
   btOverlappingPairCache* broadphase;
   btSequentialImpulseConstraintSolver* solver;
   //******************************************//
};
}

#endif //_OSG

#endif //PHYSICS_SIMULATOR_H

