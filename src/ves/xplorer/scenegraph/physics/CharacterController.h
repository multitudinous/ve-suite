/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef CHARACTER_CONTROLLER_H
#define CHARACTER_CONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>


// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{

}

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

class btDynamicsWorld;
class btRigidBody;
class btCollisionShape;

// --- C/C++ Libraries --- //


namespace ves
{
namespace xplorer
{
namespace scenegraph
{


/*!\file CharacterController.h
 *
 */
/*!\class ves::xplorer::scenegraph::CharacterController
 *
 */
/*!\namespace ves::xplorer::scenegraph
 *
 */
class VE_SCENEGRAPH_EXPORTS CharacterController
{
public:
    ///Constructor
    CharacterController();

    ///Destructor
    ~CharacterController();

    ///
    void Setup(
        btDynamicsWorld* dynamicsWorld,
        btScalar height = 2.0,
        btScalar width = 0.25 );

    ///
    void Destroy( btDynamicsWorld* dynamicsWorld );

    ///
    btRigidBody* GetRigidBody();

    ///
    void PreStep( btDynamicsWorld* dynamicsWorld );

    ///
    void PlayerStep( btScalar dt );

    ///
    bool CanJump() const;

    ///
    void Jump();

    void StepForward();
    void StepBackward();
    void TurnLeft();
    void TurnRight();

    ///
    void UpdateCamera();

    ///
    void UpdateCharacter( btDynamicsWorld* dynamicsWorld, btScalar dt );

    ///
    bool OnGround() const;

protected:

private:
    bool mStepForward;
    bool mStepBackward;
    bool mStrafeLeft;
    bool mStrafeRight;
    bool mTurnLeft;
    bool mTurnRight;
    bool mJump;

    btScalar mTurnAngle;
    btScalar mMaxLinearVelocity;
    btScalar mWalkVelocity;
    btScalar mTurnVelocity;
    btScalar mHalfHeight;

    btScalar mRayLambda[ 2 ];

    btVector3 mRaySource[ 2 ];
    btVector3 mRayTarget[ 2 ];
    btVector3 mRayNormal[ 2 ];

    btRigidBody* mRigidBody;

    btCollisionShape* mShape;
};

} // end scenegraph
} // end xplorer
} // end ves

#endif //CHARACTER_CONTROLLER_H
