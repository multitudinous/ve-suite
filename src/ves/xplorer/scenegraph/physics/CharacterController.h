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
#include <osg/NodeCallback>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

class btDynamicsWorld;
class btKinematicCharacterController;
class btPairCachingGhostObject;
class btCollisionObject;

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
    void Initialize( btDynamicsWorld* dynamicsWorld );

    ///
    void Destroy( btDynamicsWorld* dynamicsWorld );

    ///
    void StepForward( bool onOff );

    ///
    void StepBackward( bool onOff );

    ///
    void StrafeLeft( bool onOff );

    ///
    void StrafeRight( bool onOff );

    ///
    void Turn( double dx, double dy );
    
    ///
    void Jump();

    ///
    void Reset( btDynamicsWorld* dynamicsWorld );

    ///
    void UpdateCamera();

    ///
    void UpdateCharacter( btDynamicsWorld* dynamicsWorld, btScalar dt );

    ///
    void Zoom( bool inOut );

protected:

private:
    ///
    bool mStepForward;

    ///
    bool mStepBackward;

    ///
    bool mStrafeLeft;

    ///
    bool mStrafeRight;

    ///
    bool mJump;

    ///
    double mCameraHeight;

    ///
    double mCameraDistance;

    ///
    double mMinCameraDistance;

    ///
    double mMaxCameraDistance;

    ///
    double mDeltaZoom;

    ///
    double mSpeed;

    ///
    double mMinSpeed;

    ///
    double mMaxSpeed;

    ///
    double mTurnAngleX;

    ///
    double mTurnAngleZ;

    ///
    double mTurnSpeed;

    ///
    btKinematicCharacterController* mCharacter;

    ///
    btPairCachingGhostObject* mGhostObject;

    ///
    class CharacterTransformCallback : public osg::NodeCallback
    {
    public:
        CharacterTransformCallback( btCollisionObject* collisionObject );

        CharacterTransformCallback( const CharacterTransformCallback& ctc );
        
        virtual ~CharacterTransformCallback();

        virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

    protected:

    private:
        btCollisionObject* mCollisionObject;

    };

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //CHARACTER_CONTROLLER_H
