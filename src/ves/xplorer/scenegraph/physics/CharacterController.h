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

namespace osg
{
class MatrixTransform;
}

// --- Bullet Includes --- //
#include <LinearMath/btTransform.h>

class btDynamicsWorld;
class btKinematicCharacterController;
class btPairCachingGhostObject;
class btCollisionObject;

// --- C/C++ Includes --- //
#include <vector>
#include <deque>

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

    ///Advance the character in time
    void Advance( btScalar dt );

    ///
    void Destroy( btDynamicsWorld* dynamicsWorld );

    ///Make the character jump
    void Jump();

    ///Initialize the physics and geometric models for the character controller
    void Initialize( btDynamicsWorld* dynamicsWorld );

    ///Returns if the character controller is active
    bool IsActive();

    ///Reset the character controller
    void Reset( btDynamicsWorld* dynamicsWorld );

    ///
    void FirstPersonMode( bool onOff );

    ///Move the character forward
    void StepForward( bool onOff );

    ///Move the character backward
    void StepBackward( bool onOff );

    ///Bank the character to the left
    void StrafeLeft( bool onOff );

    ///Bank the character to the right
    void StrafeRight( bool onOff );

    ///Rotate the character and/or the camera
    void Rotate( double dx, double dy );

    ///Activate the character controller
    void TurnOn();

    ///Deactivate the character controller
    void TurnOff();

    ///Position the camera relative to the character
    void UpdateCamera();

    ///Zoom the camera in and out from the character position
    void Zoom( bool inOut );

protected:

private:
    ///
    void QuatSlerp(
        btQuaternion& from, btQuaternion& to, double t, btQuaternion& result );

    void SetBufferSizeAndWeights( size_t bufferSize, double weightModifier );

    ///
    bool mActive;

    ///
    bool m1stPersonMode;

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
    bool mFlying;

    ///
    double mCharacterWidth;

    ///
    double mCharacterHeight;

    ///Used to offset the "look at" point from center of the character transform
    double mLookAtOffsetZ;

    ///The distance the camera is from the "look at" point
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
    double mDeltaTurnAngleX;

    ///
    double mDeltaTurnAngleZ;

    ///
    double mTurnSpeed;

    ///
    double mWeightModifier;

    ///
    double mTotalWeight;

    ///
    size_t mBufferSize;

    ///
    std::vector< double > mWeights;

    ///
    std::deque< std::pair< double, double > > mHistoryBuffer;

    ///
    btQuaternion mCameraRotation;

    ///
    btKinematicCharacterController* mCharacter;

    ///
    btPairCachingGhostObject* mGhostObject;

    ///
    osg::ref_ptr< osg::MatrixTransform > mMatrixTransform;

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
