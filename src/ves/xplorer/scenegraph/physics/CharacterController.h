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

#include <osgUtil/LineSegmentIntersector>

namespace osg
{
class MatrixTransform;
class Switch;
}

// --- Bullet Includes --- //
#include <LinearMath/btTransform.h>

class btDynamicsWorld;
class btPairCachingGhostObject;
class btCollisionObject;

// --- STL Includes --- //
#include <vector>
#include <deque>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class KinematicCharacterController;

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
    ///\post Call the bullet simulation step function
    ///\note This function MUST be called BEFORE the bullet simulation is stepped
    ///in time so that the new position change requested by the user
    ///is considered in the respective simulation step.
    void Advance( btScalar dt );

    ///Free memory for CharacterController
    void Destroy();

    ///Make the character jump
    void Jump();

    ///Initialize the physics and geometric models for the character controller
    void Initialize();

    ///Returns if the character controller is active
    bool IsActive();

    ///Reset the character controller
    void Reset();

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

    ///
    ///\param onOff
    void SetCameraRotationSLERP( bool onOff );

    ///
    void SetCharacterRotationFromCamera();

    ///Activate the character controller
    void TurnOn();

    ///Deactivate the character controller
    void TurnOff();

    ///Position the camera relative to the character
    ///\pre Call the bullet simulation step function
    ///\note This MUST be called AFTER the bullet simulation has stepped in time
    ///so that the new position information from the simulation can be
    ///set on the camera view matrix.
    void UpdateCamera();

    ///Zoom the camera in and out from the character position
    void Zoom( bool inOut );

protected:

private:
    ///Linearly interpolate the camera's distance from the character
    void CameraDistanceLERP();

    ///Spherically interpolate the camera's rotation about the character
    void CameraRotationSLERP();

    ///Linearly interpolate the camera from the character
    void OccludeDistanceLERP();

    ///Tests if there is an occluder between the camera and character positions
    ///\param eye The eye vector
    ///\param center The character vector
    ///\return 
    void EyeToCenterRayTest( btVector3& eye, btVector3& center );

    ///
    ///\param eye
    ///\param center
    ///\param up
    void LookAt( btVector3& eye, btVector3& center, btVector3& up );

    ///Sets the buffer size & weight modifier to calculate device input damping
    ///\param bufferSize The size of the history buffer
    ///\param weightModifier The value of the weight modifier
    void SetBufferSizeAndWeights(
        unsigned int bufferSize, double weightModifier );

    ///
    ///\return Returns the delta device input for the frame
    std::pair< double, double > UpdateHistoryBuffer();

    ///
    void UpdateCharacterRotation();
    
    ///
    void UpdateCharacterTranslation( btScalar dt );

    ///Tracks the on/off status of the character controller
    bool mActive;

    ///Tracks if the character controller is in 1st person mode
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
    bool mCameraDistanceLERP;

    ///
    bool mCameraRotationSLERP;

    ///
    bool mOccludeDistanceLERP;

    ///
    bool mPreviousOccluder;

    ///
    double mCharacterWidth;

    ///
    double mCharacterHeight;

    ///Used to offset the "look at" point from center of the character transform
    double mLookAtOffsetZ;

    ///The distance the camera is from the "look at" point
    double mCameraDistance;

    ///
    double mOccludeDistance;

    ///
    double mMinCameraDistance;

    ///
    double mMaxCameraDistance;

    ///
    double mDeltaZoom;

    ///
    double mCameraDistanceLERPdt;

    ///
    double mCameraRotationSLERPdt;

    ///
    double mOccludeDistanceLERPdt;

    ///
    double mDeltaCameraDistanceLERP;

    ///
    double mDeltaCameraRotationSLERP;

    ///
    double mDeltaOccludeDistanceLERP;

    ///
    double mFromCameraDistance;

    ///
    double mToCameraDistance;

    ///
    double mFromOccludeDistance;

    ///
    double mToOccludeDistance;

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
    double mFromTurnAngleZ;

    ///
    double mToTurnAngleZ;

    ///
    double mTurnSpeed;

    ///
    double mWeightModifier;

    ///
    double mTotalWeight;

    ///
    unsigned int mBufferSize;

    ///
    std::vector< double > mWeights;

    ///
    std::deque< std::pair< double, double > > mHistoryBuffer;

    ///
    btQuaternion mCameraRotation;

    ///
    btQuaternion mCameraRotationX;

    ///
    btQuaternion mCameraRotationZ;

    ///
    KinematicCharacterController* mCharacter;

    ///
    btPairCachingGhostObject* mGhostObject;

    ///For character animations
    osg::ref_ptr< osg::Switch > mCharacterAnimations;

    ///
    osg::ref_ptr< osg::MatrixTransform > mMatrixTransform;

    ///
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

    ///
    class CharacterTransformCallback : public osg::NodeCallback
    {
    public:
        ///Constructor
        CharacterTransformCallback( btCollisionObject* collisionObject );

        ///Copy Constructor
        CharacterTransformCallback( const CharacterTransformCallback& ctc );
        
        ///Destructor
        virtual ~CharacterTransformCallback();

        ///Override operator
        virtual void operator()( osg::Node* node, osg::NodeVisitor* nv );

    protected:

    private:
        ///
        btCollisionObject* mCollisionObject;

    };

};

} // end scenegraph
} // end xplorer
} // end ves

#endif //CHARACTER_CONTROLLER_H
