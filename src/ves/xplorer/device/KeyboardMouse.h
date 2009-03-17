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

#ifndef KEYBOARD_MOUSE_H
#define KEYBOARD_MOUSE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

// --- vrJuggler Includes --- //
#include <gmtl/Matrix.h>

#include <boost/shared_ptr.hpp>

#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/PositionInterface.h>

// --- OSG Includes --- //
#include <osg/Geometry>

#include <osgUtil/IntersectVisitor>

namespace osg
{
class Geode;
class Group;
class Vec4d;
class Vec3d;
class LineSegment;
}

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- C/C++ Libraries --- //
#include <utility>

namespace ves
{
namespace xplorer
{

/*!\file KeyboardMouse.h
 *
 */
/*!\class VE_XPlorer::KeyboardMouse
 *
 */
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
    ///Constructor
    KeyboardMouse();

    ///Destructor
    ~KeyboardMouse();

    ///Update the position in scene
    virtual void UpdateNavigation();

    ///Update the current object selected
    virtual void UpdateSelection();

    ///Sets the screen corner values
    ///\param values A map of strings to doubles
    void SetScreenCornerValues( std::map< std::string, double > values );

    ///Determines whether or not in animation mode
    ///\param animate Bool to determine animation mode
    void Animate( bool animate );

    ///Set the window properties
    ///\param w Set the width of the window
    ///\param h Set the height of the window
    void SetWindowValues( unsigned int w, unsigned int h );

    ///Set the frustrum values
    ///\param l
    ///\param r
    ///\param t
    ///\param b
    ///\param n
    ///\param f
    void SetFrustumValues(
        double l, double r, double b, double t, double n, double f );

    ///Fit the world bounding volume into the viewing frustum
    void FrameAll();

    ///Fit the world bounding volume into the viewing frustum
    void SkyCam( );

    ///
    void SkyCamTo( );

    ///Fit the selected objects bounding volume into the viewing frustum
    void FrameSelection();

    ///Resets the scene to original position
    void ResetTransforms();
 
    ///Update the start and end points for the line
    void UpdateSelectionLine();

protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint(
        osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Draws a line to help visualize the selection process
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

private:
    ///Processes any keyboard events
    ///\param mode Determines whether in navigation mode or selection mode
    void ProcessKBEvents( int mode );

    ///Processes the navigation events
    void ProcessNavigationEvents();

    ///Processes the selection events
    void ProcessSelectionEvents();

    ///Process the NURBS selection events
    void ProcessNURBSSelectionEvents();

    ///Process if selection is valid
    ///\param listOfHits A vector containing CAD hit in selection process
    void ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits );

    ///Navigation functions called on keyboard press events
    void NavOnKeyboardPress();

    ///Navigation functions called on keyboard release events
    void NavOnKeyboardRelease();

    ///Navigation functions called on mouse press events
    void NavOnMousePress();

    ///Navigation functions called on mouse release events
    void NavOnMouseRelease();

    ///Navigation using the mouse
    void NavOnMouseMotion( std::pair< double, double > delta );

    ///Currently this does nothing
    void SelOnKeyboardPress();

    ///Selection functions called on mouse press events
    void SelOnMousePress();

    ///Selection functions called on mouse release events
    void SelOnMouseRelease();

    ///Currently this does nothing
    void SelOnMouseMotion( std::pair< double, double > delta );

    ///Do not know how to describe this
    ///\param dx The change in the x direction
    ///\param dy The change in the y direction
    //void RotateView( double dx, double dy );

    ///Do not know how to describe this
    void Twist();

    ///Handles movement in and out of the scene
    ///\param dy The change in the y direction
    void Zoom( double dy );

    ///Handles movement in and out of the scene
    ///\param dy The change in the y direction
    void Zoom45( double dy  );

    ///Handles panning movements through the scene
    ///\param dx The change in the x direction
    ///\param dy The change in the y direction
    void Pan( double dx, double dy );

    ///Handles rotation of the scene
    ///\param x
    ///\param y
    ///\param z
    ///\param angle
    void Rotate( double x, double y, double z, double angle );

    ///Determines whether in animation mode
    bool mAnimate;

    ///Width of the window
    unsigned int mWidth;

    ///Height of the window
    unsigned int mHeight;

    ///Holds the value of the key being used
    int mKey;

    ///Holds the value of the mouse button being used
    int mButton;

    ///Determines if mouse button is pressed or released
    int mState;

    ///x position of the mouse
    int mX;

    ///y position of the mouse
    int mY;

    ///Aspect ratio of window
    double mAspectRatio;

    ///Field of view in the y direction
    double mFoVY;

    ///The left frustum value
    double mLeftFrustum;

    ///The right frustum value
    double mRightFrustum;

    ///The top frustum value
    double mTopFrustum;

    ///The bottom frustum value
    double mBottomFrustum;

    ///The near frustum value
    double mNearFrustum;

    ///The far frustum value
    double mFarFrustum;

    ///The magnitude of the mouse movement
    double mMagnitude;

    ///
    double mSensitivity;

    ///The minimum x position of the screen
    double mXMinScreen;

    ///The maximum x position of the screen
    double mXMaxScreen;

    ///The minimum y position of the screen
    double mYMinScreen;

    ///The maximum y position of the screen
    double mYMaxScreen;

    ///The z position of the screen
    double mZValScreen;

    ///The distance from the head position to the picked btRigidBody point
    ///Used to calculate point to point constraints for physics picking
    double mPrevPhysicsRayPos;

    ///The current mouse position
    std::pair< double, double > mCurrPos;

    ///The previous mouse position
    std::pair< double, double > mPrevPos;

    /*
    Note: osg::Matrix multiplication is reverse of gmtl::Matrix multiplication
    For: gmtl::Matrix
                    In mData form    In row by column form
                    [ 0 4  8 12 ]    [ 00 01 02 03 ]
                    [ 1 5  9 13 ]    [ 10 11 12 13 ]
                    [ 2 6 10 14 ]    [ 20 21 22 23 ]
                    [ 3 7 11 15 ]    [ 30 31 32 33 ]
    */
    ///The change to be applied to the current transform
    gmtl::Matrix44d mDeltaTransform;

    ///
    osg::ref_ptr< osg::Geode > mBeamGeode;

    ///The geometry being selected
    osg::ref_ptr< osg::Geode > mSelectedGeometry;

    ///
    osg::ref_ptr< osg::LineSegment > mBeamLineSegment;

    ///VRJuggler's keyboard/mouse positional interface
    gadget::KeyboardMouseInterface mKeyboard;

    ///VRJuggler's head positional interface
    gadget::PositionInterface mHead;

    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* mPickedBody;

    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* mPickConstraint;

};
} //end xplorer
} //end ves

#endif //KEYBOARD_MOUSE_H
