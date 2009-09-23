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

#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

// --- vrJuggler Includes --- //
#include <vrj/Display/DisplayPtr.h>

#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <boost/shared_ptr.hpp>

#include <gadget/gadgetParam.h>

#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/PositionInterface.h>

#include <gadget/Type/KeyboardMouse/EventPtr.h>

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- C/C++ Includes --- //

namespace ves
{
namespace xplorer
{
namespace device
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

    ///Processes keyboard events
    virtual void ProcessEvents();

    ///Sets the screen corner values
    ///\param values A map of strings to doubles
    void SetScreenCornerValues( std::map< std::string, double > values );

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
    void SkyCam();

    ///Fit the selected objects bounding volume into the viewing frustum
    void FrameSelection();

    ///Resets the scene to original position
    void ResetTransforms();
 
    ///Update the start and end points for the line
    void UpdateSelectionLine();
    
    ///Get raw vrjuggler keyboardmouse ptr
    gadget::KeyboardMousePtr GetKeyboardMouseVRJDevice();
    
    ///Get the line segment intersector
    ///\pre UpdateSelectionLine must be called first
    ///\return Returns the osg class that manages the line interesection tests
    osgUtil::LineSegmentIntersector* GetLineSegmentIntersector();

    ///Set wether the keyboardmouse device should select things
    void SetProcessSelection( bool processSelection );

protected:
    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void SetStartEndPoint(
        osg::Vec3d& startPoint, osg::Vec3d& endPoint );

    ///Draws a line to help visualize the selection process
    ///\param startPoint The start position
    ///\param endPoint The end position
    virtual void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

private:
    ///Processes the navigation events
    void ProcessNavigation();

    ///Processes the selection events
    virtual void ProcessSelection();

    /*
    ///Process the NURBS selection events
    void ProcessNURBSSelectionEvents();
    */

    ///Functions called on keyboard press events
    void OnKeyPress();

    ///Functions called on keyboard release events
    void OnKeyRelease();

    ///Functions called on mouse press events
    void OnMousePress();

    ///Functions called on mouse release events
    void OnMouseRelease();

    ///Functions called on mouse move events
    void OnMouseMotionDown();

    ///Functions called on mouse move events
    void OnMouseMotionUp();

    //This stuff is used for the old NURBS selection events
    //In the future, NURBS should be selectable in the scene like manipulators
    //A button in the toolbar could turn NURBS points on/off like manipulators
    /*
    ///Currently this does nothing
    void SelOnKeyboardPress();

    ///Selection functions called on mouse press events
    void SelOnMousePress();

    ///Selection functions called on mouse release events
    void SelOnMouseRelease();

    ///Currently this does nothing
    void SelOnMouseMotion( std::pair< double, double > delta );
    */

    ///Rotates an object about the y-axis
    void Twist();

    ///Handles movement in and out of the scene
    ///\param dy The change in the y direction
    void Zoom( double dy );

    ///Handles movement in and out of the scene
    ///\param dy The change in the y direction
    void Zoom45( double dy  );

    ///Handles panning movements through the scene
    ///\param dx The change in the x direction
    ///\param dz The change in the yz direction
    void Pan( double dx, double dz );

    ///Handles rotation of the scene
    ///\param x
    ///\param y
    ///\param z
    ///\param angle
    void Rotate( double angle, gmtl::Vec3d axis );

#if __GADGET_version >= 1003023
    ///
    vrj::DisplayPtr const GetCurrentDisplay( const gadget::EventPtr event );

    ///
    bool SetCurrentGLTransformInfo(
        const vrj::DisplayPtr display, bool isKeyEvent );
#endif //__GADGET_version >= 1003023

    ///Create physics point constraint
    bool CreatePointConstraint();
    ///Clear point constraint
    void ClearPointConstraint();
    ///Update point constraint
    void UpdatePointConstraint();
    
    ///Is no key pushed
    bool mKeyNone;

    ///Is shift pushed
    bool mKeyShift;

    ///Is alt pushed
    bool mKeyAlt;

    ///Process selection from a mouse pick
    bool m_processSelection;

    ///Are we in selection or nav mode
    bool m_mousePickEvent;

    ///Width of the window
    unsigned int mWidth;

    ///Height of the window
    unsigned int mHeight;

    ///
    const unsigned int m_pickCushion;

    ///
    unsigned int m_xMotionPixels;

    ///
    unsigned int m_yMotionPixels;

    ///Holds the value of the key being used
    int mKey;

    ///Holds the value of the mouse button being used
    int mButton;

    ///Determines if mouse button is pressed or released
    bool mState;

    ///x position of the mouse
    int mX;

    ///y position of the mouse
    int mY;

    ///Aspect ratio of window
    double mAspectRatio;

    ///Field of view in the y direction
    double mFoVZ;

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
    std::pair< int, int > mCurrPos;

    ///The previous mouse position
    std::pair< int, int > mPrevPos;

    /*
    Note: osg::Matrix multiplication is reverse of gmtl::Matrix multiplication
    For: gmtl::Matrix
                    In mData form    In [row][col] form
                    [ 0 4  8 12 ]    [ 00 01 02 03 ]
                    [ 1 5  9 13 ]    [ 10 11 12 13 ]
                    [ 2 6 10 14 ]    [ 20 21 22 23 ]
                    [ 3 7 11 15 ]    [ 30 31 32 33 ]
    */
    ///The change to be applied to the current transform
    //gmtl::Matrix44d mDeltaTransform;

    ///
    gmtl::Quatd mDeltaRotation;

    ///
    gmtl::Vec3d mDeltaTranslation;

    ///
    osg::ref_ptr< osg::Geode > mBeamGeode;

    ///The geometry being selected
    osg::ref_ptr< osg::Geode > mSelectedGeometry;

    ///
    osg::ref_ptr< osgUtil::LineSegmentIntersector > mLineSegmentIntersector;

    ///
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;

    ///VRJuggler's keyboard/mouse positional interface
    gadget::KeyboardMouseInterface mKeyboardMouse;

    ///VRJuggler's head positional interface
    gadget::PositionInterface mHead;

    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* mPickedBody;

    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* mPickConstraint;

    /*
    void RotateView( double dx, double dy )
    {
        double tbAxis[ 3 ];
        double angle = mMagnitude * 400.0;

        gmtl::Matrix44d matrix;
        gmtl::identity( matrix );

        dy *= -1.0;
        tbAxis[ 0 ] = matrix.mData[ 0 ] * dy + matrix.mData[  2 ] * dx;
        tbAxis[ 1 ] = matrix.mData[ 4 ] * dy + matrix.mData[  6 ] * dx;
        tbAxis[ 2 ] = matrix.mData[ 8 ] * dy + matrix.mData[ 10 ] * dx;

        Rotate( tbAxis[ 0 ], tbAxis[ 1 ], tbAxis[ 2 ], angle );
    }
    */

};
} //end device
} //end xplorer
} //end ves

#endif //KEYBOARD_MOUSE_H
