/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef VES_XPLORER_DEVICE_KEYBOARDMOUSE_H
#define VES_XPLORER_DEVICE_KEYBOARDMOUSE_H

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

namespace gadget
{
class InputArea;
}

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- Boost includes --- //
#ifdef QT_ON
#include <boost/signals2/signal.hpp>
#include <ves/xplorer/eventmanager/InteractionEvent.h>
#endif // QT_ON

// --- STL Includes --- //
#include <bitset>

namespace ves
{
namespace xplorer
{
namespace device
{

/*!\file KeyboardMouse.h
 *
 */
/*!\class ves::xplorer::device::KeyboardMouse
 *
 */
/*!\namespace ves::xplorer::device
 *
 */
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
    ///Constructor
    KeyboardMouse();

    ///Destructor
    ~KeyboardMouse();

    ///
    ///\return
    virtual KeyboardMouse* AsKeyboardMouse();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

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
    ///\return
    gadget::KeyboardMousePtr GetKeyboardMouseVRJDevice();

    ///Get the line segment intersector
    ///\pre UpdateSelectionLine must be called first
    ///\return Returns the osg class that manages the line interesection tests
    osgUtil::LineSegmentIntersector* GetLineSegmentIntersector();

    ///Set wether the keyboardmouse device should select things
    ///\param processSelection
    void SetProcessSelection( bool processSelection );

    ///
    ///\return
    bool GetMousePickEvent();

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
    ///Functions called on keyboard press events
    void OnKeyPress();

    ///Functions called on keyboard release events
    void OnKeyRelease();

    ///Functions called on mouse press events
    void OnMousePress( gadget::InputArea& inputArea );

    ///Functions called on mouse release events
    void OnMouseRelease( gadget::InputArea& inputArea );

    ///Functions called on mouse move events
    void OnMouseMotionDown( double dx, double dy );

    ///Functions called on mouse move events
    void OnMouseMotionUp();

    ///Processes the navigation events
    void ProcessNavigation();

    ///Processes the selection events
    virtual void ProcessSelection();

    ///Rotates an object about the y-axis
    void Twist( double dx, double dy );

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
    ///\param angle
    ///\param axis
    void Rotate( double angle, gmtl::Vec3d axis );

    ///
    ///\param
    ///\return
    vrj::DisplayPtr const GetCurrentDisplay(
        const gadget::InputArea* inputArea );

    ///
    ///\param
    ///\param
    ///\return
    bool SetCurrentGLTransformInfo(
        const vrj::DisplayPtr display, bool isKeyEvent );

    ///Create physics point constraint
    ///\return
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

    ///Holds the value of the current key being used
    gadget::Keys m_currKey;

    ///
    gadget::Keys m_currMouse;

    ///Width of the window
    unsigned int m_windowWidth;

    ///Height of the window
    unsigned int m_windowHeight;

    ///
    const unsigned int m_pickCushion;

    ///
    unsigned int m_xMotionPixels;

    ///
    unsigned int m_yMotionPixels;

    ///The current X mouse position
    int m_currX;

    ///The current Y mouse position
    int m_currY;

    //These variables are needed on linux and mac to calculate mouse dx and dy
#if !defined( VPR_OS_Windows )
    ///The previous X mouse position
    int m_prevX;

    ///The previous Y mouse position
    int m_prevY;
#endif

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

    /*
    Note: osg::Matrix multiplication is reverse of gmtl::Matrix multiplication
    For: gmtl::Matrix
                    In mData form    In [row][col] form
                    [ 0 4  8 12 ]    [ 00 01 02 03 ]
                    [ 1 5  9 13 ]    [ 10 11 12 13 ]
                    [ 2 6 10 14 ]    [ 20 21 22 23 ]
                    [ 3 7 11 15 ]    [ 30 31 32 33 ]
    */
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

    ///
    std::bitset< gadget::LAST_KEY > m_keys;

#ifdef QT_ON
    typedef boost::signals2::signal<bool (ves::xplorer::eventmanager::InteractionEvent&)> InteractionSignal_type;
    InteractionSignal_type mInteractionSignal;

    typedef boost::signals2::signal<void ( )> HideShowUISignal_type;
    HideShowUISignal_type mHideShowUISignal;
    
    typedef boost::signals2::signal< void (osg::NodePath&) > ObjectPickedSignal_type;
    ObjectPickedSignal_type mObjectPickedSignal;
#endif // QT_ON

};
} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_KEYBOARDMOUSE_H

//This stuff is used for the old NURBS selection events
//In the future, NURBS should be selectable in the scene like manipulators
//A button in the toolbar could turn NURBS points on/off like manipulators
/*
///Process the NURBS selection events
void ProcessNURBSSelectionEvents();

///Currently this does nothing
void SelOnKeyboardPress();

///Selection functions called on mouse press events
void SelOnMousePress();

///Selection functions called on mouse release events
void SelOnMouseRelease();

///Currently this does nothing
void SelOnMouseMotion( std::pair< double, double > delta );
*/
