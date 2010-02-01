/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,fmin
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iam_state.edu/~kmbryden
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/device/KeyboardMouse.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>

#include <ves/xplorer/plugin/PluginBase.h>

#include <ves/xplorer/network/cfdExecutive.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/LocalToWorldNodePath.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/PointLineSegmentIntersector.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/manipulator/RotateTwist.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/environment/NavigationAnimationEngine.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- vrJuggler Includes --- //
#include <vrj/vrjParam.h>

#include <gadget/Devices/KeyboardMouseDevice/InputArea.h>
#if __VJ_version >= 2003000
#include <vrj/Draw/OpenGL/Window.h>
#if defined VPR_OS_Darwin
#include <vrj/Draw/OpenGL/WindowCocoa.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowCocoa.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaCocoa.h>
#elif defined VPR_OS_Windows
#include <vrj/Draw/OpenGL/WindowWin32.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowWin32.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaWin32.h>
#elif defined VPR_OS_Linux
#include <vrj/Draw/OpenGL/WindowXWin.h>
#include <gadget/Devices/KeyboardMouseDevice/InputWindowXWin.h>
#include <gadget/Devices/KeyboardMouseDevice/InputAreaXWin.h>
#endif
#else
#endif //__VJ_version >= 2003000

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include <gadget/Devices/KeyboardMouseDevice/InputArea.h>

#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
//#include <osg/PolygonStipple>

// --- osgBullet Includes --- //
#include <osgwTools/AbsoluteModelTransform.h>
#include <osgbBullet/RefRigidBody.h>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace ves::xplorer::device;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
    :
    Device( KEYBOARD_MOUSE ),

    mKeyNone( false ),
    mKeyShift( false ),
    mKeyAlt( false ),

    m_mousePickEvent( false ),
    m_processSelection( true ),

    mWidth( 1 ),
    mHeight( 1 ),
    m_pickCushion( 1 ),
    m_xMotionPixels( 0 ),
    m_yMotionPixels( 0 ),

    mKey( -1 ),
    mButton( -1 ),
    mState( false ),
    mX( 0 ),
    mY( 0 ),

    mAspectRatio( 0.0 ),
    mFoVZ( 0.0 ),

    mLeftFrustum( 0.0 ),
    mRightFrustum( 0.0 ),
    mTopFrustum( 0.0 ),
    mBottomFrustum( 0.0 ),
    mNearFrustum( 0.0 ),
    mFarFrustum( 0.0 ),

    mMagnitude( 0.0 ),

    mXMinScreen( 0.0 ),
    mXMaxScreen( 0.0 ),
    mYMinScreen( 0.0 ),
    mYMaxScreen( 0.0 ),
    mZValScreen( 0.0 ),

    mPrevPhysicsRayPos( 0.0 ),

    mCurrPos( 0, 0 ),
    mPrevPos( 0, 0 ),

    mDeltaRotation( 0.0, 0.0, 0.0, 1.0 ),
    mDeltaTranslation( 0.0, 0.0, 0.0 ),

    mLineSegmentIntersector(
        new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),

    m_currentGLTransformInfo( scenegraph::GLTransformInfoPtr() ),

    mPickedBody( NULL ),
    mPickConstraint( NULL )
{
    mKeyboardMouse.init( "VJKeyboard" );
    mHead.init( "VJHead" );
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::~KeyboardMouse()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse* KeyboardMouse::AsKeyboardMouse()
{
    return this;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetStartEndPoint(
    osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
#if __GADGET_version >= 1003023
    osg::Matrixd inverseMVPW = m_currentGLTransformInfo->GetOSGMVPWMatrix();
    inverseMVPW.invert( inverseMVPW );

    startPoint = osg::Vec3d( mX, mY, 0.0 ) * inverseMVPW;
    endPoint = osg::Vec3d( mX, mY, 1.0 ) * inverseMVPW;

    //std::cout << "startPoint: " << startPoint << std::endl;
    //std::cout << "endPoint: " << endPoint << std::endl;
    
#else
    //Meters to feet conversion
    double m2ft = 3.2808399;

    //Set the start point to be the head position in osg space
    {
        //Note: for osg we are in z up land
        gmtl::Matrix44d vjHeadMat =
            gmtl::convertTo< double >( mHead->getData() );
        gmtl::Point3d jugglerHeadPoint =
            gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

        //We have to offset negative mX because the
        //view and frustum are drawn for the left eye
        startPoint.set(
            jugglerHeadPoint.mData[ 0 ] - ( 0.0345 * m2ft ),
           -jugglerHeadPoint.mData[ 2 ],
            jugglerHeadPoint.mData[ 1 ] );
    }

    //Set the end point
    {
        //Be sure mWidth and mHeight are set before calling this function
        double xScreenRatio =
            ( mXMaxScreen - mXMinScreen ) / static_cast< double >( mWidth );
        double yScreenRatio =
            ( mYMaxScreen - mYMinScreen ) / static_cast< double >( mHeight );

        //Get the mouse position in juggler world coordinates
        osg::Vec3d jugglerMousePosition(
            mXMinScreen + ( mX * xScreenRatio ),
#if __VJ_version >= 2003000
            mYMinScreen + ( mY * yScreenRatio ),
#else
            mYMinScreen - ( mY * yScreenRatio ),
#endif
            mZValScreen );

        //Convert meters to feet
        jugglerMousePosition *= m2ft;

        //Get the mouse position in osg world coordinates
        osg::Vec3d mousePosition(
            jugglerMousePosition.x(),
           -jugglerMousePosition.z(),
            jugglerMousePosition.y() );

        //Find where the end point lands on the far plane
        osg::Vec3d vecNear = mousePosition - startPoint;
        osg::Vec3d vecFar = -startPoint;
        vecFar.y() += mFarFrustum;

        double distance = vecFar.y() / vecNear.y();
        endPoint.set(
            startPoint.x() + ( vecNear.x() * distance ),
            mFarFrustum,
            startPoint.z() + ( vecNear.z() * distance ) );
    }

    //Need to negate the the camera transform that is multiplied into the view
    {
        osg::Matrixd inverseCameraTransform(
            m_sceneManager.GetInvertedWorldDCS().getData() );

        startPoint = startPoint * inverseCameraTransform;
        endPoint = endPoint * inverseCameraTransform;
    }

    
    //std::cout << "startPoint: " << startPoint << std::endl;
    //std::cout << "endPoint: " << endPoint << std::endl;
#endif //__GADGET_version >= 1003023
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{   
    osg::Group* rootNode = m_sceneManager.GetRootNode();

    if( mBeamGeode.valid() )
    {
        rootNode->removeChild( mBeamGeode.get() );
    }

    mBeamGeode = new osg::Geode();
    mBeamGeode->setName( "Laser" );

    osg::ref_ptr< osg::Geometry > line = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array();
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet();

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 1.0, 0.0, 0.0, 1.0 ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > line_width = new osg::LineWidth();
    line_width->setWidth( 2.0 );
    stateset->setAttribute( line_width.get() );
    line->setStateSet( stateset.get() );

    line->addPrimitiveSet( new osg::DrawArrays(
        osg::PrimitiveSet::LINES, 0, vertices->size() ) );

    mBeamGeode->addDrawable( line.get() );
    
    rootNode->addChild( mBeamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetScreenCornerValues(
    std::map< std::string, double > values )
{
    mXMinScreen = values[ "xmin" ];
    mXMaxScreen = values[ "xmax" ];
    mYMinScreen = values[ "ymin" ];
    mYMaxScreen = values[ "ymax" ];
    mZValScreen = values[ "zval" ];
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessEvents()
{
    //Get the event queue
    gadget::KeyboardMouse::EventQueue evt_queue =
        mKeyboardMouse->getEventQueue();

    //Reset intersector every frame so that the list is erased if others are 
    //using the list
    mLineSegmentIntersector->reset();

    //Return if no events occurred
    if( evt_queue.empty() )
    {
        return;
    }

    //Get the modifier key values
    mKeyNone = mKeyboardMouse->modifierOnly( gadget::KEY_NONE );
    mKeyShift = mKeyboardMouse->modifierOnly( gadget::KEY_SHIFT );
    mKeyAlt = mKeyboardMouse->modifierOnly( gadget::KEY_ALT );

    //Iterate over the keyboard and mouse events
    gadget::KeyboardMouse::EventQueue::const_iterator i;
    for( i = evt_queue.begin(); i != evt_queue.end(); ++i )
    {
        const gadget::EventPtr event = *i;
#if __GADGET_version >= 1003023
        //Get the current display from the event
        vrj::DisplayPtr currentDisplay = GetCurrentDisplay( event );
#endif //__GADGET_version >= 1003023

        const gadget::EventType eventType = event->type();
        switch( eventType )
        {
        case gadget::KeyPressEvent:
        {
            const gadget::KeyEventPtr keyEvt =
                boost::dynamic_pointer_cast< gadget::KeyEvent >( event );

            mKey = keyEvt->getKey();

#if __GADGET_version >= 1003023
            //Set the current GLTransfromInfo from the event
            if( !SetCurrentGLTransformInfo( currentDisplay, true ) )
            {
                return;
            }
#endif //__GADGET_version >= 1003023

            OnKeyPress();

            break;
        }
        case gadget::KeyReleaseEvent:
        {
            const gadget::KeyEventPtr keyEvt =
                boost::dynamic_pointer_cast< gadget::KeyEvent >( event );

            mKey = keyEvt->getKey();

#if __GADGET_version >= 1003023
            //Set the current GLTransfromInfo from the event
            if( !SetCurrentGLTransformInfo( currentDisplay, true ) )
            {
                return;
            }
#endif //__GADGET_version >= 1003023

            OnKeyRelease();

            break;
        }
        case gadget::MouseButtonPressEvent:
        {
            const gadget::MouseEventPtr mouse_evt =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( event );

            mButton = mouse_evt->getButton();
            mState = true;
            mX = mouse_evt->getX();
            mY = mouse_evt->getY();

#if __GADGET_version >= 1003023
            //Set the current GLTransfromInfo from the event
            if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
            {
                return;
            }
#endif //__GADGET_version >= 1003023

            OnMousePress();

            break;
        }
        case gadget::MouseButtonReleaseEvent:
        {
            const gadget::MouseEventPtr mouse_evt =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( event );

            mButton = mouse_evt->getButton();
            mState = false;
            mX = mouse_evt->getX();
            mY = mouse_evt->getY();

#if __GADGET_version >= 1003023
            //Set the current GLTransfromInfo from the event
            if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
            {
                return;
            }
#endif //__GADGET_version >= 1003023

            OnMouseRelease();

            break;
        }
        case gadget::MouseMoveEvent:
        {
            const gadget::MouseEventPtr mouse_evt =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( event );

            mX = mouse_evt->getX();
            mY = mouse_evt->getY();

#if __GADGET_version >= 1003023
            //Set the current GLTransfromInfo from the event
            if( !SetCurrentGLTransformInfo( currentDisplay, false ) )
            {
                return;
            }
#endif //__GADGET_version >= 1003023

            if( !mState )
            {
                OnMouseMotionUp();
            }
            else
            {
                OnMouseMotionDown();
            }

            break;
        }
        } //end switch( eventType )
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNavigation()
{
    gmtl::Matrix44d newTransform;
    gmtl::Matrix44d currentTransform;

    scenegraph::DCS* const activeDCS = DeviceHandler::instance()->GetActiveDCS();
    //Get the node where are all the geometry is handled
    osg::Group* const activeSwitchNode = m_sceneManager.GetActiveSwitchNode();
    //Get the node where all the nav matrix's are handled
    scenegraph::DCS* const cameraDCS = m_sceneManager.GetActiveNavSwitchNode();

    osg::ref_ptr< scenegraph::CoordinateSystemTransform > coordinateSystemTransform;
    //Test if we are manipulating the camera dcs or a model dcs
    if( activeDCS->GetName() != cameraDCS->GetName() )
    {
        //If local dcs, transform to camera space
        coordinateSystemTransform = new scenegraph::CoordinateSystemTransform(
            activeSwitchNode, activeDCS, true );

        currentTransform = coordinateSystemTransform->GetTransformationMatrix();
    }
    else
    {
        //If manipulating camera, no transformations are needed
        currentTransform = activeDCS->GetMat();
    }

    //Translate current transform origin back by the center point position
    gmtl::Matrix44d negCenterPointMatrix =
        gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
    newTransform = negCenterPointMatrix * currentTransform;

    //Apply the delta transform at this new position
    gmtl::Matrix44d deltaTransform;
    gmtl::setRot( deltaTransform, mDeltaRotation );
    gmtl::setTrans( deltaTransform, mDeltaTranslation );
    newTransform = deltaTransform * newTransform;

    //Add back the center point position to the transform
    gmtl::Matrix44d posCenterPointMatrix =
        gmtl::makeTrans< gmtl::Matrix44d >( *mCenterPoint );
    newTransform = posCenterPointMatrix * newTransform;

    //Convert matrix back to local space after delta transform has been applied
    if( activeDCS->GetName() != cameraDCS->GetName() )
    {
        //Remove local matrix from currentTransform
        //We are multiplying by a new transformed local matrix
        currentTransform =
            coordinateSystemTransform->GetTransformationMatrix( false );

        newTransform = gmtl::invert( currentTransform ) * newTransform;
    }

    //Set the activeDCS w/ new transform
    activeDCS->SetMat( newTransform );

    //Reset the delta transform
    mDeltaRotation.set( 0.0, 0.0, 0.0, 1.0 );
    mDeltaTranslation.set( 0.0, 0.0, 0.0 );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetWindowValues( unsigned int w, unsigned int h )
{
    mWidth = w;
    mHeight = h;

    mAspectRatio =
        static_cast< double >( mWidth ) / static_cast< double >( mHeight );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetFrustumValues(
    double l, double r, double b, double t, double n, double f )
{
    mLeftFrustum = l;
    mRightFrustum = r;
    mBottomFrustum = b;
    mTopFrustum = t;
    mNearFrustum = n;
    mFarFrustum = f;

    double topAngle = atan( mTopFrustum / mNearFrustum );
    double tempDiv = fabs( mBottomFrustum ) / mNearFrustum;
    double bottomAngle = atan( tempDiv );

    mFoVZ = topAngle + bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
    osg::Group* activeSwitchNode = m_sceneManager.GetActiveSwitchNode();
    scenegraph::DCS* activeNavSwitchNode = m_sceneManager.GetActiveNavSwitchNode();
    osg::BoundingSphere bs = activeSwitchNode->computeBound();

    //Meters to feet conversion
    double m2ft = 3.2808399;

    //Set the start point to be the head position in osg space
    osg::Vec3d startPoint( 0.0, 0.0, 0.0 );
    {
        //Note: for osg we are in z up land
        gmtl::Matrix44d vjHeadMat =
            gmtl::convertTo< double >( mHead->getData() );
        gmtl::Point3d jugglerHeadPoint =
            gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

        //We have to offset negative mX because the
        //view and frustum are drawn for the left eye
        startPoint.set(
            jugglerHeadPoint.mData[ 0 ] - ( 0.0345 * m2ft ),
           -jugglerHeadPoint.mData[ 2 ],
            jugglerHeadPoint.mData[ 1 ] );
    }

    //Set the end point
    osg::Vec3d endPoint( 0.0, 0.0, 0.0 );
    {
        //Be sure mWidth and mHeight are set before calling this function
        double xScreenRatio =
            ( mXMaxScreen - mXMinScreen ) / static_cast< double >( mWidth );
        double yScreenRatio =
            ( mYMaxScreen - mYMinScreen ) / static_cast< double >( mHeight );

        //Get the center screen position in juggler world coordinates
        osg::Vec3d vrjCenterScreenPosition( 
            mXMinScreen + 0.5 * static_cast< double >( mWidth ) * xScreenRatio,
            mYMinScreen + 0.5 * static_cast< double >( mHeight ) * yScreenRatio,
            mZValScreen );

        //Convert meters to feet
        vrjCenterScreenPosition *= m2ft;

        //Get the screen position in osg world coordinates
        osg::Vec3d centerPosition(
            vrjCenterScreenPosition.x(),
           -vrjCenterScreenPosition.z(),
            vrjCenterScreenPosition.y() );

        //Calculate the distance we need to move along the center vector to fit
        //the bounding sphere of all the geometry inside the viewing frustum
        double theta = mFoVZ * 0.5;
        if( mAspectRatio < 1.0 )
        {
            theta *= mAspectRatio;
        }
        double distance;
        distance = ( bs.radius() / tan( theta ) );

        //Now we can get our center frustum vector
        osg::Vec3d vecNear = centerPosition - startPoint;
        osg::Vec3d vecFar = -startPoint;
        vecFar.y() = distance + vecFar.y();

        double ratio = vecFar.y() / vecNear.y();
        endPoint.set(
            startPoint.x() + ( vecNear.x() * ratio ),
            distance,
            startPoint.z() + ( vecNear.z() * ratio ) );
    }
    
    //Set the current switch node's matrix w/ the new "frame all" transform
    activeNavSwitchNode->setPosition( endPoint - bs.center() );
    activeNavSwitchNode->setAttitude( osg::Quat( 0.0, 0.0, 0.0, 1.0 ) );

    //Get the new center of the bounding sphere in camera space
    activeSwitchNode->computeBound();
    osg::Vec3d center =
        bs.center() * osg::Matrixd( activeNavSwitchNode->GetMat().getData() );
    mCenterPoint->set( center.x(), center.y(), center.z() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameSelection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SkyCam()
{
    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();

    //gmtl::Matrix44d matrix;
    //mCenterPoint->mData[ 1 ] = matrix[ 1 ][ 3 ] = *mCenterPointThreshold;
    //m_sceneManager.GetActiveSwitchNode()->SetMat( matrix );

    //Grab the current matrix
    osg::ref_ptr< scenegraph::DCS > activeSwitchDCS = m_sceneManager.GetWorldDCS();

    //reset view
    activeSwitchDCS->SetQuat( *mResetAxis );
    activeSwitchDCS->SetTranslationArray( *mResetPosition );

    osg::BoundingSphere bs = activeSwitchDCS->computeBound();

    //Calculate the distance
    double distance =  bs.radius();

    //move the cad
    double osgTransformedPosition[ 3 ];
    osgTransformedPosition[ 0 ] = -bs.center( ).x( );
    osgTransformedPosition[ 1 ] = -bs.center( ).y( ) + distance;
    osgTransformedPosition[ 2 ] = -bs.center( ).z( );
    activeSwitchDCS->SetTranslationArray( osgTransformedPosition );

    //Get the new center of the bounding sphere
    bs = activeSwitchDCS->computeBound();
    //Set the center point of rotation to the new center of the bounding sphere
    mCenterPoint->set( bs.center().x(), bs.center().y(), bs.center().z() );

    //put it at 45 degrees
    Rotate( gmtl::Math::deg2Rad( 45.0 ), gmtl::Vec3d( 1.0, 0.0, 0.0 ) );
    ProcessNavigation();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnKeyPress()
{
    switch( mKey )
    {
    case gadget::KEY_R:
    {
        ResetTransforms();

        break;
    }
    case gadget::KEY_F:
    {
        FrameAll();

        break;
    }
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( true );
        }

        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( true );
        }

        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( true );
        }

        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( true );
        }

        break;
    }
    case gadget::KEY_X:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.CanFly() )
            {
                m_characterController.StepDown( true );
            }
        }

        break;
    }
    case gadget::KEY_SPACE:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.CanFly() )
            {
                m_characterController.StepUp( true );
            }
            else
            {
                m_characterController.Jump();
            }
        }

        break;
    }
    case gadget::KEY_K:
    {
        SkyCam();

        break;
    }
        
    case gadget::KEY_UP:
    {
        Zoom45( 0.05 );
        ProcessNavigation();

        break;
    }
    case gadget::KEY_DOWN: 
    {
        Zoom45( -0.05 );
        ProcessNavigation();

        break;
    }
    case gadget::KEY_LEFT:
    {
        Pan( 0.05, 0 );
        ProcessNavigation();

        break;
    }
    case gadget::KEY_RIGHT:
    {
        Pan( -0.05, 0 );
        ProcessNavigation();

        break;
    }
    } //end switch( mKey )
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnKeyRelease()
{
    switch( mKey )
    {
    case gadget::KEY_A:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeLeft( false );
        }

        break;
    }
    case gadget::KEY_S:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepBackward( false );
        }

        break;
    }
    case gadget::KEY_D:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StrafeRight( false );
        }

        break;
    }
    case gadget::KEY_W:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.StepForward( false );
        }

        break;
    }
    case gadget::KEY_X:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.CanFly() )
            {
                m_characterController.StepDown( false );
            }
            else
            {
                ;
            }
        }

        break;
    }
    case gadget::KEY_SPACE:
    {
        if( m_characterController.IsEnabled() )
        {
            if( m_characterController.CanFly() )
            {
                m_characterController.StepUp( false );
            }
            else
            {
                ;
            }
        }

        break;
    }
    } //end switch( mKey )
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnMousePress()
{
    mCurrPos.first = mX;
    mCurrPos.second = mY;

    m_xMotionPixels = 0;
    m_yMotionPixels = 0;

    switch( mButton )
    {
    //Left mouse button
    case gadget::MBUTTON1:
    {
        //Rotate just the camera "3rd person view:
        if( m_characterController.IsEnabled() )
        {
            m_characterController.FirstPersonMode( false );
        }

        //No modifier key
        if( mKeyNone )
        {
            if( m_manipulatorManager.IsEnabled() )
            {
                UpdateSelectionLine();
                if( m_manipulatorManager.Handle(
                        scenegraph::manipulator::Event::PUSH,
                        mLineSegmentIntersector.get() ) )
                {
                    break;
                }
            }

            if( m_characterController.IsEnabled() )
            {
                m_characterController.SetCameraRotationSLERP( false );
            }
        }
        //Mod key shift
        else if( mKeyShift )
        {
            if( !CreatePointConstraint() )
            {
                break;
            }
        }
        else if( mKeyAlt )
        {
            ;
        }

        m_mousePickEvent = true;

        break;
    }
    //Middle mouse button
    case gadget::MBUTTON2:
    {
        m_mousePickEvent = true;

        break;
    }
    //Right mouse button
    case gadget::MBUTTON3:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.FirstPersonMode( true );
            m_characterController.SetCameraRotationSLERP( false );
            m_characterController.SetRotationFromCamera();
        }

        m_mousePickEvent = true;

        break;
    }
    //Scroll wheel up
    case gadget::MBUTTON4:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( true );
        }

        break;
    }
    //Scroll wheel down
    case gadget::MBUTTON5:
    {
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Zoom( false );
        }

        break;
    }
    } //end switch( mButton )

    mPrevPos.first = mCurrPos.first;
    mPrevPos.second = mCurrPos.second;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnMouseRelease()
{
    mCurrPos.first = mX;
    mCurrPos.second = mY;

    switch( mButton )
    {
    //Left mouse button
    case gadget::MBUTTON1:
    {
        //Do not require mod key depending on what the user did
        ClearPointConstraint();
        
        if( m_manipulatorManager.IsEnabled() )
        {
            if( m_manipulatorManager.Handle(
                    scenegraph::manipulator::Event::RELEASE ) )
            {
                break;
            }
        }

        if( m_characterController.IsEnabled() )
        {
            m_characterController.SetCameraRotationSLERP( true );
        }

        //No modifier key
        if( mKeyNone )
        {
            ;
        }
        //Mod key shift
        else if( mKeyShift )
        {
            ;
        }
        else if( mKeyAlt )
        {
            //OnMouseRelease();
            scenegraph::DCS* infoDCS = DeviceHandler::instance()->GetSelectedDCS();
            DeviceHandler::instance()->UnselectObjects();

            std::map< std::string,
                      ves::xplorer::plugin::PluginBase* >* tempPlugins =
                ves::xplorer::network::cfdExecutive::instance()->
                    GetTheCurrentPlugins();

            std::map< std::string, 
                ves::xplorer::plugin::PluginBase* >::iterator pluginIter;

            for( pluginIter = tempPlugins->begin(); 
                pluginIter != tempPlugins->end(); ++pluginIter )
            {
                pluginIter->second->GetCFDModel()->
                    RenderTextualDisplay( false );
            }

            if( !infoDCS )
            {
                break;
            }
            osg::Node::DescriptionList descriptorsList;
            descriptorsList = infoDCS->getDescriptions();
            std::string modelIdStr;
            for( size_t i = 0; i < descriptorsList.size(); ++i )
            {
                //std::cout << descriptorsList.at( i ) << std::endl;
                if( descriptorsList.at( i ) == "VE_XML_MODEL_ID" )
                {
                    modelIdStr = descriptorsList.at( i + 1 );
                    break;
                }
            }
                            
            pluginIter = tempPlugins->find( modelIdStr );
            if( pluginIter != tempPlugins->end() )
            {
                pluginIter->second->GetCFDModel()->RenderTextualDisplay( true );
            }
        }

        break;
    }
    //Middle mouse button
    case gadget::MBUTTON2:
    {
        break;
    }
    //Right mouse button
    case gadget::MBUTTON3:
    {
        break;
    }
    } //end switch( mButton )

    if( m_mousePickEvent )
    {
        UpdateSelectionLine();
        if( m_processSelection )
        {
            ProcessSelection();
        }
        m_mousePickEvent = false;
    }

    mPrevPos.first = mCurrPos.first;
    mPrevPos.second = mCurrPos.second;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnMouseMotionDown()
{
    mCurrPos.first = mX;
    mCurrPos.second = mY;

    double xDelta = mCurrPos.first - mPrevPos.first;
    double yDelta = mCurrPos.second - mPrevPos.second;

    if( m_mousePickEvent )
    {
        m_xMotionPixels += abs( static_cast< int >( xDelta ) );
        m_yMotionPixels += abs( static_cast< int >( yDelta ) );
    }

    xDelta /= mWidth;
    yDelta /= mHeight;

    mMagnitude = sqrt( xDelta * xDelta + yDelta * yDelta );

    switch( mButton )
    {
    //Left mouse button
    case gadget::MBUTTON1:
    {
        //No modifier key
        if( mKeyNone )
        {
            if( m_manipulatorManager.IsEnabled() )
            {
                UpdateSelectionLine();
                if( m_manipulatorManager.Handle(
                        scenegraph::manipulator::Event::DRAG ) )
                {
                    break;
                }
            }

            //Rotate just the camera "3rd person view:
            if( m_characterController.IsEnabled() )
            {
                m_characterController.Rotate( xDelta, yDelta );
            }
            else
            {
                if( ( mX > 0.1 * mWidth ) && ( mX < 0.9 * mWidth ) &&
                    ( mY > 0.1 * mHeight ) && ( mY < 0.9 * mHeight ) )
                {
                    double angle = mMagnitude * 7.0;
#if __VJ_version >= 2003000
                    Rotate( angle, gmtl::Vec3d( -yDelta, 0.0, xDelta ) );
#else
                    Rotate( angle, gmtl::Vec3d(  yDelta, 0.0, xDelta ) );
#endif
                }
                else
                {
                    Twist();
                }

                ProcessNavigation();
            }
        }
        //Mod key shift
        else if( mKeyShift )
        {
            UpdatePointConstraint();
        }

        break;
    }
    //Middle mouse button
    case gadget::MBUTTON2:
    {
        if( m_characterController.IsEnabled() )
        {
            ;
        }
        else
        {
            Pan( xDelta, yDelta );
            ProcessNavigation();
        }

        break;
    }
    //Right mouse button
    case gadget::MBUTTON3:
    {
        //Rotate the character and camera at the same time
        if( m_characterController.IsEnabled() )
        {
            m_characterController.Rotate( xDelta, yDelta );
        }
        else
        {
            Zoom( yDelta );
            ProcessNavigation();
        }

        break;
    }
    } //end switch( mButton )

    //If delta mouse motion is less than m_pickCushion, do selection
    if( m_mousePickEvent && ( ( m_xMotionPixels > m_pickCushion ) ||
                     ( m_yMotionPixels > m_pickCushion ) ) )
    {
        m_mousePickEvent = false;
    }

    mPrevPos.first = mCurrPos.first;
    mPrevPos.second = mCurrPos.second;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::OnMouseMotionUp()
{
    if( m_manipulatorManager.IsEnabled() )
    {
        UpdateSelectionLine();
        if( m_manipulatorManager.Handle(
                scenegraph::manipulator::Event::FOCUS,
                mLineSegmentIntersector.get() ) )
        {
            ;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
/*
//This stuff is used for the old NURBS selection events
//In the future, NURBS should be selectable in the scene like manipulators
//A button in the toolbar could turn NURBS points on/off like manipulators

void KeyboardMouse::SelOnKeyboardPress()
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnMousePress()
{
    UpdateSelectionLine();

    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            if( mKeyNone )
            {
                ProcessNURBSSelectionEvents();
            }

            ProcessSelection();

            break;
        }
        //Middle mouse button
        case gadget::MBUTTON2:
        {
            break;
        }
        //Right mouse button
        case gadget::MBUTTON3:
        {
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnMouseRelease()
{
    UpdateSelectionLine();

    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            if( mKeyNone )
            {
                scenegraph::SetStateOnNURBSNodeVisitor(
                    m_sceneManager.GetActiveSwitchNode(), false,
                    false, mCurrPos, std::pair< double, double >( 0.0, 0.0 ) );
            }

            break;
        }
        //Middle mouse button
        case gadget::MBUTTON2:
        {
            break;
        }
        //Right mouse button
        case gadget::MBUTTON3:
        {
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnMouseMotion( std::pair< double, double > delta )
{
    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            scenegraph::SetStateOnNURBSNodeVisitor(
                m_sceneManager.GetActiveSwitchNode(),
                true, true, mCurrPos, delta );

            break;
        }
        //Middle mouse button
        case gadget::MBUTTON2:
        {
            break;
        }
        //Right mouse button
        case gadget::MBUTTON3:
        {
            break;
        }
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    DeviceHandler::instance()->ResetCenterPoint();

    gmtl::Matrix44d matrix;
    gmtl::identity( matrix );

    osg::ref_ptr< scenegraph::DCS > worldDCS = m_sceneManager.GetWorldDCS();
    worldDCS->SetMat( matrix );
    worldDCS->SetQuat( *mResetAxis );
    worldDCS->SetTranslationArray( *mResetPosition );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist()
{
    double xTheta = mPrevPos.first;
    xTheta /= mWidth;
    double yTheta = mPrevPos.second;
    yTheta /= mHeight;
    double theta = atan2( xTheta - 0.5, yTheta - 0.5 );
    xTheta = mCurrPos.first;
    xTheta /= mWidth;
    yTheta = mCurrPos.second;
    yTheta /= mHeight;
    double newTheta = atan2( xTheta - 0.5, yTheta - 0.5 );
#if __VJ_version >= 2003000
    double angle = newTheta - theta;
#else
    double angle = theta - newTheta;
#endif

    //Twist about the y-axis
    Rotate( angle, gmtl::Vec3d( 0.0, 1.0, 0.0 ) );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( double dy )
{
/*
#if __GADGET_version >= 1003023
    gmtl::Matrix44d vpwMatrix = m_currentGLTransformInfo->GetVPWMatrix();

    double viewlength = mCenterPoint->mData[ 1 ];
    double d = ( viewlength * ( 1 / ( 1 - dy * 2 ) ) ) - viewlength;

    gmtl::Point3d yTransform = *mCenterPoint;
    yTransform.mData[ 1 ] += d;
    yTransform = vpwMatrix * yTransform;

    gmtl::Point3d position = vpwMatrix * *mCenterPoint;
    position.mData[ 2 ] = yTransform.mData[ 2 ];
    position = gmtl::invert( vpwMatrix ) * position;
    mDeltaTranslation = position - *mCenterPoint;

    //Test if center point has breached our specified threshold
    if( position.mData[ 1 ] < *mCenterPointThreshold )
    {
        //Prevent center point from jumping when manipulating a selected object
        scenegraph::DCS* const selectedDCS = DeviceHandler::instance()->GetSelectedDCS();
        if( selectedDCS )
        {
            mDeltaTranslation.set( 0.0, 0.0, 0.0 );

            return;
        }

        position.mData[ 1 ] = *mCenterPointJump;
    }

    *mCenterPoint = position;
#else
*/
    double viewlength = mCenterPoint->mData[ 1 ];
#if __VJ_version >= 2003000
    double d = ( viewlength * ( 1 / ( 1 - dy * 2 ) ) ) - viewlength;
#else
    double d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;
#endif
    mDeltaTranslation.mData[ 1 ] = d;
    mCenterPoint->mData[ 1 ] += d;

    //Test if center point has breached our specified threshold
    if( mCenterPoint->mData[ 1 ] < *mCenterPointThreshold )
    {
        scenegraph::DCS* const selectedDCS = DeviceHandler::instance()->GetSelectedDCS();
        //Only jump center point for the worldDCS
        if( !selectedDCS )
        {
            mCenterPoint->mData[ 1 ] = *mCenterPointJump;
        }
        //Prevent the center point from jumping
        //if we are manipulating a selected object
        else
        {
            mDeltaTranslation.mData[ 1 ] = 0.0;
            mCenterPoint->mData[ 1 ] -= d;
        }
    }
//#endif //__GADGET_version >= 1003023
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom45( double dy )
{
    //needed for sky cam control
    double viewlength = mCenterPoint->mData[ 1 ];
#if __VJ_version >= 2003000
    double d = ( viewlength * ( 1 / ( 1 - dy * 2 ) ) ) - viewlength;
#else
    double d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;
#endif

    mDeltaTranslation.mData[ 1 ] = d;
    mDeltaTranslation.mData[ 2 ] = d;
    mCenterPoint->mData[ 1 ] += d;
    mCenterPoint->mData[ 2 ] += d;

    //Test if center point has breached our specified threshold
    if( mCenterPoint->mData[ 1 ] < *mCenterPointThreshold )
    {
        scenegraph::DCS* const selectedDCS = DeviceHandler::instance()->GetSelectedDCS();
        //Only jump center point for the worldDCS
        if( !selectedDCS )
        {
            mCenterPoint->mData[ 1 ] = *mCenterPointJump;
        }
        //Prevent the center point from jumping
        //if we are manipulating a selected object
        else
        {
            mDeltaTranslation.mData[ 1 ] = 0.0;
            mCenterPoint->mData[ 1 ] -= d;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( double dx, double dz )
{
    /*
#if __GADGET_version >= 1003023
    gmtl::Matrix44d vpwMatrix = m_currentGLTransformInfo->GetVPWMatrix();

    //std::cout << "mCenterPoint: " << *mCenterPoint << std::endl;
    gmtl::Point3d position = vpwMatrix * *mCenterPoint;
    //std::cout << "vpwMatrix * *mCenterPoint: " << position << std::endl;
    position.mData[ 0 ] += dx * mWidth;
    position.mData[ 1 ] += dz * mHeight;
    position = gmtl::invert( vpwMatrix ) * position;
    //std::cout << "gmtl::invert( vpwMatrix ) * position: " << position << std::endl;

    mDeltaTranslation = position - *mCenterPoint;
    *mCenterPoint = position;

    //std::cout << std::endl;
#else
    */
    double d = mCenterPoint->mData[ 1 ];
    double theta = mFoVZ * 0.5 ;
    double b = 2.0 * d * tan( theta );
    double dwx = dx * b;
#if __VJ_version >= 2003000
    double dwz =  dz * b;
#else
    double dwz = -dz * b;
#endif

    if( mAspectRatio > 1.0 )
    {
        dwx *= mAspectRatio;
    }

    mDeltaTranslation.mData[ 0 ] = dwx;
    mDeltaTranslation.mData[ 2 ] = dwz;

    mCenterPoint->mData[ 0 ] += dwx;
    mCenterPoint->mData[ 2 ] += dwz;
//#endif //__GADGET_version >= 1003023
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( double angle, gmtl::Vec3d axis )
{
    gmtl::normalize( axis );
    gmtl::AxisAngled axisAngle( angle, axis );
    mDeltaRotation = gmtl::makeRot< gmtl::Quatd >( axisAngle );

    /*
    double temp = ::sqrtf( x * x + y * y + z * z );
    if( temp != 0.0 )
    {
        double tempRatio = 1 / temp;
        x *= tempRatio;
        y *= tempRatio;
        z *= tempRatio;
    }

    double rad = angle;
    double cosAng = cos( rad );
    double sinAng = sin( rad );

    gmtl::zero( mDeltaTransform );
    mDeltaTransform.mData[ 0 ]  = ( x * x ) +
                                  ( cosAng * ( 1 - ( x * x ) ) );
    mDeltaTransform.mData[ 1 ]  = ( y * x ) -
                                  ( cosAng * ( y * x ) ) + ( sinAng * z );
    mDeltaTransform.mData[ 2 ]  = ( z * x ) -
                                  ( cosAng * ( z * x ) ) - ( sinAng * y );
    mDeltaTransform.mData[ 4 ]  = ( x * y ) -
                                  ( cosAng * ( x * y ) ) - ( sinAng * z );
    mDeltaTransform.mData[ 5 ]  = ( y * y ) +
                                  ( cosAng * ( 1 - ( y * y ) ) );
    mDeltaTransform.mData[ 6 ]  = ( z * y ) -
                                  ( cosAng * ( z * y ) ) + ( sinAng * x );
    mDeltaTransform.mData[ 8 ]  = ( x * z ) -
                                  ( cosAng * ( x * z ) ) + ( sinAng * y );
    mDeltaTransform.mData[ 9 ]  = ( y * z ) -
                                  ( cosAng * ( y * z ) ) - ( sinAng * x );
    mDeltaTransform.mData[ 10 ] = ( z * z ) +
                                  ( cosAng * ( 1 - ( z * z ) ) );
    mDeltaTransform.mData[ 15 ] = 1.0;
    */
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelectionLine()
{
    osg::Vec3d startPoint, endPoint;
    SetStartEndPoint( startPoint, endPoint );
    mLineSegmentIntersector->reset();
    mLineSegmentIntersector->setStart( startPoint );
    mLineSegmentIntersector->setEnd( endPoint );

    //Used to debug the selection line
    //If working correctly, the line should show up as 1 red pixel where picked
    //DrawLine( startPoint, endPoint );
}
////////////////////////////////////////////////////////////////////////////////
/*
void KeyboardMouse::ProcessNURBSSelectionEvents()
{
    osg::ref_ptr< osgUtil::IntersectorGroup > intersectorGroup =
        new osgUtil::IntersectorGroup();
    osg::ref_ptr< scenegraph::nurbs::PointLineSegmentIntersector > intersector =
        new scenegraph::nurbs::PointLineSegmentIntersector(
            mLineSegmentIntersector->getStart(),
            mLineSegmentIntersector->getEnd() );
    intersectorGroup->addIntersector( intersector.get() );

    osgUtil::IntersectionVisitor controlMeshPointIntersectVisitor;

    controlMeshPointIntersectVisitor.setIntersector( intersectorGroup.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    m_sceneManager.GetRootNode()->accept( controlMeshPointIntersectVisitor );

    if( intersectorGroup->containsIntersections() )
    {
         //std::cout<<"Found intersections "<<std::endl;
         ///only want the first one
         scenegraph::nurbs::PointLineSegmentIntersector::Intersections& intersections =
             intersector->getIntersections();
         scenegraph::nurbs::PointLineSegmentIntersector::Intersection closestControlPoint =
             (*intersections.begin());
         osg::ref_ptr<scenegraph::nurbs::NURBSControlMesh> ctMesh =
            dynamic_cast< scenegraph::nurbs::NURBSControlMesh* >(
                closestControlPoint.drawable.get() );
         if( ctMesh.valid() )
         {
             osg::ref_ptr<scenegraph::nurbs::NURBS> nurbs = 
                dynamic_cast<scenegraph::nurbs::NURBS*>( ctMesh->getParent( 0 ) );
             if( nurbs.valid() )
             {
                 nurbs->SetSelectedControlPoint(
                     closestControlPoint.primitiveIndex );
             }
         }
    }
}
*/
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessSelection()
{
    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    m_sceneManager.GetModelRoot()->accept( intersectionVisitor );

    //Unselect the previous selected DCS
    DeviceHandler::instance()->UnselectObjects();

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        mLineSegmentIntersector->getIntersections();

    //Now find the new selected DCS
    if( intersections.empty() )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tKeyboardMouse::ProcessHit No object selected"
            << std::endl << vprDEBUG_FLUSH;

        return;
    }

    //Search for first item that is not the laser
    osg::Node* objectHit( NULL );
    osgUtil::LineSegmentIntersector::Intersections::iterator itr =
        intersections.begin();
    for( itr; itr != intersections.end(); ++itr )
    {
        objectHit = *( itr->nodePath.rbegin() );
        if( objectHit->getName() != "Laser" &&
            objectHit->getName() != "Root Node" )
        {
            break;
        }
    }

    //Make sure it is good
    if( !objectHit )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tKeyboardMouse::ProcessHit Invalid object selected"
            << std::endl << vprDEBUG_FLUSH;

        return;
    }

    //Now find the id for the cad
    scenegraph::FindParentsVisitor parentVisitor( objectHit );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( !parentNode.valid() )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tObject does not have name parent name"
            << objectHit->getParents().front()->getName()
            << std::endl << vprDEBUG_FLUSH;

        return;
    }

    vprDEBUG( vesDBG, 1 ) << "|\tObjects has name "
                          << parentNode->getName()
                          << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\tObjects descriptors "
                          << parentNode->getDescriptions().at( 1 )
                          << std::endl << vprDEBUG_FLUSH;

    scenegraph::DCS* newSelectedDCS =
        static_cast< scenegraph::DCS* >( parentNode.get() );
    if( m_sceneManager.IsRTTOn() )
    {
        newSelectedDCS->SetTechnique( "Glow" );
    }
    else
    {
        newSelectedDCS->SetTechnique( "Select" );
    }
    DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );

    //Move the center point to the center of the selected object
    osg::ref_ptr< scenegraph::CoordinateSystemTransform > cst =
        new scenegraph::CoordinateSystemTransform(
            m_sceneManager.GetActiveSwitchNode(), newSelectedDCS, true );
    gmtl::Matrix44d localToWorldMatrix = cst->GetTransformationMatrix( false );

    //Multiplying by the new local matrix mCenterPoint
    osg::Matrixd tempMatrix;
    tempMatrix.set( localToWorldMatrix.getData() );
    osg::Vec3d center = newSelectedDCS->getBound().center() * tempMatrix;
    mCenterPoint->set( center.x(), center.y(), center.z() );

    //Set the connection between the scene manipulator and the selected dcs
    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        m_manipulatorManager.GetSceneManipulator();
    scenegraph::manipulator::RotateTwist* rotateTwist =
        m_manipulatorManager.GetTwistManipulator();

    //Check and see if the selected node has an attached physics mesh
    bool hasAPhysicsMesh( false );
    osg::ref_ptr< osgwTools::AbsoluteModelTransform > tempAMT = 
        dynamic_cast< osgwTools::AbsoluteModelTransform* >( 
            newSelectedDCS->getParent( 0 ) );
    if( tempAMT )
    {
        osgbBullet::RefRigidBody* tempRB = 
            dynamic_cast< osgbBullet::RefRigidBody* >( tempAMT->getUserData() );
        if( tempRB )
        {
            hasAPhysicsMesh = true;
        }
    }

    if( hasAPhysicsMesh )
    {
        //rotateTwist->Connect( tempAMT.get() );
        sceneManipulator->Connect( tempAMT.get() );
    }
    else
    {
        //rotateTwist->Connect( newSelectedDCS );
        sceneManipulator->Connect( newSelectedDCS );
    }

    //Move the scene manipulator to the center point
    scenegraph::LocalToWorldNodePath nodePath(
        newSelectedDCS, m_sceneManager.GetModelRoot() );
    scenegraph::LocalToWorldNodePath::NodeAndPathList npl =
        nodePath.GetLocalToWorldNodePath();
    scenegraph::LocalToWorldNodePath::NodeAndPath nap = npl.at( 0 );
    osg::Matrixd localToWorld = osg::computeLocalToWorld( nap.second );
    osg::Vec3d newCenter = newSelectedDCS->getBound().center() * localToWorld;
    //rotateTwist->SetPosition( newCenter );
    sceneManipulator->SetPosition( newCenter );
}
////////////////////////////////////////////////////////////////////////////////
gadget::KeyboardMousePtr KeyboardMouse::GetKeyboardMouseVRJDevice()
{
    return mKeyboardMouse->getKeyboardMousePtr();
}
////////////////////////////////////////////////////////////////////////////////
#if __GADGET_version >= 1003023
vrj::DisplayPtr const KeyboardMouse::GetCurrentDisplay(
    const gadget::EventPtr event )
{
    const gadget::InputArea* inputArea = event->getSource();
    const vrj::opengl::Window* window( NULL );
#if defined VPR_OS_Darwin
    const gadget::InputWindowCocoa* inputWindowCocoa =
        dynamic_cast< const gadget::InputWindowCocoa* >( inputArea );
    if( inputWindowCocoa )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowCocoa* windowCocoa =
        static_cast< const vrj::opengl::WindowCocoa* >( inputArea );
    //upcast
    window = static_cast< const vrj::opengl::Window* >( windowCocoa );
#elif defined VPR_OS_Windows
    const gadget::InputWindowWin32* inputWindowWin32 =
        dynamic_cast< const gadget::InputWindowWin32* >( inputArea );
    if( inputWindowWin32 )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowWin32* windowWin32 =
        static_cast< const vrj::opengl::WindowWin32* >( inputArea );
    //upcast
    window = dynamic_cast< const vrj::opengl::Window* >( windowWin32 );
#elif defined VPR_OS_Linux
    const gadget::InputWindowXWin* inputWindowXWin =
        dynamic_cast< const gadget::InputWindowXWin* >( inputArea );
    if( inputWindowXWin )
    {
        return vrj::DisplayPtr();
    }
    //downcast
    const vrj::opengl::WindowXWin* windowXWin =
        static_cast< const vrj::opengl::WindowXWin* >( inputArea );
    //upcast
    window = dynamic_cast< const vrj::opengl::Window* >( windowXWin );
#endif

    if( window )
    {
        return window->getDisplay();
    }
    else
    {
        //Error output, this should never happen
        vprDEBUG( vesDBG, 2 ) 
            << "VPR OS is not defined properly in KeyboardMouse::GetCurrentDisplay." 
            << std::endl << vprDEBUG_FLUSH;
        return vrj::DisplayPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
bool KeyboardMouse::SetCurrentGLTransformInfo(
    const vrj::DisplayPtr display, bool isKeyEvent )
{
    //If current display is invalid, return
    if( display == vrj::DisplayPtr() )
    {
        return false;
    }

    scenegraph::manipulator::TransformManipulator* sceneManipulator =
        m_manipulatorManager.GetSceneManipulator();
    vrj::ViewportPtr viewport;
    //Iterate over the viewports
    for( unsigned int i = 0; i < display->getNumViewports(); ++i )
    {
        viewport = display->getViewport( i );
        m_currentGLTransformInfo = m_sceneManager.GetGLTransformInfo( viewport );
        if( m_currentGLTransformInfo == scenegraph::GLTransformInfoPtr() )
        {
            return false;
        }

        // ---------- This needs to be optimized ------------ //
        // --- Does not need to be set for every viewport --- //
        const int& windowWidth = m_currentGLTransformInfo->GetWindowWidth();
        const int& windowHeight = m_currentGLTransformInfo->GetWindowHeight();
        SetWindowValues( windowWidth, windowHeight );
        // -------------------------------------------------- //

        if( isKeyEvent )
        {
            return true;
        }

        //Get dimensions of viewport in pixels
        const int& viewportOriginX = m_currentGLTransformInfo->GetViewportOriginX();
        const int& viewportOriginY = m_currentGLTransformInfo->GetViewportOriginY();
        const int& viewportWidth = m_currentGLTransformInfo->GetViewportWidth();
        const int& viewportHeight = m_currentGLTransformInfo->GetViewportHeight();

        //Check if mouse is inside viewport
        if( ( mX >= viewportOriginX ) &&
            ( mY >= viewportOriginY ) &&
            ( mX <= viewportOriginX + viewportWidth ) &&
            ( mY <= viewportOriginY + viewportHeight ) )
        {
            sceneManipulator->SetCurrentGLTransformInfo(
                m_currentGLTransformInfo );

            return true;
        }
    }

    return false;
}
#endif //__GADGET_version >= 1003023
////////////////////////////////////////////////////////////////////////////////
osgUtil::LineSegmentIntersector* KeyboardMouse::GetLineSegmentIntersector()
{
    return mLineSegmentIntersector.get();
}
////////////////////////////////////////////////////////////////////////////////
bool KeyboardMouse::CreatePointConstraint()
{
    //Add a point to point constraint for picking
    if( m_physicsSimulator.GetIdle() )
    {
        return false;
    }

    osg::Vec3d startPoint, endPoint;
    SetStartEndPoint( startPoint, endPoint );

    btVector3 rayFromWorld, rayToWorld;
    rayFromWorld.setValue( startPoint.x(), startPoint.y(), startPoint.z() );
    rayToWorld.setValue( endPoint.x(), endPoint.y(), endPoint.z() );

    btCollisionWorld::ClosestRayResultCallback rayCallback(
        rayFromWorld, rayToWorld );
    m_physicsSimulator.GetDynamicsWorld()->rayTest(
        rayFromWorld, rayToWorld, rayCallback );
    if( !rayCallback.hasHit() )
    {
        return false;
    }

    btRigidBody* body = btRigidBody::upcast( rayCallback.m_collisionObject );
    if( !body )
    {
        return false;
    }

    //Other exclusions
    if( !( body->isStaticObject() || body->isKinematicObject() ) )
    {
        mPickedBody = body;
        mPickedBody->setActivationState( DISABLE_DEACTIVATION );

        btVector3 pickPos = rayCallback.m_hitPointWorld;

        btVector3 localPivot =
            body->getCenterOfMassTransform().inverse() * pickPos;

        btPoint2PointConstraint* p2p =
            new btPoint2PointConstraint( *body, localPivot );
        m_physicsSimulator.GetDynamicsWorld()->addConstraint( p2p );
        mPickConstraint = p2p;

        mPrevPhysicsRayPos = ( pickPos - rayFromWorld ).length();

        //Very weak constraint for picking
        p2p->m_setting.m_tau = 0.1;
    }

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdatePointConstraint()
{
    if( !m_physicsSimulator.GetIdle() && mPickConstraint )
    {
        //Move the constraint pivot
        btPoint2PointConstraint* p2p =
            static_cast< btPoint2PointConstraint* >( mPickConstraint );
        if( p2p )
        {
            osg::Vec3d startPoint, endPoint;
            SetStartEndPoint( startPoint, endPoint );

            btVector3 rayFromWorld, rayToWorld;
            rayFromWorld.setValue(
                startPoint.x(), startPoint.y(), startPoint.z() );
            rayToWorld.setValue( endPoint.x(), endPoint.y(), endPoint.z() );

            //Keep it at the same picking distance
            btVector3 dir = rayToWorld - rayFromWorld;
            dir.normalize();
            dir *= mPrevPhysicsRayPos;

            btVector3 newPos = rayFromWorld + dir;
            p2p->setPivotB( newPos );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ClearPointConstraint()
{
    //Do not require mod key depending on what the user did
    if( mPickConstraint )
    {
        m_physicsSimulator.GetDynamicsWorld()->removeConstraint(
            mPickConstraint );
        delete mPickConstraint;
        mPickConstraint = NULL;

        mPickedBody->forceActivationState( ACTIVE_TAG );
        mPickedBody->setDeactivationTime( 0.0 );
        mPickedBody = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetProcessSelection( bool processSelection )
{
    m_processSelection = processSelection;
}
////////////////////////////////////////////////////////////////////////////////
bool KeyboardMouse::GetMousePickEvent()
{
    return m_mousePickEvent;
}
////////////////////////////////////////////////////////////////////////////////
