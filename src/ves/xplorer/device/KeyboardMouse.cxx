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
#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>

#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>
#include <ves/xplorer/scenegraph/nurbs/PointLineSegmentIntersector.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/PhysicsRigidBody.h>
#include <ves/xplorer/scenegraph/physics/CharacterController.h>

#include <ves/xplorer/plugin/PluginBase.h>
#include <ves/xplorer/network/cfdExecutive.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

// --- vrJuggler Includes --- //
#include <vrj/vrjParam.h>

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

#ifdef TRANSFORM_MANIPULATOR
#include <vrj/Display/Display.h>
#include <vrj/Display/Viewport.h>
#include <vrj/Display/Frustum.h>
#include <vrj/Display/Projection.h>
#include <vrj/Display/DisplayManager.h>
#endif //TRANSFORM_MANIPULATOR

// --- OSG Includes --- //
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>

//#include <osg/PolygonStipple>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace ves::xplorer::device;

namespace vx = ves::xplorer;
namespace vxs = vx::scenegraph;
namespace vxsm = vxs::manipulator;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
    :
    mKeyNone( false ),
    mKeyShift( false ),
    mKeyAlt( false ),
    
    mKey( -1 ),
    mButton( -1 ),
    mState( 0 ),
    mX( 0 ),
    mY( 0 ),

    mWidth( 1 ),
    mHeight( 1 ),

    mAspectRatio( 0.0 ),
    mFoVZ( 0.0 ),

    mLeftFrustum( 0.0 ),
    mRightFrustum( 0.0 ),
    mTopFrustum( 0.0 ),
    mBottomFrustum( 0.0 ),
    mNearFrustum( 0.0 ),
    mFarFrustum( 0.0 ),

    mXMinScreen( 0.0 ),
    mXMaxScreen( 0.0 ),
    mYMinScreen( 0.0 ),
    mYMaxScreen( 0.0 ),
    mZValScreen( 0.0 ),

    mPrevPhysicsRayPos( 0.0 ),

    mMagnitude( 0.0 ),

    mCurrPos( 0, 0 ),
    mPrevPos( 0, 0 ),

    mDeltaRotation( 0.0, 0.0, 0.0, 1.0 ),
    mDeltaTranslation( 0.0, 0.0, 0.0, 1.0 ),

    mLineSegmentIntersector(
        new osgUtil::LineSegmentIntersector(
            osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),

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
void KeyboardMouse::UpdateNavigation()
{
    ProcessKBEvents( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelection()
{
    ProcessKBEvents( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetStartEndPoint(
    osg::Vec3d& startPoint, osg::Vec3d& endPoint )
{
#ifdef TRANSFORM_MANIPULATOR
    //Get the displays
    const std::vector< vrj::DisplayPtr >& displayVector =
        vrj::DisplayManager::instance()->getActiveDisplays();

    //Iterate over the displays
    std::vector< vrj::DisplayPtr >::const_iterator itr = displayVector.begin();
    for( itr; itr != displayVector.end(); ++itr )
    {
        //Get actual dimensions of display
        int xOriginDisplay, yOriginDisplay, widthDisplay, heightDisplay;
        (*itr)->getOriginAndSize(
            xOriginDisplay, yOriginDisplay, widthDisplay, heightDisplay );

        //Iterate over the viewports
        for( size_t i = 0; i < (*itr)->getNumViewports(); ++i )
        {
#if __VJ_version >= 2003000
            vrj::ViewportPtr viewport = (*itr)->getViewport( i );
#else
            vrj::Viewport* viewport = (*itr)->getViewport( i );
#endif
            //Get normalized dimensions of viewport
            float xOriginViewport, yOriginViewport, widthViewport, heightViewport;
            viewport->getOriginAndSize(
                xOriginViewport, yOriginViewport,
                widthViewport, heightViewport );

            //Get dimensions of viewport in pixels
            const size_t xOriginViewportInPixels =
                static_cast< size_t >( xOriginViewport * widthDisplay );
            const size_t yOriginViewportInPixels =
                static_cast< size_t >( yOriginViewport * heightDisplay );
            const size_t widthViewportInPixels =
                static_cast< size_t >( widthViewport * widthDisplay );
            const size_t heightViewportInPixels =
                static_cast< size_t >( heightViewport * heightDisplay );

            //Check if mouse is inside viewport
            if( ( mX >= xOriginViewportInPixels ) &&
                ( mY >= yOriginViewportInPixels ) &&
                ( mX <= xOriginViewportInPixels + widthViewportInPixels ) &&
                ( mY <= yOriginViewportInPixels + heightViewportInPixels ) )
            {
                //Make the coordinates relative to viewport
                mX -= xOriginViewportInPixels;
                mY -= yOriginViewportInPixels;

                //Convert relative screen coords
                //mCurrPos.first = 
                    //static_cast< double >( mX ) /
                    //static_cast< double >( widthViewportInPixels );
                //mCurrPos.second =
                    //static_cast< double >( mY ) /
                    //static_cast< double >( heightViewportInPixels );
                
                //The view for the active viewport
                vrj::Viewport::View view = viewport->getView();
#if __VJ_version >= 2003000
                vrj::ProjectionPtr project;
#else
                vrj::Projection* project;
#endif
                if( ( vrj::Viewport::STEREO == view ) ||
                    ( vrj::Viewport::LEFT_EYE == view ) )
                {
                    project = viewport->getLeftProj();
                }

                if( ( vrj::Viewport::STEREO == view ) ||
                    ( vrj::Viewport::RIGHT_EYE == view ) )
                {
                    project = viewport->getRightProj();
                }

                //Calculate the window matrix
                gmtl::Matrix44d windowMatrix;
                windowMatrix.mState =
                    gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
                windowMatrix.mData[  0 ] = 0.5 * widthViewportInPixels;
                windowMatrix.mData[  5 ] = 0.5 * heightViewportInPixels;
                windowMatrix.mData[ 10 ] = 0.5;
                windowMatrix.mData[ 12 ] =
                    ( 0.5 * widthViewportInPixels ) + xOriginViewportInPixels;
                windowMatrix.mData[ 13 ] =
                    ( 0.5 * heightViewportInPixels ) + yOriginViewportInPixels;
                windowMatrix.mData[ 14 ] = 0.5;

                //std::cout << "windowMatrix: " << std::endl << windowMatrix << std::endl << std::endl;

                //Calculate the projecton matrix
                gmtl::Matrix44d projectionMatrix;
                projectionMatrix.mState =
                    gmtl::Matrix44d::AFFINE | gmtl::Matrix44d::NON_UNISCALE;
                projectionMatrix.mData[  0 ] =
                    ( 2.0 * mNearFrustum ) /
                    ( mRightFrustum - mLeftFrustum );
                projectionMatrix.mData[  5 ] =
                    ( 2.0 * mNearFrustum ) /
                    ( mTopFrustum - mBottomFrustum );
                projectionMatrix.mData[  8 ] =
                    ( mRightFrustum + mLeftFrustum ) /
                    ( mRightFrustum - mLeftFrustum );
                projectionMatrix.mData[  9 ] =
                    ( mTopFrustum + mBottomFrustum ) /
                    ( mTopFrustum - mBottomFrustum );
                projectionMatrix.mData[ 10 ] =
                    -1.0 * ( mFarFrustum + mNearFrustum ) /
                           ( mFarFrustum - mNearFrustum );
                projectionMatrix.mData[ 11 ] = -1.0;
                projectionMatrix.mData[ 14 ] =
                    ( -2.0 * mFarFrustum * mNearFrustum ) /
                           ( mFarFrustum - mNearFrustum );
                projectionMatrix.mData[ 15 ] =  0.0;

                //std::cout << "projectionMatrix: " << std::endl << projectionMatrix << std::endl << std::endl;

                //Calculate the view matrix
                gmtl::Matrix44d viewMatrix =
                    gmtl::convertTo< double >( project->getViewMatrix() );

                //Transform view matrix into z up land
                gmtl::Vec3d x_axis( 1.0, 0.0, 0.0 );
                gmtl::Matrix44d zUp = gmtl::makeRot< gmtl::Matrix44d >( 
                    gmtl::AxisAngled( -gmtl::Math::PI_OVER_2, x_axis ) );
                viewMatrix *= zUp;
                
                //std::cout << "viewMatrix: " << std::endl << viewMatrix << std::endl << std::endl;

                //Calculate the MVPW matrix
                gmtl::Matrix44d MVPW =
                    //mul by inverse - normalized device coordinates( -1 to 1 )
                    windowMatrix *
                    //mul by inverse - clip coordinates
                    projectionMatrix *
                    //mul by inverse - eye coordinates
                    viewMatrix *
                    //mul by inverse - object coordinates
                    vxs::SceneManager::instance()->GetActiveNavSwitchNode()->GetMat();

                //std::cout << "MVPW: " << std::endl << MVPW << std::endl << std::endl;

                //Calculate the inverse MVPW matrix
                gmtl::Matrix44d inverseMVPW = gmtl::invert( MVPW );

                //std::cout << "inverseMVPW: " << std::endl << inverseMVPW << std::endl << std::endl;

                //Now get the inverse
                osg::Matrix osgInverseMVPW( inverseMVPW.getData() );
                startPoint = osg::Vec3d( mX, mY, 0.0 ) * osgInverseMVPW;
                endPoint = osg::Vec3d( mX, mY, 1.0 ) * osgInverseMVPW;

                /*
                std::cout << "near_point: "
                          << "( " << startPoint.x()
                          << ", " << startPoint.y()
                          << ", " << startPoint.z()
                          << " )" << std::endl;

                std::cout << "far_point: "
                          << "( " << endPoint.x()
                          << ", " << endPoint.y()
                          << ", " << endPoint.z()
                          << " )" << std::endl;
                */

                //return;
            }
        }
    }
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
            vxs::SceneManager::instance()->GetInvertedWorldDCS().getData() );
        
        startPoint = startPoint * inverseCameraTransform;
        endPoint = endPoint * inverseCameraTransform;
    }

    /*
    std::cout << "startPoint: "
              << "( " << startPoint.x()
              << ", " << startPoint.y()
              << ", " << startPoint.z()
              << " )" << std::endl;

    std::cout << "endPoint: "
              << "( " << endPoint.x()
              << ", " << endPoint.y()
              << ", " << endPoint.z()
              << " )" << std::endl;
    */
#endif //TRANSFORM_MANIPULATOR
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{   
    osg::Group* rootNode =
        vxs::SceneManager::instance()->GetRootNode();

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
void KeyboardMouse::ProcessKBEvents( int mode )
{
    //Get the event queue
    gadget::KeyboardMouse::EventQueue evt_queue =
        mKeyboardMouse->getEventQueue();

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
    gadget::KeyboardMouse::EventQueue::iterator i;
    for( i = evt_queue.begin(); i != evt_queue.end(); ++i )
    {
        const gadget::EventType type = ( *i )->type();

        switch( type )
        {
            case gadget::KeyPressEvent:
            {
                gadget::KeyEventPtr keyEvt =
                    boost::dynamic_pointer_cast< gadget::KeyEvent >( *i );

                mKey = keyEvt->getKey();

                //Navigation mode
                if( mode == 0 )
                {
                    NavOnKeyboardPress();
                }
                //Selection mode
                else if( mode == 1 )
                {
                    SelOnKeyboardPress();
                }

                break;
            }
            case gadget::KeyReleaseEvent:
            {
                gadget::KeyEventPtr keyEvt =
                    boost::dynamic_pointer_cast< gadget::KeyEvent >( *i );

                mKey = keyEvt->getKey();

                //Navigation mode
                if( mode == 0 )
                {
                    NavOnKeyboardRelease();
                }

                break;
            }
            case gadget::MouseButtonPressEvent:
            {
                gadget::MouseEventPtr mouse_evt =
                    boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

                mButton = mouse_evt->getButton();
                mState = 1;
                mX = mouse_evt->getX();
                mY = mouse_evt->getY();

                mCurrPos.first =
                    static_cast< double >( mX ) /
                    static_cast< double >( mWidth );
                mCurrPos.second =
                    static_cast< double >( mY ) /
                    static_cast< double >( mHeight );

                //Navigation mode
                if( mode == 0 )
                {
                    NavOnMousePress();
                }
                //Selection mode
                else if( mode == 1 )
                {
                    SelOnMousePress();
                }

                mPrevPos.first = mCurrPos.first;
                mPrevPos.second = mCurrPos.second;

                break;
            }
            case gadget::MouseButtonReleaseEvent:
            {
                gadget::MouseEventPtr mouse_evt =
                    boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

                mButton = mouse_evt->getButton();
                mState = 0;
                mX = mouse_evt->getX();
                mY = mouse_evt->getY();

                mCurrPos.first =
                    static_cast< double >( mX ) /
                    static_cast< double >( mWidth );
                mCurrPos.second =
                    static_cast< double >( mY ) /
                    static_cast< double >( mHeight );

                //Navigation mode
                if( mode == 0 )
                {
                    NavOnMouseRelease();
                }
                //Selection mode
                else if( mode == 1 )
                {
                    //We process selection on the release of the left button
                    //because in the future we would like to be able to select
                    //with a rubber band rectangle which would mean the mouse
                    //down would be the first point of the rectangle and the
                    //mouse up would be the second point
                    SelOnMouseRelease();
                }

                mPrevPos.first = mCurrPos.first;
                mPrevPos.second = mCurrPos.second;

                break;
            }
            case gadget::MouseMoveEvent:
            {
                gadget::MouseEventPtr mouse_evt =
                    boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

                mX = mouse_evt->getX();
                mY = mouse_evt->getY();
                
                if( mState == 0 )
                {
#ifdef TRANSFORM_MANIPULATOR
                    UpdateSelectionLine();

                    if(
                    vxs::SceneManager::instance()->GetManipulatorManager()->Handle(
                        vxsm::Event::FOCUS, mLineSegmentIntersector.get() ) )
                    {
                        break;
                    }
#endif //TRANSFORM_MANIPULATOR
                }
                else if( mState == 1 )
                {
                    mCurrPos.first =
                        static_cast< double >( mX ) /
                        static_cast< double >( mWidth );
                    mCurrPos.second =
                        static_cast< double >( mY ) /
                        static_cast< double >( mHeight );

                    std::pair< double, double > delta;
                    delta.first = mCurrPos.first - mPrevPos.first;
                    delta.second = mCurrPos.second - mPrevPos.second;

                    //Navigation mode
                    if( mode == 0 )
                    {
                        NavOnMouseMotion( delta );
                    }
                    //Selection mode
                    else if( mode == 1 )
                    {
                        SelOnMouseMotion( delta );
                    }

                    mPrevPos.first = mCurrPos.first;
                    mPrevPos.second = mCurrPos.second;
                }

                break;
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNavigationEvents()
{
    gmtl::Matrix44d newTransform;
    gmtl::Matrix44d currentTransform;
    
    vxs::DCS* const activeDCS =
        vx::DeviceHandler::instance()->GetActiveDCS();
    //Get the node where are all the geometry is handled
    osg::Group* const activeSwitchNode =
        vxs::SceneManager::instance()->GetActiveSwitchNode();
    //Get the node where all the nav matrix's are handled
    vxs::DCS* const cameraDCS =
        vxs::SceneManager::instance()->GetActiveNavSwitchNode();

    osg::ref_ptr< vxs::CoordinateSystemTransform >
        coordinateSystemTransform;

    //Test if we are manipulating the camera dcs or a model dcs
    if( activeDCS->GetName() != cameraDCS->GetName() )
    {
        //If local dcs, transform to camera space
        coordinateSystemTransform = new vxs::CoordinateSystemTransform(
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
    mDeltaTranslation.set( 0.0, 0.0, 0.0, 1.0 );
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
    osg::Group* activeSwitchNode =
        vxs::SceneManager::instance()->GetActiveSwitchNode();
    vxs::DCS* activeNavSwitchNode =
        vxs::SceneManager::instance()->GetActiveNavSwitchNode();
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
    vx::DeviceHandler::instance()->UnselectObjects();

    //gmtl::Matrix44d matrix;
    //mCenterPoint->mData[ 1 ] = matrix[ 1 ][ 3 ] = *mCenterPointThreshold;
    //vxs::SceneManager::instance()->GetActiveSwitchNode()->SetMat( matrix );
    
    //reset view
    vxs::SceneManager::instance()->GetWorldDCS()->SetQuat( *mResetAxis );
    vxs::SceneManager::instance()->GetWorldDCS()->SetTranslationArray(
        *mResetPosition );
    
    //Grab the current matrix
    osg::ref_ptr< vxs::DCS > activeSwitchDCS =
        vxs::SceneManager::instance()->GetWorldDCS();

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
    ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavOnKeyboardPress()
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
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StrafeLeft( true );
            }

            break;
        }
        case gadget::KEY_S:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StepBackward( true );
            }

            break;
        }
        case gadget::KEY_W:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StepForward( true );
            }

            break;
        }
        case gadget::KEY_D:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StrafeRight( true );
            }

            break;
        }
        case gadget::KEY_C:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                //mCharacterController
            }

            break;
        }
        case gadget::KEY_SPACE:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->Jump();
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
            ProcessNavigationEvents();

            break;
        }
        case gadget::KEY_DOWN: 
        {
            Zoom45( -0.05 );
            ProcessNavigationEvents();

            break;
        }
        case gadget::KEY_LEFT:
        {
            Pan( 0.05, 0 );
            ProcessNavigationEvents();

            break;
        }
        case gadget::KEY_RIGHT:
        {
            Pan( -0.05, 0 );
            ProcessNavigationEvents();

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavOnKeyboardRelease()
{
    switch( mKey )
    {
        case gadget::KEY_A:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StrafeLeft( false );
            }

            break;
        }
        case gadget::KEY_S:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StepBackward( false );
            }

            break;
        }
        case gadget::KEY_D:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StrafeRight( false );
            }

            break;
        }
        case gadget::KEY_W:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->StepForward( false );
            }

            break;
        }
        case gadget::KEY_SPACE:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                //mCharacterController->Jump();
            }

            break;
        }
        case gadget::KEY_C:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                mCharacterController->IsActive() )
            {
                //mCharacterController
            }

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavOnMousePress()
{
    //Would be cool to change mouse cursor for nav events here
    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            //Rotate just the camera "3rd person view:
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                 mCharacterController->IsActive() )
            {
                mCharacterController->FirstPersonMode( false );
            }

            //No modifier key
            if( mKeyNone )
            {
#ifdef TRANSFORM_MANIPULATOR
                UpdateSelectionLine();

                if( vxs::SceneManager::instance()->GetManipulatorManager()->Handle(
                        vxsm::Event::PUSH, mLineSegmentIntersector.get() ) )
                {
                    break;
                }
#endif //TRANSFORM_MANIPULATOR

                if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                     mCharacterController->IsActive() )
                {
                    mCharacterController->SetCameraRotationSLERP( false );
                }
            }
            //Mod key shift
            else if( mKeyShift )
            {
                //Add a point to point constraint for picking
                if( vxs::PhysicsSimulator::instance()->GetIdle() )
                {
                    break;
                }
                
                osg::Vec3d startPoint, endPoint;
                SetStartEndPoint( startPoint, endPoint );

                btVector3 rayFromWorld, rayToWorld;
                rayFromWorld.setValue(
                    startPoint.x(), startPoint.y(), startPoint.z() );
                rayToWorld.setValue(
                    endPoint.x(), endPoint.y(), endPoint.z() );

                btCollisionWorld::ClosestRayResultCallback rayCallback(
                    rayFromWorld, rayToWorld );
                vxs::PhysicsSimulator::instance()->GetDynamicsWorld()->rayTest(
                    rayFromWorld, rayToWorld, rayCallback );
                
                if( !rayCallback.hasHit() )
                {
                    break;
                }
                
                btRigidBody* body = btRigidBody::upcast(
                    rayCallback.m_collisionObject );
                if( !body )
                {
                    break;
                }

                //Other exclusions
                if( !( body->isStaticObject() ||
                       body->isKinematicObject() ) )
                {
                    mPickedBody = body;
                    mPickedBody->setActivationState(
                        DISABLE_DEACTIVATION );

                    btVector3 pickPos = rayCallback.m_hitPointWorld;

                    btVector3 localPivot =
                        body->getCenterOfMassTransform().inverse() *
                        pickPos;

                    btPoint2PointConstraint* p2p =
                        new btPoint2PointConstraint(
                            *body, localPivot );
                    vxs::PhysicsSimulator::instance()->GetDynamicsWorld()->addConstraint( p2p );
                    mPickConstraint = p2p;

                    mPrevPhysicsRayPos =
                        ( pickPos - rayFromWorld ).length();

                    //Very weak constraint for picking
                    p2p->m_setting.m_tau = 0.1;
                }
            }
            else if( mKeyAlt )
            {
                SelOnMouseRelease();
                vxs::DCS* infoDCS = 
                    vx::DeviceHandler::instance()->GetSelectedDCS();
                vx::DeviceHandler::instance()->UnselectObjects();
                
                std::map< std::string, ves::xplorer::plugin::PluginBase* >* 
                    tempPlugins = 
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
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                mCharacterController->IsActive() )
            {
                mCharacterController->FirstPersonMode( true );
                mCharacterController->SetCameraRotationSLERP( false );
                mCharacterController->SetCharacterRotationFromCamera();
            }

            break;
        }
        //Scroll wheel up
        case gadget::MBUTTON4:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                mCharacterController->IsActive() )
            {
                mCharacterController->Zoom( true );
            }

            break;
        }
        //Scroll wheel down
        case gadget::MBUTTON5:
        {
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                mCharacterController->IsActive() )
            {
                mCharacterController->Zoom( false );
            }

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavOnMouseRelease()
{
    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            //Do not require mod key depending on what the user did
            if( mPickConstraint )
            {
                vxs::PhysicsSimulator::instance()->GetDynamicsWorld()->removeConstraint( mPickConstraint );
                delete mPickConstraint;
                mPickConstraint = NULL;

                mPickedBody->forceActivationState( ACTIVE_TAG );
                mPickedBody->setDeactivationTime( 0.0 );
                mPickedBody = NULL;
            }

            //No modifier key
            if( mKeyNone )
            {
#ifdef TRANSFORM_MANIPULATOR
                if( vxs::SceneManager::instance()->GetManipulatorManager()->Handle(
                        vxsm::Event::RELEASE ) );
                {
                    break;
                }
#endif //TRANSFORM_MANIPULATOR

                if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                     mCharacterController->IsActive() )
                {
                    mCharacterController->SetCameraRotationSLERP( true );
                }
            }
            //Mod key shift
            else if( mKeyShift )
            {
                ;
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
void KeyboardMouse::NavOnMouseMotion( std::pair< double, double > delta )
{
    mMagnitude =
        sqrtf( delta.first * delta.first + delta.second * delta.second );

    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            //No modifier key
            if( mKeyNone )
            {

#ifdef TRANSFORM_MANIPULATOR
                UpdateSelectionLine();

                if( vxs::SceneManager::instance()->GetManipulatorManager()->Handle(
                        vxsm::Event::DRAG ) )
                {
                    break;
                }
#endif //TRANSFORM_MANIPULATOR

                //Rotate just the camera "3rd person view:
                if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                     mCharacterController->IsActive() )
                {
                    mCharacterController->Rotate( delta.first, delta.second );
                }
                else
                {
                    if( ( mX > 0.1 * mWidth ) && ( mX < 0.9 * mWidth ) &&
                        ( mY > 0.1 * mHeight ) && ( mY < 0.9 * mHeight ) )
                    {
                        double angle = mMagnitude * 7.0;
#if __VJ_version >= 2003000
                        Rotate(
                            angle, gmtl::Vec3d( -delta.second, 0.0, delta.first ) );
#else
                        Rotate(
                            angle, gmtl::Vec3d(  delta.second, 0.0, delta.first ) );
#endif
                    }
                    else
                    {
                        Twist();
                    }

                    ProcessNavigationEvents();
                }
            }
            //Mod key shift
            else if( mKeyShift )
            {
                if( !vxs::PhysicsSimulator::instance()->GetIdle() && mPickConstraint )
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
                        rayToWorld.setValue(
                            endPoint.x(), endPoint.y(), endPoint.z() );

                        //Keep it at the same picking distance
                        btVector3 dir = rayToWorld - rayFromWorld;
                        dir.normalize();
                        dir *= mPrevPhysicsRayPos;

                        btVector3 newPos = rayFromWorld + dir;
                        p2p->setPivotB( newPos );
                    }
                }
            }

            break;
        }
        //Middle mouse button
        case gadget::MBUTTON2:
        {
            Pan( delta.first, delta.second );
            ProcessNavigationEvents();

            break;
        }
        //Right mouse button
        case gadget::MBUTTON3:
        {
            //Rotate the character and camera at the same time
            if( !vxs::PhysicsSimulator::instance()->GetIdle() &&
                mCharacterController->IsActive() )
            {
                mCharacterController->Rotate( delta.first, delta.second );
            }
            else
            {
                Zoom( delta.second );
                ProcessNavigationEvents();
            }

            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnKeyboardPress()
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnMousePress()
{
#ifndef TRANSFORM_MANIPULATOR
    UpdateSelectionLine();
#endif //TRANSFORM_MANIPULATOR

    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            if( mKeyNone )
            {
                ProcessNURBSSelectionEvents();
            }

            ProcessSelectionEvents();

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
#ifndef TRANSFORM_MANIPULATOR
    UpdateSelectionLine();
#endif //TRANSFORM_MANIPULATOR

    switch( mButton )
    {
        //Left mouse button
        case gadget::MBUTTON1:
        {
            if( mKeyNone )
            {
                vxs::SetStateOnNURBSNodeVisitor(
                    vxs::SceneManager::instance()->GetActiveSwitchNode(), false,
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
            vxs::SetStateOnNURBSNodeVisitor(
                vxs::SceneManager::instance()->GetActiveSwitchNode(),
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
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    vx::DeviceHandler::instance()->ResetCenterPoint();

    gmtl::Matrix44d matrix;
    gmtl::identity( matrix );
    vxs::SceneManager::instance()->
        GetWorldDCS()->SetMat( matrix );
    
    vxs::SceneManager::instance()->GetWorldDCS()->SetQuat( *mResetAxis );
    vxs::SceneManager::instance()->GetWorldDCS()->SetTranslationArray(
        *mResetPosition );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist()
{
    double theta = atan2( mPrevPos.first - 0.5, mPrevPos.second - 0.5 );
    double newTheta = atan2( mCurrPos.first - 0.5, mCurrPos.second - 0.5 );
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
        vxs::DCS* const selectedDCS =
            vx::DeviceHandler::instance()->GetSelectedDCS();
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
        vxs::DCS* const selectedDCS =
            vx::DeviceHandler::instance()->GetSelectedDCS();
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
void KeyboardMouse::ProcessNURBSSelectionEvents()
{
    osg::ref_ptr< osgUtil::IntersectorGroup > intersectorGroup =
        new osgUtil::IntersectorGroup();
    osg::ref_ptr< vxs::nurbs::PointLineSegmentIntersector > intersector =
        new vxs::nurbs::PointLineSegmentIntersector(
            mLineSegmentIntersector->getStart(),
            mLineSegmentIntersector->getEnd() );
    intersectorGroup->addIntersector( intersector.get() );

    osgUtil::IntersectionVisitor controlMeshPointIntersectVisitor;

    controlMeshPointIntersectVisitor.setIntersector( intersectorGroup.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    vxs::SceneManager::instance()->GetRootNode()->accept(
        controlMeshPointIntersectVisitor );

    if( intersectorGroup->containsIntersections() )
    {
         //std::cout<<"Found intersections "<<std::endl;
         ///only want the first one
         vxs::nurbs::PointLineSegmentIntersector::Intersections& intersections =
             intersector->getIntersections();
         vxs::nurbs::PointLineSegmentIntersector::Intersection closestControlPoint =
             (*intersections.begin());
         osg::ref_ptr<vxs::nurbs::NURBSControlMesh> ctMesh =
            dynamic_cast< vxs::nurbs::NURBSControlMesh* >(
                closestControlPoint.drawable.get() );
         if( ctMesh.valid() )
         {
             osg::ref_ptr<vxs::nurbs::NURBS> nurbs = 
                dynamic_cast<vxs::nurbs::NURBS*>( ctMesh->getParent( 0 ) );
             if( nurbs.valid() )
             {
                 nurbs->SetSelectedControlPoint(
                     closestControlPoint.primitiveIndex );
             }
         }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessSelectionEvents()
{
    osgUtil::IntersectionVisitor intersectionVisitor(
        mLineSegmentIntersector.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    vxs::SceneManager::instance()->GetModelRoot()->accept( intersectionVisitor );

    ProcessHit();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessHit()
{
    //Unselect the previous selected DCS
    vx::DeviceHandler::instance()->UnselectObjects();

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
    vxs::FindParentsVisitor parentVisitor( objectHit );
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

    vxs::DCS* newSelectedDCS =
        static_cast< vxs::DCS* >( parentNode.get() );
    newSelectedDCS->SetTechnique( "Select" );
    vx::DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );

    //Move the center point to the center of the selected object
    osg::ref_ptr< vxs::CoordinateSystemTransform > cst =
        new vxs::CoordinateSystemTransform(
            vxs::SceneManager::instance()->GetActiveSwitchNode(),
            newSelectedDCS, true );
    gmtl::Matrix44d localToWorldMatrix =
        cst->GetTransformationMatrix( false );

    //Multiplying by the new local matrix mCenterPoint
    osg::Matrixd tempMatrix;
    tempMatrix.set( localToWorldMatrix.getData() );
    osg::Vec3d center = newSelectedDCS->getBound().center() * tempMatrix;
    mCenterPoint->set( center.x(), center.y(), center.z() );
}
////////////////////////////////////////////////////////////////////////////////
gadget::KeyboardMousePtr KeyboardMouse::GetKeyboardMouseVRJDevice()
{
    return mKeyboardMouse->getKeyboardMousePtr();
}
////////////////////////////////////////////////////////////////////////////////
