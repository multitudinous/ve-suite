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

#include <ves/open/xml/Command.h>

#include <ves/xplorer/Debug.h>
#include <ves/xplorer/Model.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/environment/NavigationAnimationEngine.h>

#include <ves/xplorer/scenegraph/CoordinateSystemTransform.h>
#include <ves/xplorer/scenegraph/SetStateOnNURBSNodeVisitor.h>
#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/FindParentsVisitor.h>
#include <ves/xplorer/scenegraph/Group.h>

#include <ves/xplorer/scenegraph/nurbs/PointLineSegmentIntersector.h>
#include <ves/xplorer/scenegraph/nurbs/NURBS.h>
#include <ves/xplorer/scenegraph/nurbs/NURBSControlMesh.h>
#include <ves/xplorer/scenegraph/nurbs/ControlPoint.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

// --- vrJuggler Includes --- //
#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>
#include <gmtl/Quat.h>
#include <gmtl/gmtl.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- OSG Includes --- //
#include <osg/Array>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/LineSegment>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>
#include <osg/Texture2D>
#include <osg/CameraNode>
#include <osg/Shape>
#include <osg/ShapeDrawable>
#include <osg/Quat>

//#include <osg/PolygonStipple>
#include <osgUtil/LineSegmentIntersector>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace ves::xplorer;

namespace vxs = ves::xplorer::scenegraph;

const double OneEightyDivPI = 57.29577951;
const double PIDivOneEighty = 0.0174532925;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
    :
    mKey( -1 ),
    mButton( -1 ),
    mState( 0 ),
    mX( 0 ),
    mY( 0 ),

    mWidth( 1 ),
    mHeight( 1 ),

    mAspectRatio( 0.0f ),
    mFoVY( 0.0f ),
    mLeftFrustum( 0.0f ),
    mRightFrustum( 0.0f ),
    mTopFrustum( 0.0f ),
    mBottomFrustum( 0.0f ),
    mNearFrustum( 0.0f ),
    mFarFrustum( 0.0f ),

    mXMinScreen( 0.0f ),
    mXMaxScreen( 0.0f ),
    mYMinScreen( 0.0f ),
    mYMaxScreen( 0.0f ),
    mZValScreen( 0.0f ),

    mMagnitude( 0.0f ),
    mSensitivity( 1.0e-06 ),

    mCurrPos( 0, 0 ),
    mPrevPos( 0, 0 ),

    mAnimate( false ),

    mBeamLineSegment( new osg::LineSegment )
{
    mKeyboard.init( "VJKeyboard" );
    mHead.init( "VJHead" );

    gmtl::identity( mDeltaTransform );
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
    osg::Vec3d* startPoint, osg::Vec3d* endPoint )
{
    //Be sure mWidth and mHeight are set before calling this function
    double wc_x_trans_ratio = ( mXMaxScreen - mXMinScreen ) /
                              static_cast< double >( mWidth );
    double wc_y_trans_ratio = ( mYMaxScreen - mYMinScreen ) /
                              static_cast< double >( mHeight );

    std::pair< double, double > screenRatios =
        std::pair< double, double >( wc_x_trans_ratio, wc_y_trans_ratio );

    //Get the mouse position in juggler world coordinates
    gmtl::Point3d transformedPosition;
    transformedPosition[ 0 ] = mXMinScreen + ( mX * screenRatios.first );
    transformedPosition[ 1 ] = mYMaxScreen - ( mY * screenRatios.second );
    transformedPosition[ 2 ] = mZValScreen;
    //Convert meters to feet
    transformedPosition *= 3.2808399;

    //Get the mouse position in osg world coordinates
    gmtl::Point3d mousePosition;
    mousePosition[ 0 ] =  transformedPosition[ 0 ];
    mousePosition[ 1 ] = -transformedPosition[ 2 ];
    mousePosition[ 2 ] =  transformedPosition[ 1 ];

  /*std::cout << " mX location = " << mX << std::endl 
              << " mY location = " << mY << std::endl 
              << " location = " << osgTransformedPosition[ 0 ]
              << " " << osgTransformedPosition[ 1 ] << " "
              << osgTransformedPosition[ 2 ] << std::endl
              << " ratio = " << screenRatios.first << " "
              << screenRatios.second << std::endl
              << " screen values = " << mXMinScreen << " "
              << mYMinScreen << " " << mZValScreen << std::endl
              << std::endl;

    std::cout << " mX " << mXMinScreen << " " << mXMaxScreen 
              << " mY " << mYMinScreen << " " << mYMaxScreen 
              << " z " << mZValScreen 
              << " mWidth " << mWidth << " mHeight "
              << mHeight << std::endl;*/

    //Get juggler Matrix of worldDCS
    //Note:: for pf we are in juggler land
    //       for osg we are in z up land
    gmtl::Matrix44d vjHeadMat = gmtl::convertTo< double >( mHead->getData() );
    gmtl::Point3d jugglerHeadPoint =
        gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

    //We have to offset negative mX because
    //the view and frustum are drawn for the left eye
    gmtl::Point3d jugglerHeadPointTemp;
    jugglerHeadPointTemp[ 0 ] =  jugglerHeadPoint[ 0 ] - ( 0.0345 * 3.2808399 );
    jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[ 2 ];
    jugglerHeadPointTemp[ 2 ] =  jugglerHeadPoint[ 1 ];

    //Set the start point
    startPoint->set( jugglerHeadPointTemp[ 0 ],
                     jugglerHeadPointTemp[ 1 ],
                     jugglerHeadPointTemp[ 2 ] );

    std::cout << " start point "
              << jugglerHeadPointTemp[ 0 ] << " "
              << jugglerHeadPointTemp[ 1 ] << " "
              << jugglerHeadPointTemp[ 2 ] << std::endl;

    //Get the vector
    gmtl::Vec3d vjVecNear = mousePosition - jugglerHeadPointTemp;
    gmtl::Vec3d vjVecFar = -jugglerHeadPointTemp;
    vjVecFar.mData[ 1 ] = mFarFrustum + vjVecFar.mData[ 1 ];

    double distance = vjVecFar.mData[ 1 ] / vjVecNear.mData[ 1 ];
    //Set the end point
    endPoint->set(
        jugglerHeadPointTemp.mData[ 0 ] + ( vjVecNear.mData[ 0 ] * distance ),
        mFarFrustum,
        jugglerHeadPointTemp.mData[ 2 ] + ( vjVecNear.mData[ 2 ] * distance ) );

    std::cout << " end point " 
              << endPoint->x() << " "
              << endPoint->y() << " "
              << endPoint->z() << std::endl;

    //Need to negate the the camera transform that is multiplied into the view
    {
        osg::Matrixd inverseCameraTransform =
            osg::Matrixd( vxs::SceneManager::instance()->
            GetInvertedWorldDCS().getData() );
        
        *startPoint = *startPoint * inverseCameraTransform;
        *endPoint = *endPoint * inverseCameraTransform;
    }
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
    osg::ref_ptr< osg::StateSet > m_stateset = new osg::StateSet();

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 1.0f, 0.0f, 0.0f, 1.0f ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > line_width = new osg::LineWidth();
    line_width->setWidth( 2.0f );
    m_stateset->setAttribute( line_width.get() );
    line->setStateSet( m_stateset.get() );

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
    gadget::KeyboardMouse::EventQueue evt_queue = mKeyboard->getEventQueue();
    gadget::KeyboardMouse::EventQueue::iterator i;

    if( evt_queue.empty() )
    {
        return;
    }

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
                //Use this call if you want to hold a mKey for it to be active
                mKey = -1;

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

                    //If animation mode, stop the animation with mouse press
                    if( mAnimate )
                    {
                        gmtl::identity( mDeltaTransform );
                        mDeltaTransform.mData[ 12 ] =
                        mDeltaTransform.mData[ 13 ] =
                        mDeltaTransform.mData[ 14 ] = 0.0f;
                    }
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

                if( mState == 1 )
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
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();
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
    newTransform = mDeltaTransform * newTransform;

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

    //If not in animation mode, reset the transform
    if( !mAnimate )
    {
        gmtl::identity( mDeltaTransform );
        mDeltaTransform.mData[ 12 ] =
        mDeltaTransform.mData[ 13 ] =
        mDeltaTransform.mData[ 14 ] = 0.0f;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Animate( bool animate )
{
    mAnimate = animate;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetWindowValues( unsigned int w, unsigned int h )
{
    mWidth = w;
    mHeight = h;

    mAspectRatio = static_cast< double >( mWidth ) /
                   static_cast< double >( mHeight );
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

    double topAngle = OneEightyDivPI * atan( mTopFrustum / mNearFrustum );
    double tempDiv = fabs( mBottomFrustum ) / mNearFrustum;
    double bottomAngle = OneEightyDivPI * atan( tempDiv );

    mFoVY = topAngle + bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
    //Grab the current switch node's matrix from SceneManager
    //We want to manipulate the translation of the matrix until
    //all geometry is framed within the current viewing frustum
    osg::ref_ptr< vxs::DCS > worldDCS =
        vxs::SceneManager::instance()->GetActiveNavSwitchNode();
    gmtl::Matrix44d matrix = worldDCS->GetMat();

    //////////////////////////////////////////////////////////////////////
    //Now we need to find the center of the viewing frustum in osg coordinates
    //////////////////////////////////////////////////////////////////////
    //Find the vector which goes down the center of the viewing frustum
    osg::Vec3d frustumCenterVector( 0, 0, 0 );
    osg::Vec3d startPoint( 0, 0, 0 );
    osg::Vec3d endPoint( 0, 0, 0 );

    //Set the start point to the juggler head position in osg coordinates
    gmtl::Matrix44d vjHeadMatrix = gmtl::convertTo< double >( mHead->getData() );
    gmtl::Point3d vjHeadPosition =
        gmtl::makeTrans< gmtl::Point3d >( vjHeadMatrix );
    //We have to offset negative mX because
    //the view and frustum are drawn for the left eye
    startPoint[ 0 ] =  vjHeadPosition.mData[ 0 ];
    startPoint[ 1 ] = -vjHeadPosition.mData[ 2 ];
    startPoint[ 2 ] =  vjHeadPosition.mData[ 1 ];

    //Move our matrix translation array to the head position
    matrix.mData[ 12 ] = startPoint[ 0 ];
    matrix.mData[ 13 ] = startPoint[ 1 ];
    matrix.mData[ 14 ] = startPoint[ 2 ];

    //Set the end point to the center of the juggler screen in osg coordinates
    //Calculate the screen pixel/juggler coordinate ratios

    /***********************/
    //This should probably be from the near plane, not the screen values,
    //to appropriately calculate the distance.
    /***********************/

    double wc_x_trans_ratio =
        ( mXMaxScreen - mXMinScreen ) / static_cast< double >( mWidth );
    double wc_y_trans_ratio =
        ( mYMaxScreen - mYMinScreen ) / static_cast< double >( mHeight );
    std::pair< double, double > screenRatios =
        std::pair< double, double >( wc_x_trans_ratio, wc_y_trans_ratio );

    //Convert the center screen pixel to a real world juggler coordinate
    gmtl::Point3d vjCenterScreenPosition;;
    vjCenterScreenPosition.mData[ 0 ] = mXMinScreen +
        0.5 * static_cast< double >( mWidth ) * screenRatios.first;
    vjCenterScreenPosition.mData[ 1 ] = mYMaxScreen -
        0.5 * static_cast< double >( mHeight ) * screenRatios.second;
    vjCenterScreenPosition.mData[ 2 ] = mZValScreen;

    //Convert meters to feet
    vjCenterScreenPosition *= 3.2808399;

    endPoint[ 0 ] =  vjCenterScreenPosition.mData[ 0 ];
    endPoint[ 1 ] = -vjCenterScreenPosition.mData[ 2 ];
    endPoint[ 2 ] =  vjCenterScreenPosition.mData[ 1 ];

    //Now we can get our center frustum vector
    frustumCenterVector = endPoint - startPoint;

    //////////////////////////////////////////////////////////////////////
    //Since not all geometry is drawn about the point (0, 0, 0),
    //we need to offset my the bounding sphere center
    //////////////////////////////////////////////////////////////////////
    //Move active switch node to (0, 0, 0) and then compute the bounding sphere
    //double position[ 3 ] = { 0, 0, 0 };
    //activeSwitchDCS->SetTranslationArray( position );
    osg::BoundingSphere bs = 
        vxs::SceneManager::instance()->GetActiveSwitchNode()->computeBound();
    
    //Bring the center of the geometry to the vj head position
    matrix.mData[ 12 ] -= bs.center().x();
    matrix.mData[ 13 ] -= bs.center().y();
    matrix.mData[ 14 ] -= bs.center().z();

    //////////////////////////////////////////////////////////////////////
    //Calculate the distance we need to move along the frustum center vector
    //to fit the bounding sphere of all the geometry inside the viewing frustum
    //////////////////////////////////////////////////////////////////////
    //Calculate the distance
    double distance;
    double theta = ( mFoVY * 0.5f ) * PIDivOneEighty;
    distance = ( bs.radius() / tan( theta ) );
    /*
    //This calculation assumes our screen width >= screen height
    if( mAspectRatio < 1.0 )
    {
        //Need to increase distance to fit the width FoV
        distance *= 1.0 / mAspectRatio;
    }
    */

    //Now move along the frustum center vector by the distance we calculated
    frustumCenterVector.normalize();
    frustumCenterVector *= distance;

    //Translate our matrix to this new frustum center vector position
    matrix.mData[ 12 ] += frustumCenterVector[ 0 ];
    matrix.mData[ 13 ] += frustumCenterVector[ 1 ];
    matrix.mData[ 14 ] += frustumCenterVector[ 2 ];

    //Set the current switch node's matrix w/ the new "frame all" transform 
    worldDCS->SetMat( matrix );

    //Get the new center of the bounding sphere in camera space
    osg::Vec3d center =
        vxs::SceneManager::instance()->GetActiveSwitchNode()->
        computeBound().center() * osg::Matrixd( worldDCS->GetMat().getData() );
    mCenterPoint->set( center.x(), center.y(), center.z() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameSelection()
{
    /*
    //Grab the selected DCS
    vxs::DCS* const selectedDCS =
        ves::xplorer::DeviceHandler::instance()->GetSelectedDCS();

    if( !selectedDCS )
    {
        FrameAll();
        return;
    }

    vxs::DCS* activeSwitchDCS =
        vxs::SceneManager::instance()->GetActiveSwitchNode();

    //Convert the selected matrix to world space
    osg::ref_ptr< vxs::LocalToWorldTransform > ltwt = 
        new vxs::LocalToWorldTransform( activeSwitchDCS, selectedDCS );
    gmtl::Matrix44d matrix = ltwt->GetLocalToWorldTransform();

    //activeSwitchDCS->SetMat( matrix );

    //Move the current matrix to its original position
    double position[ 3 ] = { 0, 0, 0 };
    activeSwitchDCS->SetTranslationArray( position );

    //Grab the bound and corresponding center values of the current matrix
    osg::BoundingSphere bs = selectedDCS->computeBound();

    //Calculate distance to fit current bounding sphere inside viewing frustum
    double distance;
    double theta = ( mFoVY * 0.5f ) * PIDivOneEighty;

    //if( mAspectRatio <= 1.0f )
    //{
        distance = ( bs.radius() / tan( theta ) ) * mAspectRatio;
    //}
    //else
    //{
        //distance = bs.radius() / tan( theta );
    //}

    //Transform the current matrix to the center of the juggler screen
    double wc_x_trans_ratio =
        ( mXMaxScreen - mXMinScreen ) / static_cast< double >( mWidth );
    double wc_y_trans_ratio =
        ( mYMaxScreen - mYMinScreen ) / static_cast< double >( mHeight );

    std::pair< double, double > screenRatios =
        std::pair< double, double >( wc_x_trans_ratio, wc_y_trans_ratio );

    double transformedPosition[ 3 ];
    double osgTransformedPosition[ 3 ];
    transformedPosition[ 0 ] = mXMinScreen +
        0.5 * static_cast< double >( mWidth ) * screenRatios.first;
    transformedPosition[ 1 ] = mYMaxScreen -
        0.5 * static_cast< double >( mHeight ) * screenRatios.second;
    transformedPosition[ 2 ] = mZValScreen;

    transformedPosition[ 0 ] *= 3.2808399;
    transformedPosition[ 1 ] *= 3.2808399;
    transformedPosition[ 2 ] *= 3.2808399;

    osgTransformedPosition[ 0 ] =  transformedPosition[ 0 ];
    osgTransformedPosition[ 1 ] = -transformedPosition[ 2 ];
    osgTransformedPosition[ 2 ] =  transformedPosition[ 1 ];

    matrix.mData[ 12 ] = osgTransformedPosition[ 0 ];
    matrix.mData[ 13 ] = osgTransformedPosition[ 1 ];
    matrix.mData[ 14 ] = osgTransformedPosition[ 2 ];

    //Translate into the screen for the calculated distance
    matrix.mData[ 13 ] += distance;

    //Translate center of bounding volume to the center of the screen
    matrix.mData[ 12 ] -= bs.center().x();
    matrix.mData[ 13 ] -= bs.center().y();
    matrix.mData[ 14 ] -= bs.center().z();

    //Set the current matrix w/ the new matrix
    activeSwitchDCS->SetMat( matrix );

    //Remove the local matrix from localToWorldMatrix
    gmtl::Matrix44d activeMatrix = selectedDCS->GetMat();
    matrix *= gmtl::invert( activeMatrix );

    //Multiplying by the new local matrix (mCenterPoint)
    osg::Matrixd tempMatrix;
    tempMatrix.set( matrix.getData() );
    osg::Vec3d center = selectedDCS->getBound().center() * tempMatrix;
    mCenterPoint->set( center.x(), center.y(), center.z() );
    */
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SkyCam()
{
    //Unselect the previous selected DCS
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

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
    Rotate( 1, 0, 0, 45 );
    ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SkyCamTo()
{
    if( !ModelHandler::instance()->GetActiveModel() )
    {
        return;
    }

    //To make this work we must:
    //1. get current state of world dcs
    //2. set dcs back to zero.
    //3. figure out where to set the center of the world
    //4. set the new rotation
    //5. get the vector of the this location
    //6. hand of to nav animation engine
    //7. reset the world dcs back to original state
    //Unselect the previous selected DCS
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

    //get the selected plugins cad
    //highlight it.
    osg::ref_ptr< vxs::DCS >selectedDCS =
        ves::xplorer::ModelHandler::instance()->GetActiveModel()->GetDCS();
    selectedDCS->SetTechnique("Select");
    ves::xplorer::DeviceHandler::instance()->SetSelectedDCS(
        selectedDCS.get() );
    osg::BoundingSphere sbs = selectedDCS->getBound();
    
    //Grab the current matrix
    //osg::ref_ptr< vxs::DCS > activeSwitchDCS =
    //    vxs::SceneManager::instance()->GetActiveSwitchNode();

    //Calculate the offset distance
    double distance = 2 * sbs.radius();

    ///Get the location of the selected model in local coordinates
    ///This value is always the same no matter where we are
    gmtl::Point3d osgTransformedPosition;
    gmtl::Point3d osgOrigPosition;
    osgTransformedPosition[ 0 ] = sbs.center( ).x( );
    osgTransformedPosition[ 1 ] = sbs.center( ).y( ) - distance;
    osgTransformedPosition[ 2 ] = sbs.center( ).z( );
    osgOrigPosition[ 0 ] = sbs.center( ).x( );
    osgOrigPosition[ 1 ] = sbs.center( ).y( );
    osgOrigPosition[ 2 ] = sbs.center( ).z( );

    //Move the center point to the center of the selected object
    osg::ref_ptr< vxs::CoordinateSystemTransform > cst =
        new vxs::CoordinateSystemTransform(
            vxs::SceneManager::instance()->GetActiveSwitchNode(),
            selectedDCS.get(), true );
    gmtl::Matrix44d localToWorldMatrix = cst->GetTransformationMatrix( false );

    //Remove the local matrix from localToWorldMatrix
    //gmtl::Matrix44d activeMatrix = selectedDCS->GetMat();
    //localToWorldMatrix *= gmtl::invert( activeMatrix );

    gmtl::Point3d tempTransPoint = 
        gmtl::makeTrans< gmtl::Point3d >( localToWorldMatrix );
    ///Remove the rotation from the transform matrix
    gmtl::Matrix44d tempTrans;
    tempTrans = gmtl::makeTrans< gmtl::Matrix44d >( tempTransPoint );
    //gmtl::Matrix44d invertTransMat = tempTrans;
    //gmtl::invert( invertTransMat );
    //std::cout << vxs::SceneManager::instance()->
    //GetActiveSwitchNode()->GetMat() << std::endl;
    //double tempRotRad = osg::DegreesToRadians( 90.0 );
    double tempRotRad2 = PIDivOneEighty * 0;
    //std::cout << tempRotRad << " " << tempRotRad2 << std::endl;
    //gmtl::Quatd convQuat( 1, 0, 0, tempRotRad2  );
    //gmtl::normalize( convQuat );
    gmtl::AxisAngled axisAngle( tempRotRad2, 1, 0, 0 );
    gmtl::Quatd quatAxisAngle = gmtl::make< gmtl::Quatd >( axisAngle );
    //std::cout << quatAxisAngle << std::endl;
    gmtl::Matrix44d tempRot;
    gmtl::setRot( tempRot, quatAxisAngle );
    //gmtl::Matrix44d rotateMat = tempTrans * tempRot * invertTransMat;
    gmtl::Matrix44d combineMat = tempTrans;// * tempRot;
    ///Add our end rotation back into the mix
    //std::cout << osgTransformedPosition << " first " << std::endl;

    osgTransformedPosition = combineMat * osgTransformedPosition;
    osgOrigPosition = combineMat * osgOrigPosition;
    //osgOrigPosition[ 1 ] = osgOrigPosition[ 1 ] + distance;
    //osgOrigPosition[ 2 ] = osgOrigPosition[ 2 ] + distance;
    //osgOrigPosition[ 1 ] = osgOrigPosition[ 1 ] - sbs.radius();
    //osgOrigPosition[ 2 ] = osgOrigPosition[ 2 ] + sbs.radius();
    //osgTransformedPosition =   tempTrans * tempRot * osgTransformedPosition;
    //std::cout << osgTransformedPosition << " " << osgOrigPosition << std::endl;
    ///Since the math implies we are doing a delta translation
    ///we need to go grab where we previously were
    double* temp = vxs::SceneManager::instance()->
        GetWorldDCS()->GetVETranslationArray();
    ///Add our distance and previous position back in and get our new end point
    gmtl::Vec4d pos;
    pos[ 0 ] = - osgOrigPosition[ 0 ] + temp[ 0 ];
    pos[ 1 ] = - ( osgOrigPosition[ 1 ] - distance ) + temp[ 1 ];
    pos[ 2 ] = - ( osgOrigPosition[ 2 ] ) + temp[ 2 ];

    //std::cout << pos << " " << osgTransformedPosition << std::endl;
    gmtl::Vec3d pos2;
    pos2[ 0 ] = pos[ 0 ];
    pos2[ 1 ] = pos[ 1 ];
    pos2[ 2 ] = pos[ 2 ];

    ///Set the center point to the new location
    mCenterPoint->set(
        -osgOrigPosition[ 0 ], -osgOrigPosition[1], -osgOrigPosition[2] );

    ///Hand the node we are interested in off to the animation engine
    ves::xplorer::NavigationAnimationEngine::instance()->SetDCS(
        vxs::SceneManager::instance()->GetWorldDCS() );
    ///Hand our created end points off to the animation engine
    ves::xplorer::NavigationAnimationEngine::instance()->SetAnimationEndPoints(
        pos2, quatAxisAngle );
    //This code needs to go in the animation engine
    /*
    //Multiplying by the new local matrix (mCenterPoint)
    osg::Matrixd tempMatrix;
    tempMatrix.set( localToWorldMatrix.getData() );
    osg::Vec3d center = selectedDCS->getBound().center() * tempMatrix;
    //osg::Vec3d center = sbs.center() * tempMatrix;
    mCenterPoint->set( center.x(), center.y(), center.z( ) );
    */
    
    //ProcessNavigationEvents();
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

        //keystrokes for 1st person mode
        //STRAFE LEFT
        case gadget::KEY_A:
        {
            Pan( -0.05, 0.0 );
            ProcessNavigationEvents();

            break;
        }
        //BACKWARD
        case gadget::KEY_S:
        {
            Zoom( -0.05 );
            ProcessNavigationEvents();

            break;
        }
        //STRAFE RIGHT
        case gadget::KEY_D:
        {
            Pan( 0.05, 0.0 );
            ProcessNavigationEvents();

            break;
        }
        //FORWARD
        case gadget::KEY_W:
        {
            Zoom( 0.05 );
            ProcessNavigationEvents();

            break;
        }
        //JUMP or UP if no physics
        case gadget::KEY_SPACE:
        {
            Pan( 0.0, 0.5 );
            ProcessNavigationEvents();

            break;
        }
        //DOWN if no physics
        case gadget::KEY_C:
        {
            Pan( 0.0, -0.5 );
            ProcessNavigationEvents();

            break;
        }

        //keystrokes for skycam mode
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
void KeyboardMouse::NavOnMousePress()
{
    //Nothing implemented yet
    return;

    //Would be cool to change mouse cursor for nav events here
    switch( mButton )
    {
        case gadget::MBUTTON1:
        {
            break;
        }
        case gadget::MBUTTON2:
        {
            break;
        }
        case gadget::MBUTTON3:
        {
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavOnMouseRelease()
{
    //Nothing implemented yet
    return;

    switch( mButton )
    {
        case gadget::MBUTTON1:
        {
            break;
        }
        case gadget::MBUTTON2:
        {
            break;
        }
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

    if( mMagnitude < mSensitivity )
    {
        return;
    }

    switch( mButton )
    {
        case gadget::MBUTTON1:
        {
            if( ( mX > 0.1f * mWidth ) && ( mX < 0.9f * mWidth ) &&
                ( mY > 0.1f * mHeight ) && ( mY < 0.9f * mHeight ) )
            {
                RotateView( delta.first, delta.second );
            }
            else
            {
                Twist();
            }

            break;
        }
        case gadget::MBUTTON2:
        {
            Pan( delta.first, delta.second );
            break;
        }
        case gadget::MBUTTON3:
        {
            Zoom( delta.second );
            break;
        }
    }

    ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
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
        case gadget::MBUTTON1:
        {
            ProcessNURBSSelectionEvents();

            break;
        }
        case gadget::MBUTTON2:
        {
            break;
        }
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
        case gadget::MBUTTON1:
        {
            vxs::SetStateOnNURBSNodeVisitor(
                vxs::SceneManager::instance()->GetActiveSwitchNode(), false,
                false, mCurrPos, std::pair< double, double >( 0.0, 0.0 ) );

            break;
        }
        case gadget::MBUTTON2:
        {
            break;
        }
        case gadget::MBUTTON3:
        {
            break;
        }
    }

    ProcessSelectionEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelOnMouseMotion( std::pair< double, double > delta )
{
    switch( mButton )
    {
        case gadget::MBUTTON1:
        {
            vxs::SetStateOnNURBSNodeVisitor(
                vxs::SceneManager::instance()->GetActiveSwitchNode(),
                true, true, mCurrPos, delta );

            break;
        }
        case gadget::MBUTTON2:
        {
            break;
        }
        case gadget::MBUTTON3:
        {
            break;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    ves::xplorer::DeviceHandler::instance()->ResetCenterPoint();

    gmtl::Matrix44d matrix;
    gmtl::identity( matrix );
    vxs::SceneManager::instance()->
        GetWorldDCS()->SetMat( matrix );
    
    vxs::SceneManager::instance()->GetWorldDCS()->SetQuat( *mResetAxis );
    vxs::SceneManager::instance()->GetWorldDCS()->SetTranslationArray(
        *mResetPosition );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RotateView( double dx, double dy )
{
    double tb_axis[ 3 ];
    double angle = mMagnitude * 400.0f;

    gmtl::Matrix44d matrix;
    gmtl::identity( matrix );

    tb_axis[ 0 ] = matrix[ 0 ][ 0 ] * dy + matrix[ 2 ][ 0 ] * dx;
    tb_axis[ 1 ] = matrix[ 0 ][ 1 ] * dy + matrix[ 2 ][ 1 ] * dx;
    tb_axis[ 2 ] = matrix[ 0 ][ 2 ] * dy + matrix[ 2 ][ 2 ] * dx;

    Rotate( tb_axis[ 0 ], tb_axis[ 1 ], tb_axis[ 2 ], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist()
{
    double theta = atan2f( mPrevPos.first - 0.5, mPrevPos.second - 0.5 );
    double newTheta = atan2f( mCurrPos.first - 0.5, mCurrPos.second - 0.5 );
    double angle = ( OneEightyDivPI ) * ( theta - newTheta );

    //The axis to twist about
    osg::Vec3 twist( 0, 1, 0 );
    Rotate( twist.x(), twist.y(), twist.z(), angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( double dy )
{
    double viewlength = mCenterPoint->mData[ 1 ];
    double d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;

    mDeltaTransform.mData[ 13 ] = d;
    mCenterPoint->mData[ 1 ] += d;

    //Test if center point has breached our specified threshold
    if( mCenterPoint->mData[ 1 ] < *mCenterPointThreshold )
    {
        vxs::DCS* const selectedDCS =
            ves::xplorer::DeviceHandler::instance()->GetSelectedDCS();
        //Only jump center point for the worldDCS
        if( !selectedDCS )
        {
            mCenterPoint->mData[ 1 ] = *mCenterPointJump;
        }
        //Prevent the center point from jumping
        //if we are manipulating a selected object
        else
        {
            mDeltaTransform.mData[ 13 ] = 0;
            mCenterPoint->mData[ 1 ] -= d;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom45( double dy )
{
    //needed for sky cam control
    double viewlength = mCenterPoint->mData[ 1 ];
    double d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;

    mDeltaTransform.mData[ 13 ] = d;
    mDeltaTransform.mData[ 14 ] = d;
    mCenterPoint->mData[ 1 ] += d;
    mCenterPoint->mData[ 2 ] += d;

    //Test if center point has breached our specified threshold
    if( mCenterPoint->mData[ 1 ] < *mCenterPointThreshold )
    {
        vxs::DCS* const selectedDCS =
            ves::xplorer::DeviceHandler::instance()->GetSelectedDCS();
        //Only jump center point for the worldDCS
        if( !selectedDCS )
        {
            mCenterPoint->mData[ 1 ] = *mCenterPointJump;
        }
        //Prevent the center point from jumping
        //if we are manipulating a selected object
        else
        {
            mDeltaTransform.mData[ 13 ] = 0;
            mCenterPoint->mData[ 1 ] -= d;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( double dx, double dy )
{
    double d = mCenterPoint->mData[ 1 ];
    double theta = ( mFoVY * 0.5f ) * ( PIDivOneEighty );
    double b = 2 * d * tan( theta );
    double dwx = dx * b * mAspectRatio;
    double dwy = -dy * b;

    mDeltaTransform.mData[ 12 ] = dwx;
    mDeltaTransform.mData[ 14 ] = dwy;

    mCenterPoint->mData[ 0 ] += dwx;
    mCenterPoint->mData[ 2 ] += dwy;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( double x, double y, double z, double angle )
{
    
    double temp = ::sqrtf( x * x + y * y + z * z );
    if( temp != 0.0f )
    {
        double tempRatio = 1 / temp;
        x *= tempRatio;
        y *= tempRatio;
        z *= tempRatio;
    }

    /*
    double rad = angle * PIDivOneEighty;
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
    mDeltaTransform.mData[ 15 ] = 1.0f;
    */

    double rad2 = angle * PIDivOneEighty;
    gmtl::AxisAngled axisAngle( rad2, x, y, z );
    mDeltaTransform = gmtl::makeRot< gmtl::Matrix44d >( axisAngle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelectionLine()
{
    osg::Vec3d startPoint, endPoint;
    SetStartEndPoint( &startPoint, &endPoint );
    mBeamLineSegment->set( startPoint, endPoint );
    //With the new implementation for the bounding volume this code 
    //causes the scene to go black
    //DrawLine( startPoint, endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNURBSSelectionEvents()
{
    osg::ref_ptr< osgUtil::IntersectorGroup > intersectorGroup =
        new osgUtil::IntersectorGroup();
    osg::ref_ptr< vxs::nurbs::PointLineSegmentIntersector > intersector =
        new vxs::nurbs::PointLineSegmentIntersector(
            mBeamLineSegment->start(), mBeamLineSegment->end() );
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
    osgUtil::IntersectVisitor objectBeamIntersectVisitor;
    objectBeamIntersectVisitor.addLineSegment( mBeamLineSegment.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    vxs::SceneManager::instance()->GetRootNode()->accept(
        objectBeamIntersectVisitor );

    osgUtil::IntersectVisitor::HitList beamHitList;
    beamHitList = objectBeamIntersectVisitor.getHitList(
        mBeamLineSegment.get() );

    ProcessHit( beamHitList );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{
    //Unselect the previous selected DCS
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

    //Now find the new selected DCS
    if( listOfHits.empty() )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tKeyboardMouse::ProcessHit No object selected"
            << std::endl << vprDEBUG_FLUSH;

        return;
    }

    //Search for first item that is not the laser
    osgUtil::Hit objectHit;
    for( size_t i = 0; i <  listOfHits.size(); ++i )
    {
        objectHit = listOfHits[ i ];
        if( objectHit._geode->getName() != "Laser" &&
            objectHit._geode->getName() != "Root Node" )
        {
            break;
        }
    }

    //Make sure it is good
    if( !objectHit._geode.valid() )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tKeyboardMouse::ProcessHit Invalid object selected"
            << std::endl << vprDEBUG_FLUSH;

        return;
    }

    //Now find the id for the cad
    vxs::FindParentsVisitor parentVisitor(
        objectHit._geode.get() );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( !parentNode.valid() )
    {
        vprDEBUG( vesDBG, 1 )
            << "|\tObject does not have name parent name"
            << objectHit._geode->getParents().front()->getName()
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
    ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );

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
