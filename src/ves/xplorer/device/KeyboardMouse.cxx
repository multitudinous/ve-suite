/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#include <ves/xplorer/scenegraph/LocalToWorldTransform.h>
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
    gmtl::identity( mCurrentTransform );
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
  /*std::cout << " start point "
              << jugglerHeadPointTemp[ 0 ] << " "
              << jugglerHeadPointTemp[ 1 ] << " "
              << jugglerHeadPointTemp[ 2 ] << std::endl;*/

    //Get the vector
    gmtl::Vec3d vjVec = mousePosition - jugglerHeadPointTemp;
    gmtl::normalize( vjVec );

    //Set the end point
    double distance = mFarFrustum;
    gmtl::Point3d endPointGMTL = vjVec * distance;
    endPoint->set( endPointGMTL[ 0 ],
                   endPointGMTL[ 1 ],
                   endPointGMTL[ 2 ] );
  /*std::cout << " end point " 
              << endPointGMTL[ 0 ] << " "
              << endPointGMTL[ 1 ] << " "
              << endPointGMTL[ 2 ] << std::endl;*/
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint )
{   
    osg::Group* rootNode =
        ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode();

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

        if( type == gadget::KeyPressEvent )
        {
            gadget::KeyEventPtr keyEvt =
                boost::dynamic_pointer_cast< gadget::KeyEvent >( *i );
            mKey = keyEvt->getKey();

            //Navigation mode
            if( mode == 0 )
            {
                NavKeyboard();
            }
            //Selection mode
            else if( mode == 1 )
            {
                SelKeyboard();
            }
        }
        /*
        //Use this call if you want to hold a mKey for it to be active
        else if( type == gadget::KeyReleaseEvent )
        {
            mKey = -1;
        }
        */
        else if( type == gadget::MouseButtonPressEvent )
        {
            gadget::MouseEventPtr mouse_evt =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );
            mButton = mouse_evt->getButton();
            mState = 1;
            mX = mouse_evt->getX();
            mY = mouse_evt->getY();
            mCurrPos.first =
                static_cast< double >( mX ) / static_cast< double >( mWidth );
            mCurrPos.second =
                static_cast< double >( mY ) / static_cast< double >( mHeight );
            mPrevPos.first = mCurrPos.first;
            mPrevPos.second = mCurrPos.second;

            //Navigation mode
            if( mode == 0 )
            {
                NavMouse();

                //If animation mode, stop the animation with mouse press event
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
                SelMouse();
            }
        }
        else if( type == gadget::MouseButtonReleaseEvent )
        {
            gadget::MouseEventPtr mouse_evt =
                boost::dynamic_pointer_cast< gadget::MouseEvent >( *i );

            mButton = mouse_evt->getButton();
            mState = 0;
            mX = mouse_evt->getX();
            mY = mouse_evt->getY();
            mCurrPos.first =
                static_cast< double >( mX ) / static_cast< double >( mWidth );
            mCurrPos.second =
                static_cast< double >( mY ) / static_cast< double >( mHeight );

            //Navigation mode
            if( mode == 0 )
            {
                NavMouse();
            }
            //Selection mode
            else if( mode == 1   )
            {
                //We process selection on the release of the left button because
                //in the future we would like to be able to select with a 
                //rubber band rectangle which would mean the mouse down would be
                //the first point of the rectangle and the mouse up would be the
                //second point
                SelMouse();
            }

            mPrevPos.first = mCurrPos.first;
            mPrevPos.second = mCurrPos.second;
        }
        else if( type == gadget::MouseMoveEvent )
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
                    NavMotion( delta );
                }
                //Selection mode
                else if( mode == 1 )
                {
                    SelMotion( delta );
                }

                mPrevPos.first = mCurrPos.first;
                mPrevPos.second = mCurrPos.second;
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNavigationEvents()
{
    ves::xplorer::scenegraph::DCS* const activeDCS =
        ves::xplorer::DeviceHandler::instance()->GetActiveDCS();
    ves::xplorer::scenegraph::DCS* activeSwitchNode =
        ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveSwitchNode();

    //Grab the active matrix to manipulate
    mCurrentTransform = activeDCS->GetMat();

    //Convert mCurrentTransform to world space if not already in it
    std::string name = activeSwitchNode->GetName();
    if( activeDCS->GetName() !=  name )
    {
        osg::ref_ptr< ves::xplorer::scenegraph::LocalToWorldTransform >
            localToWorldTransform =
                new ves::xplorer::scenegraph::LocalToWorldTransform( 
                    activeSwitchNode, activeDCS );

        mCurrentTransform = localToWorldTransform->GetLocalToWorldTransform();
    }

    //Translate active dcs by distance that the mHead is away from the origin
    gmtl::Matrix44d transMat =
        gmtl::makeTrans< gmtl::Matrix44d >( -*mCenterPoint );
    gmtl::Matrix44d worldMatTrans = transMat * mCurrentTransform;

    //Get the position of the mHead in the new world space as if it is on the origin
    gmtl::Point3d newJugglerHeadPoint;
    gmtl::Point3d newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
    gmtl::Vec4d newGlobalHeadPointVec;
    newGlobalHeadPointVec[ 0 ] = newGlobalHeadPointTemp[ 0 ];
    newGlobalHeadPointVec[ 1 ] = newGlobalHeadPointTemp[ 1 ];
    newGlobalHeadPointVec[ 2 ] = newGlobalHeadPointTemp[ 2 ];

    //Rotate the mHead vector by the rotation increment
    gmtl::Vec4d rotateJugglerHeadVec = mDeltaTransform * newGlobalHeadPointVec;

    //Split apart the current matrix into rotation and translation parts
    gmtl::Matrix44d accuRotation;
    gmtl::Matrix44d matrix;

    for( int i = 0; i < 3; ++i )
    {
        //Get the current rotation matrix
        accuRotation[ i ][ 0 ] = mCurrentTransform[ i ][ 0 ];
        accuRotation[ i ][ 1 ] = mCurrentTransform[ i ][ 1 ];
        accuRotation[ i ][ 2 ] = mCurrentTransform[ i ][ 2 ];

        //Get the current translation matrix
        matrix[ i ][ 3 ] = rotateJugglerHeadVec[ i ] + mCenterPoint->mData[ i ];
    }
    /*
    Convert head to world space to run intersection tests
    gmtl::Matrix44d worldDCSInverse;
    gmtl::invert( worldDCSInverse,
                  ves::xplorer::scenegraph::SceneManager::instance()->GetActiveSwitchNode()->GetMat() );

    gmtl::Matrix44d vjHeadMat;
    vjHeadMat = worldDCSInverse*convertTo< double >( mHead->getData() );

    osg::Vec3 headPositionInWorld = osg::Vec3( vjHeadMat[ 0 ][ 3 ] - ( 0.0345 * 3.2808399 ),
                                               vjHeadMat[ 1 ][ 3 ],
                                               vjHeadMat[ 2 ][ 3 ] );

    vprDEBUG( vesDBG, 3 ) << "|\tKeyboardMouse::ProcessNavigation Head Position in World Space: "
                          <<headPositionInWorld.x()<<","
                          <<headPositionInWorld.y()<<","
                          <<headPositionInWorld.z()<< std::endl << vprDEBUG_FLUSH;

    if( CheckCollisionsWithHead( headPositionInWorld ) )
    {
        mDeltaTransform.mData[ 12 ] = -mDeltaTransform.mData[ 12 ];
        mDeltaTransform.mData[ 13 ] = -mDeltaTransform.mData[ 13 ];
        mDeltaTransform.mData[ 14 ] = -mDeltaTransform.mData[ 14 ];
    }
    */

    //Multiply by the transform and then by the rotation
    matrix = matrix * mDeltaTransform * accuRotation;

    //Convert matrix back to local space after delta transform has been applied
    if( activeDCS->GetName() != name )
    {
        //Remove local matrix from mCurrentTransform
        //We are multiplying by a new transformed local matrix
        gmtl::Matrix44d activeMatrix = activeDCS->GetMat();
        mCurrentTransform *= gmtl::invert( activeMatrix );

        matrix = gmtl::invert( mCurrentTransform ) * matrix;
    }

    //Set the activeDCS w/ new transform
    activeDCS->SetMat( matrix );

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
    double l, double r, double t, double b, double n, double f )
{
    mLeftFrustum = l;
    mRightFrustum = r;
    mTopFrustum = t;
    mBottomFrustum = b;
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
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveSwitchNode();
    gmtl::Matrix44d matrix = activeSwitchDCS->GetMat();

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
    double position[ 3 ] = { 0, 0, 0 };
    activeSwitchDCS->SetTranslationArray( position );
    osg::BoundingSphere bs = activeSwitchDCS->computeBound();

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
    activeSwitchDCS->SetMat( matrix );

    //Get the new center of the bounding sphere
    bs = activeSwitchDCS->computeBound();
    //Set the center point of rotation to the new center of the bounding sphere
    mCenterPoint->set( bs.center().x(), bs.center().y(), bs.center().z() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameSelection()
{
    /*
    //Grab the selected DCS
    ves::xplorer::scenegraph::DCS* const selectedDCS =
        ves::xplorer::DeviceHandler::instance()->GetSelectedDCS();

    if( !selectedDCS )
    {
        FrameAll();
        return;
    }

    ves::xplorer::scenegraph::DCS* activeSwitchDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveSwitchNode();

    //Convert the selected matrix to world space
    osg::ref_ptr< ves::xplorer::scenegraph::LocalToWorldTransform > ltwt = 
        new ves::xplorer::scenegraph::LocalToWorldTransform(
            activeSwitchDCS, selectedDCS );
    gmtl::Matrix44d matrix = ltwt->GetLocalToWorldTransform();

    //activeSwitchDCS->SetMat( matrix );

    //Move the current matrix to its original position
    double position[ 3 ] = { 0, 0, 0 };
    activeSwitchDCS->SetTranslationArray( position );

    //Grab the bound and corresponding center values of the current matrix
    osg::BoundingSphere bs = selectedDCS->computeBound();

    //Calculate the distance needed to fit current bounding sphere inside viewing frustum
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
void KeyboardMouse::SkyCam( )
{
    //Unselect the previous selected DCS
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

    //gmtl::Matrix44d matrix;
    //mCenterPoint->mData[ 1 ] = matrix[ 1 ][ 3 ] = *mCenterPointThreshold;
    //ves::xplorer::scenegraph::SceneManager::instance()->GetActiveSwitchNode()->SetMat( matrix );
    
    //reset view
    ves::xplorer::scenegraph::SceneManager::instance()->
        GetWorldDCS()->SetQuat( *mResetAxis );
    ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->
        SetTranslationArray( *mResetPosition );
    
    //Grab the current matrix
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveSwitchNode();

    osg::BoundingSphere bs = activeSwitchDCS->computeBound();

    //Calculate the distance
    double distance =  bs.radius();

    //move the cad
    double osgTransformedPosition[ 3 ];
    osgTransformedPosition[ 0 ] = - bs.center( ).x( );
    osgTransformedPosition[ 1 ] = - bs.center( ).y( ) + distance;
    osgTransformedPosition[ 2 ] = - bs.center( ).z( );
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
void KeyboardMouse::SkyCamTo( )
{
    //Unselect the previous selected DCS
    ves::xplorer::DeviceHandler::instance()->UnselectObjects();

    //gmtl::Matrix44d matrix;
    //mCenterPoint->mData[ 1 ] = matrix[ 1 ][ 3 ] = *mCenterPointThreshold;
    //ves::xplorer::scenegraph::SceneManager::instance()->GetActiveSwitchNode()
    //->SetMat( matrix );
    

    //reset view
    //ves::xplorer::scenegraph::SceneManager::instance()->
    //    GetWorldDCS()->SetQuat( *mResetAxis );
    //ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->
    //    SetTranslationArray( *mResetPosition );

    //get the selected plugins cad
    //highlight it.
    osg::ref_ptr< ves::xplorer::scenegraph::DCS >selectedDCS =
        ModelHandler::instance()->GetActiveModel()->GetDCS();
    selectedDCS->SetTechnique("Select");
    ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( selectedDCS.get() );
    osg::BoundingSphere sbs = selectedDCS->getBound();
    
    //Grab the current matrix
    //osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
    //    ves::xplorer::scenegraph::SceneManager::instance()->
    //        GetActiveSwitchNode();

    //Calculate the distance
    double distance = 2 * sbs.radius();

    //move the cad
    gmtl::Point3d osgTransformedPosition;
    osgTransformedPosition[ 0 ] = sbs.center( ).x( );
    osgTransformedPosition[ 1 ] = sbs.center( ).y( );
    osgTransformedPosition[ 2 ] = sbs.center( ).z( );
    //osgTransformedPosition[3] = 1;
    //activeSwitchDCS->SetTranslationArray( osgTransformedPosition );

    //Move the center point to the center of the selected object
    osg::ref_ptr< ves::xplorer::scenegraph::LocalToWorldTransform > ltwt =
        new ves::xplorer::scenegraph::LocalToWorldTransform(
            ves::xplorer::scenegraph::SceneManager::instance()->
                GetActiveSwitchNode(), selectedDCS.get() );
    gmtl::Matrix44d localToWorldMatrix = ltwt->GetLocalToWorldTransform();
    //localToWorldMatrix->

    //Remove the local matrix from localToWorldMatrix
    gmtl::Matrix44d activeMatrix = selectedDCS->GetMat();
    localToWorldMatrix *= gmtl::invert( activeMatrix );

    //double* selectedPos = selectedDCS->GetVETranslationArray();
    //gmtl::Vec4d selectedPosVec( selectedPos[ 0 ], selectedPos[ 1 ], 
    //selectedPos[ 2 ], 1 ); 
    gmtl::Quatd convQuat( 1, 0, 0, osg::DegreesToRadians( 180.0 ) );
    gmtl::Matrix44d tempTrans;
    //gmtl::Matrix44f tempTransFloat = 
    //gmtl::convertTo<float>(localToWorldMatrix);
    gmtl::Point3d tempTransPoint = 
        gmtl::makeTrans< gmtl::Point3d >( localToWorldMatrix );
    tempTrans = gmtl::makeTrans< gmtl::Matrix44d >( tempTransPoint );
    //tempTrans = 
    gmtl::Matrix44d tempRot;
    gmtl::setRot( tempRot, convQuat );
    //osgTransformedPosition = tempTrans * tempRot * osgTransformedPosition;
    osgTransformedPosition = localToWorldMatrix * osgTransformedPosition;
    //osg::Matrix inMat;
    //inMat.set( localToWorldMatrix.getData() );
    //osg::Vec3d trans( selectedPosVec[ 0 ], selectedPosVec[ 1 ], 
    //selectedPosVec[ 2 ] );// = inMat.getTrans();

    //Multiplying by the new local matrix (mCenterPoint)
    //osg::Matrixd tempMatrix;
    //tempMatrix.set( localToWorldMatrix.getData() );
    //osg::Vec3d center = selectedDCS->getBound().center() * tempMatrix;
    //osg::Vec3d center = sbs.center() * tempMatrix;
    //mCenterPoint->set( center.x(), center.y(), center.z( ) );

    //put it at 45 degrees
    //Rotate( 1, 0, 0, 45 );
    //ves::xplorer::environment::NavigationAnimationEngine->
    //instance()->SetWorldDCS();
    double * temp = ves::xplorer::scenegraph::SceneManager::instance()->
        GetWorldDCS()->GetVETranslationArray();
    
    mCenterPoint->set( osgTransformedPosition[0], osgTransformedPosition[1],
        osgTransformedPosition[2] );
    
    gmtl::Vec3d pos;
    pos[ 0 ] = -osgTransformedPosition[ 0 ] + temp[0];
    pos[ 1 ] = -osgTransformedPosition[ 1 ] + temp[1];
    pos[ 2 ] = -osgTransformedPosition[ 2 ] + temp[2];

    //ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->
    //SetTranslationArray( pos.mData );

    //osg::Quat tempQuat = selectedDCS->GetQuat();

    ves::xplorer::NavigationAnimationEngine::instance()->
        SetDCS( ves::xplorer::scenegraph::SceneManager::instance()->
        GetWorldDCS() );

    ves::xplorer::NavigationAnimationEngine::instance()->
        SetAnimationEndPoints( pos, convQuat );
    //ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavKeyboard()
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
        case gadget::KEY_C:
        {
            FrameSelection();
            break;
        }
        case gadget::KEY_S:
        {
            ves::xplorer::scenegraph::PhysicsSimulator::instance()->
                StepSimulation();
            break;
        }
        case gadget::KEY_SPACE:
        {
            ves::xplorer::scenegraph::PhysicsSimulator::instance()->
                ResetScene();
            break;
        }
        case gadget::KEY_K:
        {
            SkyCam();
            break;
        }

        //keystrokes for skycam mode
        case gadget::KEY_UP:
        {
            //Grab the current matrix
            //osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
            //    ves::xplorer::scenegraph::SceneManager::instance()->
            //    GetActiveSwitchNode();
            //osg::BoundingSphere bs = activeSwitchDCS->computeBound();
            
            //Zoom45( bs.radius() / 100 );
            Zoom45( 0.05 );
            ProcessNavigationEvents();
            break;
        }
        case gadget::KEY_DOWN: 
        {
            //Grab the current matrix
            //osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
            //    ves::xplorer::scenegraph::SceneManager::instance()->
            //    GetActiveSwitchNode();
            //osg::BoundingSphere bs = activeSwitchDCS->computeBound();
            
            //Zoom45( -bs.radius() / 100 );
            Zoom45( -0.05 );
            ProcessNavigationEvents();
            break;
        }
        case gadget::KEY_LEFT:
        {
            //Grab the current matrix
            //osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
            //    ves::xplorer::scenegraph::SceneManager::instance()->
            //    GetActiveSwitchNode();
            //osg::BoundingSphere bs = activeSwitchDCS->computeBound();
            
            //Pan( bs.radius() / 100, 0 );
            Pan( 0.05, 0 );
            ProcessNavigationEvents();
            break;
        }
        case gadget::KEY_RIGHT:
        {
            //Grab the current matrix
            //osg::ref_ptr< ves::xplorer::scenegraph::DCS > activeSwitchDCS =
            //    ves::xplorer::scenegraph::SceneManager::instance()->
            //    GetActiveSwitchNode();
            //osg::BoundingSphere bs = activeSwitchDCS->computeBound();
            
            //Pan( - bs.radius() / 100, 0 );
            Pan( -0.05, 0 );
            ProcessNavigationEvents();
            break;
        }
    }

    //Reset mKey
    mKey = -1;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMouse()
{
    return;

    //Would be cool to change mouse cursor for nav events here
    if( mButton == gadget::MBUTTON1 )
    {
        ;
    }
    else if( mButton == gadget::MBUTTON2 )
    {
        ;
    }
    else if( mButton == gadget::MBUTTON3 )
    {
        ;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMotion( std::pair< double, double > delta )
{
    mMagnitude =
        sqrtf( delta.first * delta.first + delta.second * delta.second );

    if( mMagnitude < mSensitivity )
    {
        return;
    }

    if( mButton == gadget::MBUTTON1
            && ( mX > 0.1f * mWidth )
            && ( mX < 0.9f * mWidth )
            && ( mY > 0.1f * mHeight )
            && ( mY < 0.9f * mHeight ) )
    {
        RotateView( delta.first, delta.second );
    }
    else if( mButton == gadget::MBUTTON3 )
    {
        Zoom( delta.second );
    }
    else if( mButton == gadget::MBUTTON2 )
    {
        Pan( delta.first, delta.second );
    }
    else if( mButton == gadget::MBUTTON1 )
    {
        Twist();
    }

    ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelKeyboard()
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelMouse()
{
    UpdateSelectionLine();

    if( mState == 1 && mButton == gadget::MBUTTON1 )
    {
        ProcessNURBSSelectionEvents();

        return;
    }
    else if( mState == 0 && mButton == gadget::MBUTTON1 )
    {
        ves::xplorer::scenegraph::SetStateOnNURBSNodeVisitor(
            ves::xplorer::scenegraph::SceneManager::instance()->
                GetActiveSwitchNode(), false, false, mCurrPos,
                std::pair< double, double >( 0.0, 0.0 ) );

        ProcessSelectionEvents();
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelMotion( std::pair< double, double > delta )
{
    if( mButton == gadget::MBUTTON1 )
    {
        ves::xplorer::scenegraph::SetStateOnNURBSNodeVisitor(
            ves::xplorer::scenegraph::SceneManager::instance()->
            GetActiveSwitchNode(), true, true, mCurrPos, delta );
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    ves::xplorer::DeviceHandler::instance()->ResetCenterPoint();

    gmtl::Matrix44d matrix;
    gmtl::identity( matrix );
    ves::xplorer::scenegraph::SceneManager::instance()->
        GetActiveSwitchNode()->SetMat( matrix );
    
    ves::xplorer::scenegraph::SceneManager::instance()->
        GetWorldDCS()->SetQuat( *mResetAxis );
    ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS()->
        SetTranslationArray( *mResetPosition );
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
        ves::xplorer::scenegraph::DCS* const selectedDCS =
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
        ves::xplorer::scenegraph::DCS* const selectedDCS =
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
    double temp = sqrtf( x * x + y * y + z * z );
    if( temp != 0.0f )
    {
        x /= temp;
        y /= temp;
        z /= temp;
    }

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
    osg::ref_ptr< ves::xplorer::scenegraph::nurbs::PointLineSegmentIntersector > intersector =
        new ves::xplorer::scenegraph::nurbs::PointLineSegmentIntersector(
            mBeamLineSegment->start(), mBeamLineSegment->end() );
    intersectorGroup->addIntersector( intersector.get() );

    osgUtil::IntersectionVisitor controlMeshPointIntersectVisitor;

    controlMeshPointIntersectVisitor.setIntersector( intersectorGroup.get() );

    //Add the IntersectVisitor to the root Node so that all geometry will be
    //checked and no transforms are done to the line segement
    ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode()->accept(
        controlMeshPointIntersectVisitor );

    if( intersectorGroup->containsIntersections() )
    {
         //std::cout<<"Found intersections "<<std::endl;
         ///only want the first one
         ves::xplorer::scenegraph::nurbs::PointLineSegmentIntersector::Intersections& intersections  =
                                                  intersector->getIntersections();
         ves::xplorer::scenegraph::nurbs::PointLineSegmentIntersector::Intersection closestControlPoint = 
                            (*intersections.begin());
         osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBSControlMesh> ctMesh =
            dynamic_cast<ves::xplorer::scenegraph::nurbs::NURBSControlMesh*>( closestControlPoint.drawable.get() );
         if( ctMesh.valid() )
         {
             osg::ref_ptr<ves::xplorer::scenegraph::nurbs::NURBS> nurbs = 
                dynamic_cast<ves::xplorer::scenegraph::nurbs::NURBS*>( ctMesh->getParent( 0 ) );
             if( nurbs.valid() )
             {
                 nurbs->SetSelectedControlPoint( closestControlPoint.primitiveIndex );
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
    ves::xplorer::scenegraph::SceneManager::instance()->GetRootNode()->accept(
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
    ves::xplorer::scenegraph::FindParentsVisitor parentVisitor(
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

    ves::xplorer::scenegraph::DCS* newSelectedDCS =
        static_cast< ves::xplorer::scenegraph::DCS* >( parentNode.get() );
    newSelectedDCS->SetTechnique( "Select" );
    ves::xplorer::DeviceHandler::instance()->SetSelectedDCS( newSelectedDCS );

    //Move the center point to the center of the selected object
    osg::ref_ptr< ves::xplorer::scenegraph::LocalToWorldTransform > ltwt =
        new ves::xplorer::scenegraph::LocalToWorldTransform(
            ves::xplorer::scenegraph::SceneManager::instance()->
                GetActiveSwitchNode(), newSelectedDCS );
    gmtl::Matrix44d localToWorldMatrix = ltwt->GetLocalToWorldTransform();

    //Remove the local matrix from localToWorldMatrix
    gmtl::Matrix44d activeMatrix = newSelectedDCS->GetMat();
    localToWorldMatrix *= gmtl::invert( activeMatrix );

    //Multiplying by the new local matrix (mCenterPoint)
    osg::Matrixd tempMatrix;
    tempMatrix.set( localToWorldMatrix.getData() );
    osg::Vec3d center = newSelectedDCS->getBound().center() * tempMatrix;
    mCenterPoint->set( center.x(), center.y(), center.z() );
}
////////////////////////////////////////////////////////////////////////////////
