/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Stuff --- //
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/SceneGraph/FindParentsVisitor.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include "VE_Xplorer/SceneGraph/Group.h"

#include "VE_Xplorer/SceneGraph/Utilities/LocalToWorldNodePath.h"

// --- Bullet Stuff --- //
#include <LinearMath/btVector3.h>

// --- VR Juggler Stuff --- //
#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include <gmtl/Xforms.h>
#include <gmtl/Generate.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>

// --- OSG Stuff --- //
#include <osg/Array>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/LineSegment>
#include <osg/NodeVisitor>
#include <osg/BoundingBox>
//#include <osg/PolygonStipple>

// --- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

using namespace VE_Xplorer;

const double OneEightyDivPI = 57.29577951;
const double PIDivOneEighty = .0174532925;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
:
m_key( -1 ),
m_button( -1 ),
m_state( 0 ),
m_x( 0 ),
m_y( 0 ),

m_width( 1 ),
m_height( 1 ),

m_aspectRatio( 0.0f ),
m_fovy( 0.0f ),
m_leftFrustum( 0.0f ),
m_rightFrustum( 0.0f ),
m_topFrustum( 0.0f ),
m_bottomFrustum( 0.0f ),
m_nearFrustum( 0.0f ),
m_farFrustum( 0.0f ),

m_xminScreen( 0.0f ),
m_xmaxScreen( 0.0f ),
m_yminScreen( 0.0f ),
m_ymaxScreen( 0.0f ),
m_zvalScreen( 0.0f ),

m_magnitude( 0.0f ),
m_sensitivity( 1.0e-06 ),

m_animate( false ),

beamLineSegment( new osg::LineSegment )
{
    m_keyboard.init( "VJKeyboard" );
    m_head.init( "VJHead" );

    m_currPos[0] = 0.0f;
    m_currPos[1] = 0.0f;
    m_prevPos[0] = 0.0f;
    m_prevPos[1] = 0.0f;

    gmtl::identity( m_deltaTransform );
    gmtl::identity( m_currentTransform );
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
void KeyboardMouse::SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint )
{
    //Be sure m_width and m_height are set before calling this function
    double wc_x_trans_ratio = ( ( m_xmaxScreen - m_xminScreen ) ) / double( m_width );
    double wc_y_trans_ratio = ( ( m_ymaxScreen - m_yminScreen ) ) / double( m_height );

    std::pair< double, double > screenRatios = std::pair< double, double >( wc_x_trans_ratio, wc_y_trans_ratio );

    double transformedPosition[3];
    double osgTransformedPosition[3];
    transformedPosition[0] = m_xminScreen + ( m_x * screenRatios.first );
    transformedPosition[1] = m_ymaxScreen - ( m_y * screenRatios.second );
    transformedPosition[2] = m_zvalScreen;

    transformedPosition[0] *= 3.2808399;
    transformedPosition[1] *= 3.2808399;
    transformedPosition[2] *= 3.2808399;

    osgTransformedPosition[0] =  transformedPosition[0];
    osgTransformedPosition[1] = -transformedPosition[2];
    osgTransformedPosition[2] =  transformedPosition[1];

    /*
    std::cout << " m_x location = " << m_x << std::endl 
              << " m_y location = " << m_y << std::endl 
              << " location = " << osgTransformedPosition[0] << " " << osgTransformedPosition[1] << " " << osgTransformedPosition[2] << std::endl
              << " ratio = " << screenRatios.first << " " << screenRatios.second << std::endl
              << " screen values = " << m_xminScreen << " " << m_yminScreen << " " << m_zvalScreen << std::endl
              << std::endl;

    std::cout << " m_x " << m_xminScreen << " " << m_xmaxScreen 
              << " m_y " << m_yminScreen << " " << m_ymaxScreen 
              << " z " << m_zvalScreen 
              << " m_width " << m_width << " m_height "<< m_height << std::endl;
    */

    double wandEndPoint[3];
    double distance = 10000.0f;

    gmtl::Matrix44f vjHeadMat = m_head->getData();

    //Get juggler Matrix of worldDCS
    //Note:: for pf we are in juggler land
    //       for osg we are in z up land
    gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
    jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );

    //We have to offset negative m_x because the view is being drawn for the m_leftFrustum 
    //eye which means the the frustums are being setup for the m_leftFrustum eye
    jugglerHeadPointTemp[ 0 ] = jugglerHeadPoint[0] - ( 0.034 * 3.280839 );
    jugglerHeadPointTemp[ 1 ] = -jugglerHeadPoint[2];
    jugglerHeadPointTemp[ 2 ] = jugglerHeadPoint[1];

    gmtl::Point3d mousePosition( osgTransformedPosition[0], osgTransformedPosition[1], osgTransformedPosition[2] );
    gmtl::Vec3d vjVec = mousePosition - jugglerHeadPointTemp;
    //std::cout << vjVec << " = " << mousePosition << " - " << jugglerHeadPointTemp << std::endl;
    //gmtl::normalize( vjVec );
    //std::cout << vjVec << std::endl;

    startPoint->set( osgTransformedPosition[0], osgTransformedPosition[1], osgTransformedPosition[2] );

    for( int i = 0; i < 3; i++ )
    {
        wandEndPoint[i] = ( vjVec[i] * distance ); 
    }

    //std::cout << " end point " << wandEndPoint[0] << " " << wandEndPoint[1] << " " << wandEndPoint[2] << std::endl;
    endPoint->set( wandEndPoint[0], wandEndPoint[1], wandEndPoint[2] );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::DrawLine( osg::Vec3f startPoint, osg::Vec3f endPoint )
{
    if( beamGeode.valid() )
    {
        VE_SceneGraph::SceneManager::instance()->GetRootNode()->removeChild( beamGeode.get() );
    }

    beamGeode = new osg::Geode();
    beamGeode->setName( "Laser" );

    osg::ref_ptr< osg::Geometry > line = new osg::Geometry();
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array;
    osg::ref_ptr< osg::Vec4Array > colors = new osg::Vec4Array;
    osg::ref_ptr< osg::StateSet > m_stateset = new osg::StateSet;

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 1.0f, 0.0f, 0.0f, 1.0f ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > line_width = new osg::LineWidth;
    line_width->setWidth( 4.0f );
    m_stateset->setAttribute( line_width.get() );
    line->setStateSet( m_stateset.get() );

    line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );

    beamGeode->addDrawable( line.get() );      

    VE_SceneGraph::SceneManager::instance()->GetRootNode()->addChild( beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetScreenCornerValues( std::map< std::string, double > values )
{
    m_xminScreen = values[ "xmin" ];
    m_xmaxScreen = values[ "xmax" ];
    m_yminScreen = values[ "ymin" ];
    m_ymaxScreen = values[ "ymax" ];
    m_zvalScreen = values[ "zval" ];
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessKBEvents( int mode )
{
    gadget::KeyboardMouse::EventQueue evt_queue = m_keyboard->getEventQueue();
    gadget::KeyboardMouse::EventQueue::iterator i;

    if( evt_queue.empty() )
    {
        return;
    }

    for( i = evt_queue.begin(); i != evt_queue.end(); i++ )
    {
        const gadget::EventType type = ( *i )->type();

        if( type == gadget::KeyPressEvent )
        {
            gadget::KeyEventPtr m_key_evt = boost::dynamic_pointer_cast< gadget::KeyEvent >( *i );
            m_key = m_key_evt->getKey();

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
        //Use this call if you want to hold a m_key for it to be active
        else if( type == gadget::KeyReleaseEvent )
        {
            _m_key = -1;
        }
        */

        else if( type == gadget::MouseButtonPressEvent )
        {
            gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            m_button = mouse_evt->getButton();
            m_state = 1;
            m_x = mouse_evt->getX();
            m_y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                NavMouse();

                //If in animation mode, stop the animation with mouse press event
                if( m_animate )
                {
                    gmtl::identity( m_deltaTransform );
                    m_deltaTransform.mData[12] = m_deltaTransform.mData[13] = m_deltaTransform.mData[14] = 0.0f;
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
            gadget::MouseEventPtr mouse_evt=boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            m_button = mouse_evt->getButton();
            m_state = 0;
            m_x = mouse_evt->getX();
            m_y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                NavMouse();
            }

            //Selection mode
            else if( mode == 1 )
            {
                SelMouse();
            }
        }

        else if( type == gadget::MouseMoveEvent )
        {
            gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            m_x = mouse_evt->getX();
            m_y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                NavMotion();
            }

            //Selection mode
            else if( mode == 1 )
            {
                SelMotion();
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNavigationEvents()
{
    //Translate world dcs by distance that the m_head is away from the origin
    gmtl::Matrix44f transMat = gmtl::makeTrans< gmtl::Matrix44f >( -*center_point );
    gmtl::Matrix44f worldMatTrans = transMat * m_currentTransform;

    //Get the position of the m_head in the new world space as if the m_head is on the origin
    gmtl::Point3f newJugglerHeadPoint;
    gmtl::Point3f newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
    gmtl::Vec4f newGlobalHeadPointVec;
    newGlobalHeadPointVec[0] = newGlobalHeadPointTemp[0];
    newGlobalHeadPointVec[1] = newGlobalHeadPointTemp[1];
    newGlobalHeadPointVec[2] = newGlobalHeadPointTemp[2];

    //Rotate the m_head vector by the rotation increment
    gmtl::Vec4f rotateJugglerHeadVec = m_deltaTransform * newGlobalHeadPointVec;

    //Split apart the current matrix into rotation and translation parts
    gmtl::Matrix44f accuRotation;
    gmtl::Matrix44f matrix;

    for( int i = 0; i < 3; i++ )
    {
        //Get the current rotation matrix
        accuRotation[i][0] = m_currentTransform[i][0];
        accuRotation[i][1] = m_currentTransform[i][1];
        accuRotation[i][2] = m_currentTransform[i][2];

        //Get the current translation matrix
        matrix[i][3] = rotateJugglerHeadVec[i] + center_point->mData[i];
    }

    //Multiply by the transform and then by the rotation
    matrix *= m_deltaTransform;
    matrix *= accuRotation;

    //Set the current matrix
    activeDCS->SetMat( matrix );

    //If not in animation mode, reset the transform
    if( !m_animate )
    {
        gmtl::identity( m_deltaTransform );
        m_deltaTransform.mData[12] = m_deltaTransform.mData[13] = m_deltaTransform.mData[14] = 0.0f;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Animate( bool animate )
{
    m_animate = animate;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetWindowValues( unsigned int w, unsigned int h )
{
    m_width = w;
    m_height = h;

    m_aspectRatio = float( m_width ) / float( m_height );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetFrustumValues( float l, float r, float t, float b, float n, float f )
{
    /*
    std::cout << "m_leftFrustum: "   << m_leftFrustum       << std::endl;
    std::cout << "m_rightFrustum: "  << m_rightFrustum      << std::endl;
    std::cout << "m_topFrustum: "    << m_topFrustum        << std::endl;
    std::cout << "m_bottomFrustum: " << m_bottomFrustum     << std::endl;
    std::cout << "near: "   << m_nearFrustum << std::endl;
    std::cout << "far: "    << m_farFrustum  << std::endl;
    std::cout <<                             std::endl;
    */

    m_leftFrustum = l;
    m_rightFrustum = r;
    m_topFrustum = t;
    m_bottomFrustum = b;
    m_nearFrustum = n;
    m_farFrustum = f;

    float topAngle = OneEightyDivPI * atan( m_topFrustum / m_nearFrustum );
    float tempDiv = fabs( m_bottomFrustum ) / m_nearFrustum;
    float bottomAngle = OneEightyDivPI * atan( tempDiv );

    m_fovy = topAngle + bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavKeyboard()
{
    if( m_key == gadget::KEY_R )
    {  
        ResetTransforms();
    }

    else if( m_key == gadget::KEY_F )
    {
        FrameAll();
    }

    else if( m_key == gadget::KEY_S )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->StepSimulation();
    }

    else if( m_key == gadget::KEY_SPACE )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->ResetScene();
    }

    //Reset m_key
    m_key = -1;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMouse()
{
    if( m_state == 0 )
    {
        return;
    }

    else if( m_state == 1 )
    {
        m_currPos[0] = float( m_x ) / float( m_width );
        m_currPos[1] = float( m_y ) / float( m_height );
        m_prevPos[0] = float( m_x ) / float( m_width );
        m_prevPos[1] = float( m_y ) / float( m_height );
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMotion()
{
    if( m_state == 0 )
    {
        return;
    }

    m_currentTransform = activeDCS->GetMat();

    m_currPos[0] = float( m_x ) / float( m_width );
    m_currPos[1] = float( m_y ) / float( m_height );
    float dx = m_currPos[0] - m_prevPos[0];
    float dy = m_currPos[1] - m_prevPos[1];

    m_magnitude = sqrtf( dx * dx + dy * dy );
    if( m_magnitude < m_sensitivity )
    {
        return;
    }

    if( m_button == gadget::MBUTTON1 
        && ( m_x > 0.1f * m_width )
        && ( m_x < 0.9f * m_width )
        && ( m_y > 0.1f * m_height )
        && ( m_y < 0.9f * m_height ) )
    {
        RotateView( dx, dy );
    }

    else if( m_button == gadget::MBUTTON3 )
    {
        Zoom( dy );
    }

    else if( m_button == gadget::MBUTTON2 )
    {
        Pan( dx, dy );
    }

    else if( m_button == gadget::MBUTTON1 )
    {
        Twist( dx, dy );
    }

    m_prevPos[0] = m_currPos[0];
    m_prevPos[1] = m_currPos[1];

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
    if( m_state == 1 )
    {
        return;
    }

    else if( m_state == 0 && m_button == gadget::MBUTTON1 )
    {
        ProcessSelectionEvents();
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelMotion()
{
    if( !m_state )
    {
        return;
    }

    if( m_button == gadget::MBUTTON1 )
    {
        //osg::ref_ptr< osg::Geometry > selection_rectangle = new osg::Geometry;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    gmtl::Matrix44f matrix;
    center_point->mData[1] = matrix[1][3] = *m_threshold;

    VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->SetMat( matrix );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
    osg::ref_ptr< osg::Group > root = new osg::Group;
    root->addChild( VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode() );

    m_currentTransform = VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode()->GetMat();

    //Get the selected objects and expand by their bounding box
    float Theta = ( m_fovy * 0.5f ) * PIDivOneEighty;
    osg::BoundingSphere bs = root->computeBound();

    float x = bs.center().x();
    m_currentTransform.mData[12] -= x;

    float y;
    if( m_aspectRatio <= 1.0f )
    {
        y = ( bs.radius() / tan( Theta ) ) * m_aspectRatio;
    }
    else
    {
        y = bs.radius() / tan( Theta );
    }

    float delta_y_val = y - m_currentTransform.mData[13];
    m_currentTransform.mData[13] = y;
    center_point->mData[1] += delta_y_val;

    float z = bs.center().z();
    m_currentTransform.mData[14] -= z;

    VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode()->SetMat( m_currentTransform );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RotateView( float dx, float dy )
{
    gmtl::Matrix44f mat;
    float tb_axis[3];

    gmtl::identity( mat );
    float angle = m_magnitude * 400.0f;

    tb_axis[0] = mat[0][0] * dy + mat[2][0] * dx;
    tb_axis[1] = mat[0][1] * dy + mat[2][1] * dx;
    tb_axis[2] = mat[0][2] * dy + mat[2][2] * dx;

    Rotate( tb_axis[0], tb_axis[1], tb_axis[2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist( float dx, float dy )
{
    gmtl::Matrix44f mat;
    gmtl::identity( mat );

    float Theta = atan2f( m_prevPos[0] - 0.5, m_prevPos[1] - 0.5 );
    float newTheta = atan2f( m_currPos[0] - 0.5, m_currPos[1] - 0.5 );
    float angle = ( OneEightyDivPI ) * ( Theta - newTheta );

    Rotate( mat[1][0], mat[1][1], mat[1][2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( float dy )
{
    float viewlength = center_point->mData[1];
    float d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;

    m_deltaTransform.mData[13] = d;

    center_point->mData[1] += d;

    if( center_point->mData[1] < *m_threshold )
    {
        center_point->mData[1] = *m_jump;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( float dx, float dy )
{
    float d = m_currentTransform.mData[13];
    float theta = ( m_fovy * 0.5f ) * ( PIDivOneEighty );
    float b = 2 * d * tan( theta );
    float dwx = dx * b * m_aspectRatio;
    float dwy = -dy * b;

    m_deltaTransform.mData[12] = dwx;
    m_deltaTransform.mData[14] = dwy;

    center_point->mData[0] += dwx;
    center_point->mData[2] += dwy;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( float x, float y, float z, float angle )
{
    float rad = angle * PIDivOneEighty;
    float cosAng = cos( rad );
    float sinAng = sin( rad );
    float denom = sqrtf( x * x + y * y + z * z );

    if( denom != 0.0f )
    {
        x /= denom;
        y /= denom;
        z /= denom;
    }

    gmtl::zero( m_deltaTransform );

    m_deltaTransform.mData[0]  = ( x * x ) + ( cosAng * ( 1 - ( x * x ) ) );
    m_deltaTransform.mData[1]  = ( y * x ) - ( cosAng *       ( y * x ) ) + ( sinAng * z );
    m_deltaTransform.mData[2]  = ( z * x ) - ( cosAng *       ( z * x ) ) - ( sinAng * y );
    m_deltaTransform.mData[4]  = ( x * y ) - ( cosAng *       ( x * y ) ) - ( sinAng * z );
    m_deltaTransform.mData[5]  = ( y * y ) + ( cosAng * ( 1 - ( y * y ) ) );
    m_deltaTransform.mData[6]  = ( z * y ) - ( cosAng *       ( z * y ) ) + ( sinAng * x );
    m_deltaTransform.mData[8]  = ( x * z ) - ( cosAng *       ( x * z ) ) + ( sinAng * y );
    m_deltaTransform.mData[9]  = ( y * z ) - ( cosAng *       ( y * z ) ) - ( sinAng * x );
    m_deltaTransform.mData[10] = ( z * z ) + ( cosAng * ( 1 - ( z * z ) ) );
    m_deltaTransform.mData[15] = 1.0f;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessSelectionEvents()
{
    osg::Vec3f startPoint, endPoint;
    SetStartEndPoint( &startPoint, &endPoint );

    //DrawLine( startPoint, endPoint);

    beamLineSegment->set(startPoint, endPoint);

    osgUtil::IntersectVisitor objectBeamIntersectVisitor;
    objectBeamIntersectVisitor.addLineSegment( beamLineSegment.get() );

    //Add the IntersectVisitor to the root Node so that all all geometry will be
    //checked and no transforms are done to the Line segement.
    VE_SceneGraph::SceneManager::instance()->GetRootNode()->accept( objectBeamIntersectVisitor );

    osgUtil::IntersectVisitor::HitList beamHitList;
    beamHitList = objectBeamIntersectVisitor.getHitList( beamLineSegment.get() );

    ProcessHit( beamHitList );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{ 
    osgUtil::Hit objectHit;
    selectedGeometry = 0;

    if( listOfHits.empty() )
    {
        vprDEBUG(vesDBG,1) << "|\tKeyboardMouse::ProcessHit No object selected" 
                           << std::endl << vprDEBUG_FLUSH;

        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();

        return;
    }

    //Search for first item that is not the laser
    for( size_t i = 0; i <  listOfHits.size(); ++i )
    {
        objectHit = listOfHits[ i ];
        if( ( objectHit._geode->getName() != "Laser" ) && ( objectHit._geode->getName() != "Root Node" ) ) 
        {
            break;
        }
    }

    //Make sure it is good
    if( !objectHit._geode.valid() )
    {
        return;
    } 

    //Now find the id for the cad
    selectedGeometry = objectHit._geode;
    VE_SceneGraph::FindParentsVisitor parentVisitor( selectedGeometry.get() );
    osg::ref_ptr< osg::Node > parentNode = parentVisitor.GetParentNode();
    if( parentNode.valid() )
    {
        vprDEBUG(vesDBG,1) << "|\tObjects has name " 
                           << parentNode->getName() << std::endl 
                           << vprDEBUG_FLUSH;
        vprDEBUG(vesDBG,1) << "|\tObjects descriptors " 
                           << parentNode->getDescriptions().at( 1 ) << std::endl 
                           << vprDEBUG_FLUSH;

        activeDCS = dynamic_cast< VE_SceneGraph::DCS* >( parentNode.get() );
    }

    else
    {
        selectedGeometry = objectHit._geode;
        vprDEBUG(vesDBG,1) << "|\tObject does not have name parent name " 
                           << objectHit._geode->getParents().front()->getName() 
                           << std::endl << vprDEBUG_FLUSH;

        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
    }

    osg::Vec3 nodeCenterWorldCoords( 0, 0, 0 );

    if( activeDCS->GetName() != "World DCS" )
    {
        osg::ref_ptr< VE_SceneGraph::Utilities::LocalToWorldNodePath > nv = new VE_SceneGraph::Utilities::LocalToWorldNodePath( activeDCS.get() );
        
        osg::Matrix toWorldTransform;
        toWorldTransform.identity();
        toWorldTransform = osg::computeLocalToWorld( nv->GetNodePath() );

        osg::Matrix localDCS;
        localDCS.identity();
        localDCS.setTrans( activeDCS->getPosition() );
        localDCS.setRotate( activeDCS->getAttitude() );

        nodeCenterWorldCoords = activeDCS->getBound().center() * osg::Matrix::inverse( localDCS ) * toWorldTransform;
    }
    /*
    else
    {
        nodeCenterWorldCoords = activeDCS->getBound().center();
    }
    */

    center_point->set( nodeCenterWorldCoords.x(), nodeCenterWorldCoords.y(), nodeCenterWorldCoords.z() );

    if( center_point->mData[1] < *m_threshold )
    {
        center_point->mData[1] = *m_jump;
    }
}
////////////////////////////////////////////////////////////////////////////////
