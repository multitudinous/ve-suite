/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Stuff --- //
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

#include "VE_Xplorer/SceneGraph/SceneManager.h"
#include "VE_Xplorer/SceneGraph/FindParentsVisitor.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"
#include "VE_Xplorer/SceneGraph/Group.h"

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
#include <osg/BoundingSphere>
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
key( -1 ),
button( -1 ),
state( -1 ),
x( 0 ),
y( 0 ),

width( 1 ),
height( 1 ),

aspect_ratio( 0.0f ),
fovy( 0.0f ),
left( 0.0f ),
right( 0.0f ),
top( 0.0f ),
bottom( 0.0f ),
near_plane( 0.0f ),
far_plane( 0.0f ),

wc_screen_xmin( 0.0f ),
wc_screen_xmax( 0.0f ),
wc_screen_ymin( 0.0f ),
wc_screen_ymax( 0.0f ),
wc_screen_zval( 0.0f ),

tb_magnitude( 0.0f ),
tb_sensitivity( 1.0e-06 ),
tb_threshold( 0.5f ),
tb_jump( 1.0f ),

tb_animate( false ),
tb_mode( false ),

beamLineSegment( new osg::LineSegment )
{
    mKeyboard.init( "VJKeyboard" );
    head.init( "VJHead" );

    tb_currPos[0] = 0.0f;
    tb_currPos[1] = 0.0f;
    tb_prevPos[0] = 0.0f;
    tb_prevPos[1] = 0.0f;

    gmtl::identity( tb_transform );
    gmtl::identity( tb_currTransform );
    //gmtl::identity( tb_worldTransform );

    activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::~KeyboardMouse()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateNavigation()
{
    this->ProcessKBEvents( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelection()
{
    this->ProcessKBEvents( 1 );
}
////////////////////////////////////////////////////////////////////////////////
float KeyboardMouse::GetCPThreshold()
{
    return tb_threshold;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetStartEndPoint( osg::Vec3f* startPoint, osg::Vec3f* endPoint )
{
    //Be sure width and height are set before calling this function
    double wc_x_trans_ratio = ( ( wc_screen_xmax - wc_screen_xmin ) ) / double( width );
    double wc_y_trans_ratio = ( ( wc_screen_ymax - wc_screen_ymin ) ) / double( height );

    screenRatios = std::pair< double, double >( wc_x_trans_ratio, wc_y_trans_ratio );

    double transformedPosition[3];
    double osgTransformedPosition[3];
    transformedPosition[0] = wc_screen_xmin + ( x * screenRatios.first );
    transformedPosition[1] = wc_screen_ymax - ( y * screenRatios.second );
    transformedPosition[2] = wc_screen_zval;

    transformedPosition[0] *= 3.2808399;
    transformedPosition[1] *= 3.2808399;
    transformedPosition[2] *= 3.2808399;

    osgTransformedPosition[0] =  transformedPosition[0];
    osgTransformedPosition[1] = -transformedPosition[2];
    osgTransformedPosition[2] =  transformedPosition[1];

    /*
    std::cout << " x location = " << x << std::endl 
              << " y location = " << y << std::endl 
              << " location = " << osgTransformedPosition[0] << " " << osgTransformedPosition[1] << " " << osgTransformedPosition[2] << std::endl
              << " ratio = " << screenRatios.first << " " << screenRatios.second << std::endl
              << " screen values = " << wc_screen_xmin << " " << wc_screen_ymin << " " << wc_screen_zval << std::endl
              << std::endl;

    std::cout << " x " << wc_screen_xmin << " " << wc_screen_xmax 
              << " y " << wc_screen_ymin << " " << wc_screen_ymax 
              << " z " << wc_screen_zval 
              << " width " << width << " height "<< height << std::endl;
    */

    double wandEndPoint[3];
    double distance = 10000.0f;

    gmtl::Matrix44f vjHeadMat = head->getData();
    // get juggler Matrix of worldDCS
    // Note:: for pf we are in juggler land
    //        for osg we are in z up land
    gmtl::Point3d jugglerHeadPoint, jugglerHeadPointTemp;
    jugglerHeadPoint = gmtl::makeTrans< gmtl::Point3d >( vjHeadMat );
    //We have to offset negative x because the view is being drawn for the left 
    //eye which means the the frustums are being setup for the left eye
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
    osg::ref_ptr< osg::StateSet > stateset = new osg::StateSet;

    vertices->push_back( startPoint );
    vertices->push_back( endPoint );
    line->setVertexArray( vertices.get() );

    colors->push_back( osg::Vec4( 1.0f, 0.0f, 0.0f, 1.0f ) );
    line->setColorArray( colors.get() );
    line->setColorBinding( osg::Geometry::BIND_OVERALL );

    osg::ref_ptr< osg::LineWidth > linewidth = new osg::LineWidth;
    linewidth->setWidth( 4.0f );
    stateset->setAttribute( linewidth.get() );
    line->setStateSet( stateset.get() );

    line->addPrimitiveSet( new osg::DrawArrays( osg::PrimitiveSet::LINES, 0, vertices->size() ) );

    beamGeode->addDrawable( line.get() );      

    VE_SceneGraph::SceneManager::instance()->GetRootNode()->addChild( beamGeode.get() );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetScreenCornerValues( std::map< std::string, double > values )
{
    wc_screen_xmin = values[ "xmin" ];
    wc_screen_xmax = values[ "xmax" ];
    wc_screen_ymin = values[ "ymin" ];
    wc_screen_ymax = values[ "ymax" ];
    wc_screen_zval = values[ "zval" ];
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

    for( i = evt_queue.begin(); i != evt_queue.end(); i++ )
    {
        const gadget::EventType type = ( *i )->type();

        if( type == gadget::KeyPressEvent )
        {
            gadget::KeyEventPtr key_evt = boost::dynamic_pointer_cast< gadget::KeyEvent >( *i );
            key = key_evt->getKey();

            //Navigation mode
            if( mode == 0 )
            {
                this->NavKeyboard();
            }

            //Selection mode
            else if( mode == 1 )
            {
                this->SelKeyboard();
            }
        }

        /*
        //Use this call if you want to hold a key for it to be active
        else if( type == gadget::KeyReleaseEvent )
        {
            _key = -1;
        }
        */

        else if( type == gadget::MouseButtonPressEvent )
        {
            gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            button = mouse_evt->getButton();
            state = 1;
            x = mouse_evt->getX();
            y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                this->NavMouse();

                //If in animation mode, stop the animation with mouse press event
                if( tb_animate )
                {
                    gmtl::identity( tb_transform );
                    tb_transform[0][3] = tb_transform[1][3] = tb_transform[2][3] = 0.0f;
                }
            }

            //Selection mode
            else if( mode == 1 )
            {
                this->SelMouse();
            }
        }

        else if( type == gadget::MouseButtonReleaseEvent )
        {
            gadget::MouseEventPtr mouse_evt=boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            button = mouse_evt->getButton();
            state = 0;
            x = mouse_evt->getX();
            y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                this->NavMouse();
            }

            //Selection mode
            else if( mode == 1 )
            {
                this->SelMouse();
            }
        }

        else if( type == gadget::MouseMoveEvent )
        {
            gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
            x = mouse_evt->getX();
            y = mouse_evt->getY();

            //Navigation mode
            if( mode == 0 )
            {
                this->NavMotion();
            }

            //Selection mode
            else if( mode == 1 )
            {
                this->SelMotion();
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessNavigationEvents()
{
    //Translate world dcs by distance that the head is away from the origin
    gmtl::Matrix44f transMat = gmtl::makeTrans< gmtl::Matrix44f >( -*center_point );
    gmtl::Matrix44f worldMatTrans = transMat * VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->GetMat();

    //Get the position of the head in the new world space as if the head is on the origin
    gmtl::Point3f newJugglerHeadPoint;
    gmtl::Point3f newGlobalHeadPointTemp = worldMatTrans * newJugglerHeadPoint;
    gmtl::Vec4f newGlobalHeadPointVec;
    newGlobalHeadPointVec[0] = newGlobalHeadPointTemp[0];
    newGlobalHeadPointVec[1] = newGlobalHeadPointTemp[1];
    newGlobalHeadPointVec[2] = newGlobalHeadPointTemp[2];

    //Rotate the head vector by the rotation increment
    gmtl::Vec4f rotateJugglerHeadVec = tb_transform * newGlobalHeadPointVec;

    //Split apart the current matrix into rotation and translation parts
    gmtl::Matrix44f accuRotation;
    gmtl::Matrix44f matrix;

    for( int i = 0; i < 3; i++ )
    {
        //Get the current rotation matrix
        accuRotation[i][0] = tb_currTransform[i][0];
        accuRotation[i][1] = tb_currTransform[i][1];
        accuRotation[i][2] = tb_currTransform[i][2];

        //Get the current translation matrix
        matrix[i][3] = rotateJugglerHeadVec[i] + center_point->mData[i];
    }

    //Multiply by the transform and then by the rotation
    matrix *= tb_transform;
    matrix *= accuRotation;

    //Set the current matrix
    activeDCS->SetMat( matrix );

    //If not in animation mode, reset the transform
    if( !tb_animate )
    {
        gmtl::identity( tb_transform );
        tb_transform[0][3] = tb_transform[1][3] = tb_transform[2][3] = 0.0f;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Animate( bool animate )
{
    tb_animate = animate;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetWindowValues( unsigned int w, unsigned int h )
{
    width = w;
    height = h;

    aspect_ratio = (float)width / (float)height;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetFrustumValues( float l, float r, float t, float b, float n, float f )
{
    /*
    std::cout << "left: "   << left       << std::endl;
    std::cout << "right: "  << right      << std::endl;
    std::cout << "top: "    << top        << std::endl;
    std::cout << "bottom: " << bottom     << std::endl;
    std::cout << "near: "   << near_plane << std::endl;
    std::cout << "far: "    << far_plane  << std::endl;
    std::cout <<                             std::endl;
    */

    left = l;
    right = r;
    top = t;
    bottom = b;
    near_plane = n;
    far_plane = f;

    float topAngle = OneEightyDivPI * atan( top / near_plane );
    float tempDiv = fabs( bottom ) / near_plane;
    float bottomAngle = OneEightyDivPI * atan( tempDiv );

    fovy = topAngle + bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavKeyboard()
{
    if( key == gadget::KEY_R )
    {  
        ResetTransforms();
    }

    else if( key == gadget::KEY_F )
    {
        FrameAll();
    }

    else if( key == gadget::KEY_S )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->StepSimulation();
    }

    else if( key == gadget::KEY_SPACE )
    {
        VE_SceneGraph::PhysicsSimulator::instance()->ResetScene();
    }

    //Reset key
    key = -1;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMouse()
{
    if( state == 0 )
    {
        return;
    }

    else if( state == 1 )
    {
        tb_currPos[0] = (float)x / (float)width;
        tb_currPos[1] = (float)y / (float)height;
        tb_prevPos[0] = (float)x / (float)width;
        tb_prevPos[1] = (float)y / (float)height;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::NavMotion()
{
    if( state == 0 )
    {
        return;
    }

    //Get the current matrix and world matrix
    tb_currTransform = activeDCS->GetMat();
    //tb_worldTransform = VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->GetMat();

    tb_currPos[0] = (float)x / (float)width;
    tb_currPos[1] = (float)y / (float)height;
    float dx = tb_currPos[0] - tb_prevPos[0];
    float dy = tb_currPos[1] - tb_prevPos[1];

    tb_magnitude = sqrtf( dx * dx + dy * dy );
    if( tb_magnitude < tb_sensitivity )
    {
        return;
    }

    if( button == gadget::MBUTTON1 
        && ( x > 0.1f * width )
        && ( x < 0.9f * width )
        && ( y > 0.1f * height )
        && ( y < 0.9f * height ) )
    {
        RotateView( dx, dy );
    }

    else if( button == gadget::MBUTTON3 )
    {
        Zoom( dy );
    }

    else if( button == gadget::MBUTTON2 )
    {
        Pan( dx, dy );
    }

    else if( button == gadget::MBUTTON1 )
    {
        Twist( dx, dy );
    }

    tb_prevPos[0] = tb_currPos[0];
    tb_prevPos[1] = tb_currPos[1];

    this->ProcessNavigationEvents();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelKeyboard()
{
    return;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelMouse()
{
    if( state == 1 )
    {
        return;
    }

    else if( state == 0 && button == gadget::MBUTTON1 )
    {
        ProcessSelectionEvents();
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SelMotion()
{
    if( !state )
    {
        return;
    }

    if( button == gadget::MBUTTON1 )
    {
        //osg::ref_ptr< osg::Geometry > selection_rectangle = new osg::Geometry;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
    gmtl::Matrix44f matrix;
    center_point->mData[1] = matrix[1][3] = tb_threshold;

    VE_SceneGraph::SceneManager::instance()->GetWorldDCS()->SetMat( matrix );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
    osg::ref_ptr< osg::Group > root = new osg::Group;
    root->addChild( VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode() );

    tb_currTransform = VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode()->GetMat();

    //Get the selected objects and expand by their bounding box
    osg::BoundingSphere bs;
    for( unsigned int i = 0; i < root->getNumChildren(); i++ )
    {
        bs.expandBy( root->getChild( i )->getBound() );
    }

    float Theta = ( fovy * 0.5f ) * PIDivOneEighty;
    float x_val = bs.center().x();

    float y_val;
    if( aspect_ratio <= 1.0f )
    {
        y_val = ( bs.radius() / tan( Theta ) ) * aspect_ratio;
    }
    else
    {
        y_val = bs.radius() / tan( Theta );
    }

    float z_val = bs.center().z();

    if( bs.center().x() != 0 )
    {
        tb_currTransform[0][3] -= x_val;
    }

    tb_currTransform[1][3] = y_val;

    if( bs.center().z() != 0 )
    {
        tb_currTransform[2][3] -= z_val;
    }

    VE_SceneGraph::SceneManager::instance()->GetActiveSwitchNode()->SetMat( tb_currTransform );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RotateView( float dx, float dy )
{
    gmtl::Matrix44f mat;
    float tb_axis[3];

    identity( mat );
    float angle = tb_magnitude * 400.0f;

    tb_axis[0] = mat[0][0] * dy + mat[2][0] * dx;
    tb_axis[1] = mat[0][1] * dy + mat[2][1] * dx;
    tb_axis[2] = mat[0][2] * dy + mat[2][2] * dx;

    Rotate( tb_axis[0], tb_axis[1], tb_axis[2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist( float dx, float dy )
{
    gmtl::Matrix44f mat;
    identity( mat );

    float Theta = atan2f( tb_prevPos[0] - 0.5, tb_prevPos[1] - 0.5 );
    float newTheta = atan2f( tb_currPos[0] - 0.5, tb_currPos[1] - 0.5 );
    float angle = ( OneEightyDivPI ) * ( Theta - newTheta );

    Rotate( mat[1][0], mat[1][1], mat[1][2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( float dy )
{
    float viewlength = center_point->mData[1];
    float d = ( viewlength * ( 1 / ( 1 + dy * 2 ) ) ) - viewlength;

    tb_transform[1][3] = d;

    center_point->mData[1] += d;

    if( center_point->mData[1] < tb_threshold )
    {
        center_point->mData[1] = tb_jump;
    }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( float dx, float dy )
{
    float d = tb_currTransform[1][3];
    float theta = ( fovy * 0.5f ) * ( PIDivOneEighty );
    float b = 2 * d * tan( theta );
    float dwx = dx * b * aspect_ratio;
    float dwy = -dy * b;

    tb_transform[0][3] = dwx;
    tb_transform[2][3] = dwy;

    center_point->mData[0] += dwx;
    center_point->mData[2] += dwy;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( float x_val, float y_val, float z_val, float angle )
{
    float rad = (float)( angle * PIDivOneEighty );
    float cosAng = (float)( cos( rad ) );
    float sinAng = (float)( sin( rad ) );
    float denom = sqrtf( x_val * x_val + y_val * y_val + z_val * z_val );

    if( denom != 0.0f )
    {
        x_val /= denom;
        y_val /= denom;
        z_val /= denom;
    }

    zero( tb_transform );

    tb_transform[0][0] = ( x_val * x_val ) + ( cosAng * ( 1 - ( x_val * x_val ) ) );
    tb_transform[1][0] = ( y_val * x_val ) - ( cosAng *       ( y_val * x_val ) ) + ( sinAng * z_val );
    tb_transform[2][0] = ( z_val * x_val ) - ( cosAng *       ( z_val * x_val ) ) - ( sinAng * y_val );
    tb_transform[0][1] = ( x_val * y_val ) - ( cosAng *       ( x_val * y_val ) ) - ( sinAng * z_val );
    tb_transform[1][1] = ( y_val * y_val ) + ( cosAng * ( 1 - ( y_val * y_val ) ) );
    tb_transform[2][1] = ( z_val * y_val ) - ( cosAng *       ( z_val * y_val ) ) + ( sinAng * x_val );
    tb_transform[0][2] = ( x_val * z_val ) - ( cosAng *       ( x_val * z_val ) ) + ( sinAng * y_val );
    tb_transform[1][2] = ( y_val * z_val ) - ( cosAng *       ( y_val * z_val ) ) - ( sinAng * x_val );
    tb_transform[2][2] = ( z_val * z_val ) + ( cosAng * ( 1 - ( z_val * z_val ) ) );
    tb_transform[3][3] = 1.0f;
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

    this->ProcessHit( beamHitList );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessHit( osgUtil::IntersectVisitor::HitList listOfHits )
{ 
    osgUtil::Hit objectHit;
    this->selectedGeometry = 0;

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
    this->selectedGeometry = objectHit._geode;
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
        this->selectedGeometry = objectHit._geode;
        vprDEBUG(vesDBG,1) << "|\tObject does not have name parent name " 
                           << objectHit._geode->getParents().front()->getName() 
                           << std::endl << vprDEBUG_FLUSH;
        activeDCS = VE_SceneGraph::SceneManager::instance()->GetWorldDCS();
    }
}
////////////////////////////////////////////////////////////////////////////////
