/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#include <ves/xplorer/behavior/ConstraintSelection.h>
#include <ves/xplorer/EnvironmentHandler.h>
#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/ModelCADHandler.h>
#include <ves/xplorer/DeviceHandler.h>
#include <ves/xplorer/Model.h>

#include <ves/xplorer/network/GraphicalPluginManager.h>

#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

#include <ves/xplorer/scenegraph/SceneManager.h>
#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/scenegraph/physics/PhysicsSimulator.h>
#include <ves/xplorer/scenegraph/physics/character/CharacterController.h>

#include <ves/xplorer/scenegraph/camera/CameraManager.h>

#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>

#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>

#include <ves/xplorer/scenegraph/Select.h>
#include <ves/xplorer/scenegraph/GLTransformInfo.h>

//OSG
#include <osg/BoundingSphere>
#include <osg/Vec3d>
#include <osg/Matrix>
#include <osg/Group>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/LineWidth>
#include <osg/AutoTransform>
#include <osg/io_utils>

//GMTL
#include <gmtl/Matrix.h>
#include <gmtl/AxisAngle.h>
#include <gmtl/Generate.h>
#include <gmtl/Misc/MatrixConvert.h>

// --- Bullet Includes --- //
#include <LinearMath/btVector3.h>

#include <BulletDynamics/Dynamics/btDynamicsWorld.h>

#include <BulletDynamics/ConstraintSolver/btPoint2PointConstraint.h>

namespace ves
{
namespace xplorer
{
namespace behavior
{
////////////////////////////////////////////////////////////////////////////////
ConstraintSelection::ConstraintSelection()
    :
    m_physicsSimulator( *ves::xplorer::scenegraph::PhysicsSimulator::instance() ),
    m_sceneManager( *ves::xplorer::scenegraph::SceneManager::instance() ),
    m_characterController( m_sceneManager.GetCharacterController() ),
    m_manipulatorManager( m_sceneManager.GetManipulatorManager() ),
    m_cameraManager( m_sceneManager.GetCameraManager() ),
    m_lineSegmentIntersector( new osgUtil::LineSegmentIntersector(
        osg::Vec3( 0.0, 0.0, 0.0 ), osg::Vec3( 0.0, 0.0, 0.0 ) ) ),
    m_currX( 0 ),
    m_currY( 0 ),
    m_pickedBody( 0 ),
    m_pickConstraint( 0 )
{    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease1%", bool( gadget::Keys, int, int, int ),
                      eventmanager::BooleanPropagationCombiner, &ConstraintSelection::ProcessSelection,
                      m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress1%", bool( gadget::Keys, int, int, int ),
                     eventmanager::BooleanPropagationCombiner, &ConstraintSelection::RegisterButtonPress,
                     m_connections, any_SignalType, normal_Priority );
    
    CONNECTSIGNALS_2( "KeyboardMouse.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), &ConstraintSelection::SetStartEndPoint,
                     m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.MouseMove", bool( int, int, int, int ),
                     eventmanager::BooleanPropagationCombiner, &ConstraintSelection::ProcessNavigation,
                     m_connections, any_SignalType, normal_Priority );

    eventmanager::EventManager::instance()->RegisterSignal(
        new eventmanager::SignalWrapper< ObjectPickedSignal_type >( &m_objectPickedSignal ),
        "ConstraintSelection.ObjectPickedSignal" );
}
////////////////////////////////////////////////////////////////////////////////
ConstraintSelection::~ConstraintSelection()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstraintSelection::RegisterButtonPress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{    
    if( buttonState&gadget::KEY_SHIFT )
    {
        CreatePointConstraint();
        return true;
    }

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstraintSelection::ProcessSelection( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    //Do not require mod key depending on what the user did
    ClearPointConstraint();
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool ConstraintSelection::ProcessNavigation( int xPos, int yPos, int zPos, int buttonState )
{
    if( buttonState == 0 )
    {
        return false;
    }

    //For KBM the shift madifier is used to control if we want selection
    if( buttonState&gadget::KEY_SHIFT )
    {
        UpdatePointConstraint();
        return true;
    }
    //For wand input we should do something with multiple button inputs
    //for example require pressing both button 0 and something else
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintSelection::ClearPointConstraint()
{
    //Do not require mod key depending on what the user did
    if( m_pickConstraint )
    {
        m_physicsSimulator.GetDynamicsWorld()->
            removeConstraint( m_pickConstraint );

        delete m_pickConstraint;
        m_pickConstraint = NULL;
        
        m_pickedBody->forceActivationState( ACTIVE_TAG );
        m_pickedBody->setDeactivationTime( 0.0 );
        m_pickedBody = NULL;
    }
}
////////////////////////////////////////////////////////////////////////////////
bool ConstraintSelection::CreatePointConstraint()
{
    //Add a point to point constraint for picking
    if( m_physicsSimulator.GetIdle() )
    {
        return false;
    }
    
    btVector3 rayFromWorld, rayToWorld;
    rayFromWorld.setValue( m_startPoint.x(), m_startPoint.y(), m_startPoint.z() );
    rayToWorld.setValue( m_endPoint.x(), m_endPoint.y(), m_endPoint.z() );
    
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
        m_pickedBody = body;
        m_pickedBody->setActivationState( DISABLE_DEACTIVATION );
        
        btVector3 pickPos = rayCallback.m_hitPointWorld;
        
        btVector3 localPivot =
        body->getCenterOfMassTransform().inverse() * pickPos;
        
        btPoint2PointConstraint* p2p =
        new btPoint2PointConstraint( *body, localPivot );
        m_physicsSimulator.GetDynamicsWorld()->addConstraint( p2p );
        m_pickConstraint = p2p;
        
        m_prevPhysicsRayPos = ( pickPos - rayFromWorld ).length();
        
        //Very weak constraint for picking
        p2p->m_setting.m_tau = 0.1;
    }
    
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintSelection::UpdatePointConstraint()
{
    if( !m_physicsSimulator.GetIdle() && m_pickConstraint )
    {
        //Move the constraint pivot
        btPoint2PointConstraint* p2p =
        static_cast< btPoint2PointConstraint* >( m_pickConstraint );
        if( p2p )
        {
            btVector3 rayFromWorld, rayToWorld;
            rayFromWorld.setValue(
                m_startPoint.x(), m_startPoint.y(), m_startPoint.z() );
            rayToWorld.setValue( m_endPoint.x(), m_endPoint.y(), m_endPoint.z() );
            
            //Keep it at the same picking distance
            btVector3 dir = rayToWorld - rayFromWorld;
            dir.normalize();
            dir *= m_prevPhysicsRayPos;
            
            btVector3 newPos = rayFromWorld + dir;
            p2p->setPivotB( newPos );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void ConstraintSelection::SetStartEndPoint( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    m_startPoint = startPoint;
    m_endPoint = endPoint;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
