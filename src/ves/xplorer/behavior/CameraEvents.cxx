/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#include <ves/xplorer/behavior/CameraEvents.h>
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
CameraEvents::CameraEvents()
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
    //Setup connection to mouse events
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.MouseMove", bool( int, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMouseMove,
        m_connections, any_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress1%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMousePress,
        m_connections, any_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress2%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMousePress,
        m_connections, any_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonPress3%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMousePress,
        m_connections, any_SignalType, highest_Priority );    

    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease1%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMouseRelease,
        m_connections, any_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease2%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMouseRelease,
        m_connections, any_SignalType, highest_Priority );
    
    CONNECTSIGNALS_4_COMBINER( "KeyboardMouse.ButtonRelease3%", bool( gadget::Keys, int, int, int ), 
        eventmanager::BooleanPropagationCombiner, &CameraEvents::ProcessMouseRelease,
        m_connections, any_SignalType, highest_Priority );

    CONNECTSIGNALS_2( "Wand.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), 
        &CameraEvents::SetStartEndPoint,
        m_connections, any_SignalType, normal_Priority );

    CONNECTSIGNALS_2( "KeyboardMouse.StartEndPoint", void( osg::Vec3d, osg::Vec3d ), 
        &CameraEvents::SetStartEndPoint,
        m_connections, any_SignalType, normal_Priority );
}
////////////////////////////////////////////////////////////////////////////////
CameraEvents::~CameraEvents()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraEvents::ProcessMousePress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{        
    m_currX = xPos;
    m_currY = yPos;
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraEvents::ProcessMouseRelease( gadget::Keys buttonKey, int xPos, int yPos, int buttonState )
{
    if( (xPos > m_currX + 2) || (xPos < m_currX - 2) )
    {
        return false;
    }
    
    if( (yPos > m_currY + 2) || (yPos < m_currY - 2) )
    {
        return false;
    }
    
    m_currX = xPos;
    m_currY = yPos;

    switch( buttonKey )
    {
            //Left mouse button
        case gadget::MBUTTON1:
        {
            if( m_cameraManager.IsEnabled() )
            {
                UpdateSelectionLine();
                //If we found a camera
                if( m_cameraManager.Handle(
                                           scenegraph::camera::Event::RELEASE,
                                           *m_lineSegmentIntersector.get() ) )
                {
                    return true;
                }
                
                 osgUtil::LineSegmentIntersector::Intersections& intersections =
                scenegraph::TestForIntersections(
                                                 *m_lineSegmentIntersector.get(),
                                                 *m_sceneManager.GetModelRoot() );
                                                
                break;
            }
                        
            break;
        }
            //Middle mouse button
        case gadget::MBUTTON2:
        {
            if( m_cameraManager.IsEnabled() )
            {
                UpdateSelectionLine();
                if( m_cameraManager.Handle(
                                           scenegraph::camera::Event::RELEASE,
                                           *m_lineSegmentIntersector.get() ) )
                {
                    return true;
                }
            }
            break;
        }
            //Right mouse button
        case gadget::MBUTTON3:
        {
            if( m_cameraManager.IsEnabled() )
            {
                UpdateSelectionLine();
                if( m_cameraManager.Handle(
                                           scenegraph::camera::Event::RELEASE,
                                           *m_lineSegmentIntersector.get() ) )
                {
                    return true;
                }
            }
            break;
        }
        default:
        {
            ;
        }
    } //end switch( m_currKey )
    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraEvents::ProcessMouseMove( int xPos, int yPos, int zPos, int buttonState )
{
    if( buttonState == 0 )
    {
        if( m_cameraManager.IsEnabled() )
        {
            UpdateSelectionLine();
            if( m_cameraManager.Handle(
                                       scenegraph::camera::Event::FOCUS,
                                       *m_lineSegmentIntersector.get() ) )
            {
                return true;
            }
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEvents::UpdateSelectionLine()
{
    //std::cout << "update selection line " << std::endl;
    //osg::Vec3d startPoint, endPoint;
    //SetStartEndPoint( startPoint, endPoint );
    m_lineSegmentIntersector->reset();
    m_lineSegmentIntersector->setStart( m_startPoint );
    m_lineSegmentIntersector->setEnd( m_endPoint );
    
    //Used to debug the selection line
    //If working correctly, the line should show up as 1 red pixel where picked
    //DrawLine( startPoint, endPoint );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEvents::SetStartEndPoint( osg::Vec3d startPoint, osg::Vec3d endPoint )
{
    m_startPoint = startPoint;
    m_endPoint = endPoint;
}
////////////////////////////////////////////////////////////////////////////////
}
}
}
