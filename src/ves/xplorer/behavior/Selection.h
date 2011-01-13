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
#pragma once

#include <ves/VEConfig.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/gadgetParam.h>
#include <gadget/Event/EventPtr.h>
#include <gadget/Type/KeyboardMouseInterface.h>

#include <osgUtil/LineSegmentIntersector>

// --- Boost includes --- //
#include <boost/signals2/signal.hpp>
#include <ves/xplorer/eventmanager/InteractionEvent.h>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

namespace ves
{
namespace xplorer
{

namespace scenegraph
{
class PhysicsSimulator;
class SceneManager;
class CharacterController;

namespace camera
{
class CameraManager;
}

namespace manipulator
{
class ManipulatorManager;
}
}
    
namespace behavior
{

/*!\file Selection.h
 *
 */
/*!\class ves::xplorer::behavior::Selection
 *
 */
/*!\namespace ves::xplorer::behavior
 *
 */
class VE_XPLORER_EXPORTS Selection
{
public:
    ///Constructor
    Selection();

    ///Destructor
    ~Selection();

private:
    ///Execute the frame all functionality
    void ProcessSelection( gadget::Keys buttonKey, int xPos, int yPos, int buttonState );
    ///Clear the point constraints
    void ClearPointConstraint();
    ///Setup the start and end point for a mouse pick 
    void SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );
    ///Update the selection line
    void UpdateSelectionLine();
    ///Now try and pick the object
    void ProcessSelection();
    ///Tell if the mouse is inside or outside the UI
    void UIEnterLeave( bool insideUI );
    /// Required connections list for connecting to events via EventManager
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;
    
    ///Object picked
    typedef boost::signals2::signal< void (osg::NodePath&) > ObjectPickedSignal_type;
    ObjectPickedSignal_type m_objectPickedSignal;
    
    ///Physics simulator pointer
    scenegraph::PhysicsSimulator& m_physicsSimulator;
    
    ///Scene manager pointer
    scenegraph::SceneManager& m_sceneManager;
    
    ///Character controller pointer
    scenegraph::CharacterController& m_characterController;
    
    ///Manipulator pointer
    scenegraph::manipulator::ManipulatorManager& m_manipulatorManager;
    
    ///Camera manager
    scenegraph::camera::CameraManager& m_cameraManager;    

    ///The ray intersector test
    osg::ref_ptr< osgUtil::LineSegmentIntersector > m_lineSegmentIntersector;
    
    ///The current X mouse position
    int m_currX;
    
    ///The current Y mouse position
    int m_currY;
    
    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* m_pickedBody;
    
    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* m_pickConstraint;
    
    ///GL transform info about the scene
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;
    
    ///Determine if the mouse is inside the UI
    bool m_mouseInsideUI;
};

}
}
}
