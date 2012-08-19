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
#pragma once

#include <ves/VEConfig.h>
#include <switchwire/ScopedConnectionList.h>
#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/gadgetParam.h>
#include <gadget/Event/EventPtr.h>
#include <gadget/Type/KeyboardMouseInterface.h>

#include <osgUtil/LineSegmentIntersector>

// --- Boost includes --- //
#include <switchwire/Event.h>
#include <switchwire/InteractionEvent.h>

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

/*!\file CharacterNavigation.h
 *
 */
/*!\class ves::xplorer::behavior::CharacterNavigation
 *
 */
/*!\namespace ves::xplorer::behavior
 *
 */
class VE_XPLORER_EXPORTS CharacterNavigation
{
public:
    ///Constructor
    CharacterNavigation();

    ///Destructor
    ~CharacterNavigation();

private:
    ///Register the key press events
    bool ProcessKeyPressEvent( gadget::Keys, int, char );
    ///Register the key release events
    bool ProcessKeyReleaseEvent( gadget::Keys, int, char );
    ///Process the Button release
    bool ProcessMousePress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState );
    ///Register button press
    bool ProcessMouseRelease( gadget::Keys buttonKey, int xPos, int yPos, int buttonState );
    ///Process moving the constraint
    bool ProcessMouseMove( int xPos, int yPos, int zPos, int buttonState );
    ///Process moving the constraint
    bool ProcessMouseScroll( int deltaX, int deltaY, int x, int y, int buttonState );

    ///Selection start end point line
    osg::Vec3d m_startPoint;
    osg::Vec3d m_endPoint;

    /// Required connections list for connecting to events via EventManager
    switchwire::ScopedConnectionList m_connections;

    ///Object picked
    typedef switchwire::Event< void ( osg::NodePath& ) > ObjectPickedSignal_type;
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

    ///The previous X mouse position
    int m_prevX;

    ///The previous Y mouse position
    int m_prevY;

    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* m_pickedBody;

    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* m_pickConstraint;

    ///GL transform info about the scene
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;

    ///The distance from the head position to the picked btRigidBody point
    ///Used to calculate point to point constraints for physics picking
    double m_prevPhysicsRayPos;
};

}
}
}
