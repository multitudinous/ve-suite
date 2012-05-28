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

/*!\file Navigation.h
 *
 */
/*!\class ves::xplorer::behavior::Navigation
 *
 */
/*!\namespace ves::xplorer::behavior
 *
 */
class VE_XPLORER_EXPORTS Navigation
{
public:
    ///Constructor
    Navigation();

    ///Destructor
    ~Navigation();

private:
    ///Rotates an object about the y-axis
    void Twist( double dx, double dy );

    ///Handles movement in and out of the scene
    ///\param dy The change in the y direction
    void Zoom( double dy );

    ///Handles panning movements through the scene
    ///\param dx The change in the x direction
    ///\param dz The change in the yz direction
    void Pan( double dx, double dz );

    ///Handles rotation of the scene
    ///\param angle
    ///\param axis
    void Rotate( double angle, gmtl::Vec3d axis );

    ///Set the window properties
    ///\param w Set the width of the window
    ///\param h Set the height of the window
    void SetWindowValues( unsigned int w, unsigned int h );

    ///Processes the navigation events
    void ProcessNavigation();

    ///Set the center point mode
    ///\param jumpMode Do not know what this does
    void SetCenterPointJumpMode( const std::string& jumpMode );


    ///Process the Button release
    bool ProcessNavigation( int xPos, int yPos, int zPos, int buttonState );
    ///Register button press
    bool RegisterButtonPress( gadget::Keys buttonKey, int xPos, int yPos, int buttonState );
    ///Clear the point constraints
    void ClearPointConstraint();
    ///Setup the start and end point for a mouse pick
    void SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );
    ///Update the selection line
    void UpdateSelectionLine();
    ///Now try and pick the object
    void ProcessSelection();
    ///Reset nav slot
    void ResetToGlobalOrigin();

    /// Required connections list for connecting to events via EventManager
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;

    ///Object picked
    typedef boost::signals2::signal< void ( osg::NodePath& ) > ObjectPickedSignal_type;
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

    ///Width of the window
    unsigned int m_windowWidth;

    ///Height of the window
    unsigned int m_windowHeight;

    ///The current X mouse position
    int m_currX;

    ///The current Y mouse position
    int m_currY;

    //These variables are needed on linux and mac to calculate mouse dx and dy
    //#if !defined( VPR_OS_Windows )
    ///The previous X mouse position
    int m_prevX;

    ///The previous Y mouse position
    int m_prevY;
    //#endif

    ///Aspect ratio of window
    double mAspectRatio;

    ///The rigid body that has been selected during physics mouse picking
    btRigidBody* m_pickedBody;

    ///Bullet constraint used for physics mouse picking
    btTypedConstraint* m_pickConstraint;

    ///GL transform info about the scene
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;

    ///
    gmtl::Quatd mDeltaRotation;

    ///
    gmtl::Vec3d mDeltaTranslation;

    ///Triggers a center point jump after this distance has been breached
    ///Do not allocate memory 'new' for this pointer
    double mCenterPointThreshold;

    ///The distance the center point jumps along the +y axis
    ///Do not allocate memory 'new' for this pointer
    double mCenterPointJump;
};

}
}
}
