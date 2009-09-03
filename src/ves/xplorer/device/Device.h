/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef DEVICE_H
#define DEVICE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/DCS.h>

// --- vrJugglerIncludes --- //
#include <gmtl/Point.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace osg
{
class Vec3d;
}

// --- Bullet Includes --- //
class btDynamicsWorld;

namespace ves
{

namespace open
{
namespace xml
{
class Command;
}
}

namespace xplorer
{

namespace scenegraph
{
class PhysicsSimulator;
class SceneManager;
class CharacterController;

namespace manipulator
{
class ManipulatorManager;
}
}

namespace device
{
/*!\file Device.h
 * Device API
 */

/*!\class VE_XPlorer::Device
 *
 */
class VE_XPLORER_EXPORTS Device : public GlobalBase
{
public:
    ///Constructor
    Device();

    ///Destructor
    virtual ~Device();

    ///Processes device events
    ///This should be pure virtual, but isn't because of GlobalBase
    virtual void ProcessEvents(){;}

    ///New function for new VECommand structure
    ///\param veCommand Sets the Command used for navigation
    virtual void SetVECommand( ves::open::xml::CommandPtr command );

    ///This is a pure virtual function from GlobalBase
    virtual void UpdateCommand();
    
    ///Initialize the device if needed
    virtual void Initialize(){;}

    ///Check if the head collides with the rest of the objects in the scene
    ///\param headPositionInWorld The head position in world coordinates
    bool CheckCollisionsWithHead( osg::Vec3 headPositionInWorld );

    ///
    ///\param
    virtual void Enable( const bool& enable = true );

    ///
    ///\return
    const bool& IsEnabled();

    ///Set the center point
    ///\param cp The center point
    void SetCenterPoint( gmtl::Point3d* centerPoint );

    ///Set the center point delta jump
    void SetCenterPointJump( double* jump );

    ///Set the center point threshold
    void SetCenterPointThreshold( double* threshold );

    ///Set the reset position for the world
    void SetResetWorldPosition( osg::Quat* quat, std::vector< double >* pos );

protected:
    ///Process the selection of a piece of geometry
    virtual void ProcessSelection();

    ///Definition to set the start and end point
    ///\param startPoint The start point
    ///\param endPoint
    virtual void SetStartEndPoint(
        osg::Vec3d* startPoint, osg::Vec3d* endPoint );

    ///Set the start and end point
    ///\param startPoint The start point
    ///\param endPoint The end point
    virtual void DrawLine( osg::Vec3d startPoint, osg::Vec3d endPoint );

    ///
    bool m_enabled;

    ///Triggers a center point jump after this distance has been breached
    ///Do not allocate memory 'new' for this pointer
    double* mCenterPointThreshold;

    ///The distance the center point jumps along the +y axis
    ///Do not allocate memory 'new' for this pointer
    double* mCenterPointJump;

    ///The reset position for the world
    ///Do not allocate memory 'new' for this pointer
    std::vector< double >* mResetPosition;

    ///The point about which rotation occurs
    ///Do not allocate memory 'new' for this pointer
    gmtl::Point3d* mCenterPoint;

    ///The reset axis for the world
    ///Do not allocate memory 'new' for this pointer
    osg::Quat* mResetAxis;

    ///
    scenegraph::PhysicsSimulator& m_physicsSimulator;

    ///
    scenegraph::SceneManager& m_sceneManager;

    ///
    scenegraph::CharacterController& m_characterController;

    ///
    scenegraph::manipulator::ManipulatorManager& m_manipulatorManager;

};
} //end device
} //end xplorer
} //end ves

#endif //DEVICE_H
