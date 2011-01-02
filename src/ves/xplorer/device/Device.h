/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef VES_XPLORER_DEVICE_DEVICE_H
#define VES_XPLORER_DEVICE_DEVICE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/GlobalBase.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/eventmanager/ScopedConnectionList.h>

#include <ves/open/xml/CommandPtr.h>

// --- VR Juggler Includes --- //
#include <gmtl/Point.h>
#include <gmtl/Matrix.h>

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

namespace camera
{
class CameraManager;
}

namespace manipulator
{
class ManipulatorManager;
}
}

namespace device
{
class Gloves;
class KeyboardMouse;
class Tablet;
class Wand;
class Pointer;

/*!\file Device.h
 * Device API
 */

/*!\class VE_XPlorer::Device
 *
 */
class VE_XPLORER_EXPORTS Device : public GlobalBase
{
public:
    ///
    enum Type
    {
        NONE = 0,
        GLOVES = 1,
        KEYBOARD_MOUSE = 2,
        TABLET = 3,
        WAND = 4,
        POINTER = 5
    };

    ///Constructor
    Device( const Device::Type& type = NONE );

    ///Copy Constructor
    Device( const Device& device );

    ///Destructor
    virtual ~Device();

    ///
    ///\return
    virtual Gloves* AsGloves();

    ///
    ///\return
    virtual KeyboardMouse* AsKeyboardMouse();

    ///
    ///\return
    virtual Tablet* AsTablet();

    ///
    ///\return
    virtual Wand* AsWand();

    ///
    ///\return
    virtual Pointer* AsPointer();
    
    ///
    ///\return
    const Device::Type& GetType() const;

    ///Processes device events
    ///This should be pure virtual, but isn't because of GlobalBase
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

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

	///Does not let the user go below the ground plane at 0,0,0
    ///\param input Flag to insure translation does not go below zero plane
    void SetSubZeroFlag( int input );

	///Does not let user go to anything but the ground plane at 0,0,0
	///\param input Flag to insure translation doesn't leave zero plane
	void SetZEqualsZeroFlag( int input);

    ///Used as the slot for nav control signals
    void UpdateZEqualZero( const bool enable );
    
    ///Used as the slot for nav control signals
    void UpdateZGreaterZero( const bool enable );
    
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

    ///Ensure that the camera stays above ground.
    void EnsureCameraStaysAboveGround( const gmtl::Matrix44d& headMatrix, double* translation, const osg::Quat& world_quat, int m_subzeroFlag, int m_zEqualsZeroFlag = 0 );
 
	///<Camera above the zero plane flag
	int m_subzeroFlag; 
	
	///<Camera at the zero plane flag
	int m_zEqualsZeroFlag; 

    ///Device is enabled
    bool m_enabled;

    ///enum
    const Device::Type m_type;

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

    ///
    scenegraph::camera::CameraManager& m_cameraManager;

    /// Required to be able to connect up to signals.
    ves::xplorer::eventmanager::ScopedConnectionList m_connections;

};
} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_DEVICE_H
