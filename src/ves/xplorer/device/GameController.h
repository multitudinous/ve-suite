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
#ifndef VES_XPLORER_DEVICE_GAMECONTROLLER_H
#define VES_XPLORER_DEVICE_GAMECONTROLLER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

// --- vrJuggler Includes --- //

#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <boost/shared_ptr.hpp>

#include <gadget/gadgetParam.h>

#include <gadget/Type/PositionInterface.h>
#include <gadget/Event/AnalogEventInterface.h>

#include <gadget/Event/EventPtr.h>

namespace gadget
{
class InputArea;
}

namespace osgwMx
{
class MxInputAdapterGadgeteerGamePad;
class MxMovementStyleGamePad;
}

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- Boost includes --- //
#include <boost/signals2/signal.hpp>

// --- STL Includes --- //
#include <bitset>

namespace ves
{
namespace xplorer
{
namespace device
{

/*!\file GameController.h
 * \class ves::xplorer::device::GameController
 * \namespace ves::xplorer::device
 *
 */
class VE_XPLORER_EXPORTS GameController : public Device
{
public:
    ///Constructor
    GameController();

    ///Destructor
    virtual ~GameController();

    ///
    ///\return
    virtual GameController* AsGameController();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

private:
    typedef gadget::AnalogEventInterface<gadget::event::all_events_tag,
        gadget::event::synchronized_tag> AnalogAxisInterface;
    AnalogAxisInterface m_analogAxis0EventInterface;
    AnalogAxisInterface m_analogAxis1EventInterface;
    AnalogAxisInterface m_analogAxis2EventInterface;
    AnalogAxisInterface m_analogAxis3EventInterface;
    AnalogAxisInterface m_analogAxis4EventInterface;
    AnalogAxisInterface m_analogAxis5EventInterface;

    /// All GameController events get delivered here
    void OnAxis0Event( const float event );
    /// All GameController events get delivered here
    void OnAxis1Event( const float event );
    /// All GameController events get delivered here
    void OnAxis2Event( const float event );
    /// All GameController events get delivered here
    void OnAxis3Event( const float event );
    /// All GameController events get delivered here
    void OnAxis4Event( const float event );
    /// All GameController events get delivered here
    void OnAxis5Event( const float event );

    ///Setup the start and end point for a mouse pick 
    void SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );

    ///The current X mouse position
    int m_currX;
    
    ///The current Y mouse position
    int m_currY;

    ///Determine if the mouse is inside the UI
    bool m_exit;
    
    ///Selection ray start point
    osg::Vec3d m_startPoint;
    ///Selection ray end point
    osg::Vec3d m_endPoint;
    
    //The signal handler to hand off to VR Juggler
    //vrj::Kernel::signal_callback_t m_signalHandler;

    /// signal for generating the start and end point for selection and other
    ///interaction tools.
    /// Params are: start point and end point
    typedef boost::signals2::signal< void ( osg::Vec3d, osg::Vec3d ) > StartEndPointSignal_type;
    StartEndPointSignal_type m_startEndPointSignal;
    
    /// MouseMove signal
    /// Params are: x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( int, int, int, int ),
            eventmanager::BooleanPropagationCombiner > MouseMoveSignal_type;
    MouseMoveSignal_type m_mouseMove;

    ///Slot to determine if the mouse is inside the UI
    void Exit( bool const& exit );

    /// Scroll signal type
    /// Params are: deltaX, deltaY, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( int, int, int, int, int ),
        eventmanager::BooleanPropagationCombiner > ScrollSignal_type;
    ScrollSignal_type m_scroll;
    
    osg::ref_ptr< osgwMx::MxInputAdapterGadgeteerGamePad > m_gadgetInputAdapter;
    osg::ref_ptr< osgwMx::MxMovementStyleGamePad > m_mxGamePadStyle;
};

} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_GAMECONTROLLER_H
