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
#ifndef VES_XPLORER_DEVICE_KEYBOARDMOUSE_H
#define VES_XPLORER_DEVICE_KEYBOARDMOUSE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>

// --- vrJuggler Includes --- //
#include <vrj/Display/DisplayPtr.h>

#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <gadget/gadgetParam.h>

#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/PositionInterface.h>

#include <gadget/Event/KeyboardMouseEventInterface.h>
#include <gadget/Event/MouseClickEventInterface.h>
#include <gadget/Event/MouseClickEventInterface.h>
#include <gadget/Event/EventPtr.h>

namespace gadget
{
class InputArea;
}

// --- OSG Includes --- //
#include <osg/Geode>

#include <osgUtil/LineSegmentIntersector>

// --- Bullet Includes --- //
class btRigidBody;
class btTypedConstraint;

// --- Boost includes --- //
#include <boost/signals2/signal.hpp>
//#include <ves/xplorer/eventmanager/InteractionEvent.h>

// --- STL Includes --- //
#include <bitset>

namespace ves
{
namespace xplorer
{
namespace device
{

/*!\file KeyboardMouse.h
 * \class ves::xplorer::device::KeyboardMouse
 * \namespace ves::xplorer::device
 *
 */
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
    ///Constructor
    KeyboardMouse();

    ///Destructor
    virtual ~KeyboardMouse();

    ///
    ///\return
    virtual KeyboardMouse* AsKeyboardMouse();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Get raw vrjuggler keyboardmouse ptr
    ///\return
    gadget::KeyboardMousePtr GetKeyboardMouseVRJDevice();

     ///Get the current display based on the VR Juggler InputArea
    ///\return The display where this event occurred
    vrj::DisplayPtr const GetCurrentDisplay( const gadget::InputArea* inputArea );

    ///Set the current matrix transform information for a given display
    ///\return Wether this operation was succesfull
    bool SetCurrentGLTransformInfo( const vrj::DisplayPtr display, bool isKeyEvent );

private:
    ///Setup the start and end point for a mouse pick 
    void SetStartEndPoint( osg::Vec3d& startPoint, osg::Vec3d& endPoint );

    ///Handle VR Juggler kernal signals 
    void HandleSignal( const int signum );
    
    ///The current X mouse position
    int m_currX;
    
    ///The current Y mouse position
    int m_currY;

    ///The current matrix stack for the context
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;    

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
    
    /// Temporarily needed until legacy functions in this class can be
    /// completely removed
    gadget::KeyboardMousePtr mKeyboardMousePtr;
    
    /// The keyboardmouse device needed for juggler
    ///For now we will use the synchronized_tag rather than the immediate tag
    ///so the the function callbacks are executed from VR Juggler in sync with
    ///the draw loop rather than when the events occur. If we use the immediate
    ///tag then our event management will execute code whenever an event
    ///is called in VR Juggler. This has the potential to cause big thread
    ///sync issues. Again, for now we will use the draw loop to help us sync
    ///data access.
    gadget::KeyboardMouseEventInterface<gadget::event::all_events_tag,
        gadget::event::synchronized_tag> m_keyboardMouseEventInterface;
    
    /// All keyboardmouse events get delivered here
    void onKeyboardMouseEvent(gadget::EventPtr event);

    /// Interface to receive double-click events from gadgeteer
    ///For now we will use the synchronized_tag rather than the immediate tag
    ///so the the function callbacks are executed from VR Juggler in sync with
    ///the draw loop rather than when the events occur. If we use the immediate
    ///tag then our event management will execute code whenever an event
    ///is called in VR Juggler. This has the potential to cause big thread
    ///sync issues. Again, for now we will use the draw loop to help us sync
    ///data access.
#if 0
    gadget::MouseMultiClickEventInterface< 2,
        gadget::event::all_events_tag,
        gadget::event::synchronized_tag > m_mouseDoubleClickEventInterface;
#else
    gadget::MouseClickEventInterface< 2,
        gadget::event::all_events_tag,
        gadget::event::synchronized_tag > m_mouseDoubleClickEventInterface;
#endif

    void onMouseDoubleClick( gadget::EventPtr event );

    /// MouseMove signal
    /// Params are: x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( int, int, int, int ),
            eventmanager::BooleanPropagationCombiner > MouseMoveSignal_type;
    MouseMoveSignal_type m_mouseMove;

    /// MouseDoubleClick signal
    /// Params are: button, x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( gadget::Keys, int, int, int, int ),
        eventmanager::BooleanPropagationCombiner > MouseDoubleClickSignal_type;
    MouseDoubleClickSignal_type m_mouseDoubleClick;

    /// Sets up the mouse/wand button signal map
    void SetupButtonSignalMap();

    /// Registers ButtonPress and ButtonRelease signals with EventManager
    void RegisterButtonSignals();

    ///Slot to determine if the mouse is inside the UI
    void Exit( bool const& exit );

    /// Scroll signal type
    /// Params are: deltaX, deltaY, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( int, int, int, int, int ),
        eventmanager::BooleanPropagationCombiner > ScrollSignal_type;
    ScrollSignal_type m_scroll;

    /// ButtonPress signal type
    /// Params are: button, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< bool ( gadget::Keys, int, int, int ),
        eventmanager::BooleanPropagationCombiner > ButtonPressSignal_type;

    /// ButtonRelease signal type
    typedef boost::signals2::signal< bool ( gadget::Keys, int, int, int ),
        eventmanager::BooleanPropagationCombiner > ButtonReleaseSignal_type;

    /// Map to hold ButtonPress signals
    /// First arg is actually a gadget::Keys
    typedef std::map< int, ButtonPressSignal_type* > ButtonPressSignalMapType;
    ButtonPressSignalMapType mButtonPressSignalMap;

    /// Map to hold ButtonRelease signals
    /// First arg is actually a gadget::Keys
    typedef std::map< int, ButtonReleaseSignal_type* > ButtonReleaseSignalMapType;
    ButtonReleaseSignalMapType mButtonReleaseSignalMap;

    typedef std::map< std::string, ButtonPressSignal_type* > mButtonPressSignalHolder_type;
    mButtonPressSignalHolder_type mButtonPressSignalHolder;

    typedef std::map< std::string, ButtonReleaseSignal_type* > mButtonReleaseSignalHolder_type;
    mButtonReleaseSignalHolder_type mButtonReleaseSignalHolder;

    /// Sets up maps for KeyPress and KeyRelease signals
    void SetupKeySignalMap();

    /// Registers KeyPress and KeyRelease signals with EventManager
    /// These signals will be given names according to the schema
    /// KeyboardMouse.Key[Press,Release]_KEY_[key]
    /// where KEY_[key] follows exactly the gadget::Keys enumerator
    void RegisterKeySignals();

    /// KeyPress signal type
    /// First arg is the key that was pressed
    /// Second arg is the gadget::ModiferMask (modifier mask)
    /// Third arg is the unicode representation of the key
    ///NOTE: As soon as VR Juggler supports wide body chars we can change the 
    ///char argument back to a wchar_t
    typedef boost::signals2::signal< bool ( gadget::Keys, int, char ),
        eventmanager::BooleanPropagationCombiner > KeyPressSignal_type;

    typedef boost::signals2::signal< bool ( gadget::Keys, int, char ) > KeyPressSignal_rtype;

    /// KeyRelease signal type
    /// First arg is the key that was pressed
    /// Second arg is the gadget::ModiferMask (modifier mask)
    /// Third arg is the unicode representation of the key
    ///NOTE: As soon as VR Juggler supports wide body chars we can change the 
    ///char argument back to a wchar_t
    typedef boost::signals2::signal< bool ( gadget::Keys, int, char ),
        eventmanager::BooleanPropagationCombiner > KeyReleaseSignal_type;

    typedef boost::signals2::signal< bool ( gadget::Keys, int, char ) > KeyReleaseSignal_rtype;

    /// Map to hold individual KeyPress signals
    typedef std::map< gadget::Keys, KeyPressSignal_type* > KeyPressSignalMapType;
    KeyPressSignalMapType mKeyPressSignalMap;

    /// Map to hold individual KeyRelease signals
    typedef std::map< gadget::Keys, KeyReleaseSignal_type* > KeyReleaseSignalMapType;
    KeyReleaseSignalMapType mKeyReleaseSignalMap;

    typedef std::map< std::string, KeyPressSignal_type* > mKeyPressSignalHolder_type;
    mKeyPressSignalHolder_type mKeyPressSignalHolder;

    typedef std::map< std::string, KeyReleaseSignal_type* > mKeyReleaseSignalHolder_type;
    mKeyReleaseSignalHolder_type mKeyReleaseSignalHolder;
};

} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_KEYBOARDMOUSE_H
