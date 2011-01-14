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

#ifndef VES_XPLORER_DEVICE_KEYBOARDMOUSE_H
#define VES_XPLORER_DEVICE_KEYBOARDMOUSE_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/device/Device.h>

#include <ves/xplorer/scenegraph/GLTransformInfoPtr.h>

// --- vrJuggler Includes --- //
#include <vrj/Display/DisplayPtr.h>

#include <gmtl/Vec.h>
#include <gmtl/Quat.h>

#include <boost/shared_ptr.hpp>

#include <gadget/gadgetParam.h>

#include <gadget/Type/KeyboardMouseInterface.h>
#include <gadget/Type/PositionInterface.h>

//#include <gadget/Type/KeyboardMouse/EventPtr.h>

#include <gadget/Event/KeyboardMouseEventInterface.h>
#include <gadget/Event/MouseMultiClickEventInterface.h>
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
#include <ves/xplorer/eventmanager/InteractionEvent.h>

// --- STL Includes --- //
#include <bitset>

namespace ves
{
namespace xplorer
{
namespace behavior
{
class Selection;
}

namespace device
{

/*!\file KeyboardMouse.h
 *
 */
/*!\class ves::xplorer::device::KeyboardMouse
 *
 */
/*!\namespace ves::xplorer::device
 *
 */
class VE_XPLORER_EXPORTS KeyboardMouse : public Device
{
public:
    ///Constructor
    KeyboardMouse();

    ///Destructor
    ~KeyboardMouse();

    ///
    ///\return
    virtual KeyboardMouse* AsKeyboardMouse();

    ///Processes keyboard events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Sets the screen corner values
    ///\param values A map of strings to doubles
    void SetScreenCornerValues( std::map< std::string, double > values );

    ///Set the window properties
    ///\param w Set the width of the window
    ///\param h Set the height of the window
    void SetWindowValues( unsigned int w, unsigned int h );

    ///Set the frustrum values
    ///\param l
    ///\param r
    ///\param t
    ///\param b
    ///\param n
    ///\param f
    void SetFrustumValues(
        double l, double r, double b, double t, double n, double f );

    ///Fit the world bounding volume into the viewing frustum
    void SkyCam();

    ///Resets the scene to original position
    void ResetTransforms();

    ///Update the start and end points for the line
    void UpdateSelectionLine();

    ///Get raw vrjuggler keyboardmouse ptr
    ///\return
    gadget::KeyboardMousePtr GetKeyboardMouseVRJDevice();

    ///Get the line segment intersector
    ///\pre UpdateSelectionLine must be called first
    ///\return Returns the osg class that manages the line interesection tests
    osgUtil::LineSegmentIntersector* GetLineSegmentIntersector();

    ///Set wether the keyboardmouse device should select things
    ///\param processSelection
    void SetProcessSelection( bool processSelection );

    ///
    ///\return
    bool GetMousePickEvent();

    ///Get the current display based on the VR Juggler InputArea
    ///\return The display where this event occurred
    vrj::DisplayPtr const GetCurrentDisplay( const gadget::InputArea* inputArea );

    ///Set the current matrix transform information for a given display
    ///\return Wether this operation was succesfull
    bool SetCurrentGLTransformInfo( const vrj::DisplayPtr display, bool isKeyEvent );

private:
    ves::xplorer::behavior::Selection* m_selectionSlot;
    
    ///The current X mouse position
    int m_currX;
    
    ///The current Y mouse position
    int m_currY;

    ///
    scenegraph::GLTransformInfoPtr m_currentGLTransformInfo;    

    ///Determine if the mouse is inside the UI
    bool m_mouseInsideUI;

//    typedef boost::signals2::signal<bool (ves::xplorer::eventmanager::InteractionEvent&)> InteractionSignal_type;
//    InteractionSignal_type mInteractionSignal;

    // Temporarily needed until legacy functions in this class can be
    // completely removed
    gadget::KeyboardMousePtr mKeyboardMousePtr;
    
    /// The keyboardmouse device needed for juggler >= 3.1
    gadget::KeyboardMouseEventInterface<gadget::event::all_events_tag,
        gadget::event::immediate_tag> m_keyboardMouseEventInterface;
    
    /// All keyboardmouse events get delivered here
    void onKeyboardMouseEvent(gadget::EventPtr event);

    /// Interface to receive double-click events from gadgeteer
    gadget::MouseMultiClickEventInterface< 2,
        gadget::event::all_events_tag,
        gadget::event::immediate_tag > m_mouseDoubleClickEventInterface;

    void onMouseDoubleClick( gadget::EventPtr event );

    /// MouseMove signal
    /// Params are: x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( int, int, int, int ) > MouseMoveSignal_type;
    MouseMoveSignal_type m_mouseMove;

    /// MouseDoubleClick signal
    /// Params are: button, x, y, z, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int, int ) > MouseDoubleClickSignal_type;
    MouseDoubleClickSignal_type m_mouseDoubleClick;

    /// Sets up the mouse/wand button signal map
    void SetupButtonSignalMap();

    /// Registers ButtonPress and ButtonRelease signals with EventManager
    void RegisterButtonSignals();

    ///Slot to determine if the mouse is inside the UI
    void UIEnterLeave( bool insideUI );

    /// ButtonPress signal type
    /// Params are: button, x, y, state (modifier mask OR'd with button mask)
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int ) > ButtonPressSignal_type;

    /// ButtonRelease signal type
    typedef boost::signals2::signal< void ( gadget::Keys, int, int, int ) > ButtonReleaseSignal_type;

    /// Map to hold ButtonPress signals
    typedef std::map< gadget::Keys, ButtonPressSignal_type* > ButtonPressSignalMapType;
    ButtonPressSignalMapType mButtonPressSignalMap;

    /// Map to hold ButtonRelease signals
    typedef std::map< gadget::Keys, ButtonReleaseSignal_type* > ButtonReleaseSignalMapType;
    ButtonReleaseSignalMapType mButtonReleaseSignalMap;

    ButtonPressSignal_type mButtonPress_1;
    ButtonReleaseSignal_type mButtonRelease_1;

    ButtonPressSignal_type mButtonPress_2;
    ButtonReleaseSignal_type mButtonRelease_2;

    ButtonPressSignal_type mButtonPress_3;
    ButtonReleaseSignal_type mButtonRelease_3;

    ButtonPressSignal_type mButtonPress_4;
    ButtonReleaseSignal_type mButtonRelease_4;

    ButtonPressSignal_type mButtonPress_5;
    ButtonReleaseSignal_type mButtonRelease_5;

    ButtonPressSignal_type mButtonPress_6;
    ButtonReleaseSignal_type mButtonRelease_6;

    ButtonPressSignal_type mButtonPress_7;
    ButtonReleaseSignal_type mButtonRelease_7;

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
    typedef boost::signals2::signal< void ( gadget::Keys, int, char ) > KeyPressSignal_type;

    /// KeyRelease signal type
    /// First arg is the key that was pressed
    /// Second arg is the gadget::ModiferMask (modifier mask)
    /// Third arg is the unicode representation of the key
    ///NOTE: As soon as VR Juggler supports wide body chars we can change the 
    ///char argument back to a wchar_t
    typedef boost::signals2::signal< void ( gadget::Keys, int, char ) > KeyReleaseSignal_type;

    /// Map to hold individual KeyPress signals
    typedef std::map< gadget::Keys, KeyPressSignal_type* > KeyPressSignalMapType;
    KeyPressSignalMapType mKeyPressSignalMap;

    /// Map to hold individual KeyRelease signals
    typedef std::map< gadget::Keys, KeyReleaseSignal_type* > KeyReleaseSignalMapType;
    KeyReleaseSignalMapType mKeyReleaseSignalMap;

    /// Press and Release signals for individual keys
    KeyPressSignal_type mKeyPress_KEY_NONE;
    KeyReleaseSignal_type mKeyRelease_KEY_NONE;

    KeyPressSignal_type mKeyPress_KEY_UP;
    KeyReleaseSignal_type mKeyRelease_KEY_UP;

    KeyPressSignal_type mKeyPress_KEY_DOWN;
    KeyReleaseSignal_type mKeyRelease_KEY_DOWN;

    KeyPressSignal_type mKeyPress_KEY_LEFT;
    KeyReleaseSignal_type mKeyRelease_KEY_LEFT;

    KeyPressSignal_type mKeyPress_KEY_RIGHT;
    KeyReleaseSignal_type mKeyRelease_KEY_RIGHT;

    KeyPressSignal_type mKeyPress_KEY_SHIFT;
    KeyReleaseSignal_type mKeyRelease_KEY_SHIFT;

    KeyPressSignal_type mKeyPress_KEY_CTRL;
    KeyReleaseSignal_type mKeyRelease_KEY_CTRL;

    KeyPressSignal_type mKeyPress_KEY_ALT;
    KeyReleaseSignal_type mKeyRelease_KEY_ALT;

    KeyPressSignal_type mKeyPress_KEY_COMMAND;
    KeyReleaseSignal_type mKeyRelease_KEY_COMMAND;

    KeyPressSignal_type mKeyPress_KEY_1;
    KeyReleaseSignal_type mKeyRelease_KEY_1;

    KeyPressSignal_type mKeyPress_KEY_2;
    KeyReleaseSignal_type mKeyRelease_KEY_2;

    KeyPressSignal_type mKeyPress_KEY_3;
    KeyReleaseSignal_type mKeyRelease_KEY_3;

    KeyPressSignal_type mKeyPress_KEY_4;
    KeyReleaseSignal_type mKeyRelease_KEY_4;

    KeyPressSignal_type mKeyPress_KEY_5;
    KeyReleaseSignal_type mKeyRelease_KEY_5;

    KeyPressSignal_type mKeyPress_KEY_6;
    KeyReleaseSignal_type mKeyRelease_KEY_6;

    KeyPressSignal_type mKeyPress_KEY_7;
    KeyReleaseSignal_type mKeyRelease_KEY_7;

    KeyPressSignal_type mKeyPress_KEY_8;
    KeyReleaseSignal_type mKeyRelease_KEY_8;

    KeyPressSignal_type mKeyPress_KEY_9;
    KeyReleaseSignal_type mKeyRelease_KEY_9;

    KeyPressSignal_type mKeyPress_KEY_0;
    KeyReleaseSignal_type mKeyRelease_KEY_0;

    KeyPressSignal_type mKeyPress_KEY_A;
    KeyReleaseSignal_type mKeyRelease_KEY_A;

    KeyPressSignal_type mKeyPress_KEY_B;
    KeyReleaseSignal_type mKeyRelease_KEY_B;

    KeyPressSignal_type mKeyPress_KEY_C;
    KeyReleaseSignal_type mKeyRelease_KEY_C;

    KeyPressSignal_type mKeyPress_KEY_D;
    KeyReleaseSignal_type mKeyRelease_KEY_D;

    KeyPressSignal_type mKeyPress_KEY_E;
    KeyReleaseSignal_type mKeyRelease_KEY_E;

    KeyPressSignal_type mKeyPress_KEY_F;
    KeyReleaseSignal_type mKeyRelease_KEY_F;

    KeyPressSignal_type mKeyPress_KEY_G;
    KeyReleaseSignal_type mKeyRelease_KEY_G;

    KeyPressSignal_type mKeyPress_KEY_H;
    KeyReleaseSignal_type mKeyRelease_KEY_H;

    KeyPressSignal_type mKeyPress_KEY_I;
    KeyReleaseSignal_type mKeyRelease_KEY_I;

    KeyPressSignal_type mKeyPress_KEY_J;
    KeyReleaseSignal_type mKeyRelease_KEY_J;

    KeyPressSignal_type mKeyPress_KEY_K;
    KeyReleaseSignal_type mKeyRelease_KEY_K;

    KeyPressSignal_type mKeyPress_KEY_L;
    KeyReleaseSignal_type mKeyRelease_KEY_L;

    KeyPressSignal_type mKeyPress_KEY_M;
    KeyReleaseSignal_type mKeyRelease_KEY_M;

    KeyPressSignal_type mKeyPress_KEY_N;
    KeyReleaseSignal_type mKeyRelease_KEY_N;

    KeyPressSignal_type mKeyPress_KEY_O;
    KeyReleaseSignal_type mKeyRelease_KEY_O;

    KeyPressSignal_type mKeyPress_KEY_P;
    KeyReleaseSignal_type mKeyRelease_KEY_P;

    KeyPressSignal_type mKeyPress_KEY_Q;
    KeyReleaseSignal_type mKeyRelease_KEY_Q;

    KeyPressSignal_type mKeyPress_KEY_R;
    KeyReleaseSignal_type mKeyRelease_KEY_R;

    KeyPressSignal_type mKeyPress_KEY_S;
    KeyReleaseSignal_type mKeyRelease_KEY_S;

    KeyPressSignal_type mKeyPress_KEY_T;
    KeyReleaseSignal_type mKeyRelease_KEY_T;

    KeyPressSignal_type mKeyPress_KEY_U;
    KeyReleaseSignal_type mKeyRelease_KEY_U;

    KeyPressSignal_type mKeyPress_KEY_V;
    KeyReleaseSignal_type mKeyRelease_KEY_V;

    KeyPressSignal_type mKeyPress_KEY_W;
    KeyReleaseSignal_type mKeyRelease_KEY_W;

    KeyPressSignal_type mKeyPress_KEY_X;
    KeyReleaseSignal_type mKeyRelease_KEY_X;

    KeyPressSignal_type mKeyPress_KEY_Y;
    KeyReleaseSignal_type mKeyRelease_KEY_Y;

    KeyPressSignal_type mKeyPress_KEY_Z;
    KeyReleaseSignal_type mKeyRelease_KEY_Z;

    KeyPressSignal_type mKeyPress_KEY_ESC;
    KeyReleaseSignal_type mKeyRelease_KEY_ESC;

    KeyPressSignal_type mKeyPress_KEY_TAB;
    KeyReleaseSignal_type mKeyRelease_KEY_TAB;

    KeyPressSignal_type mKeyPress_KEY_BACKTAB;
    KeyReleaseSignal_type mKeyRelease_KEY_BACKTAB;

    KeyPressSignal_type mKeyPress_KEY_BACKSPACE;
    KeyReleaseSignal_type mKeyRelease_KEY_BACKSPACE;

    KeyPressSignal_type mKeyPress_KEY_RETURN;
    KeyReleaseSignal_type mKeyRelease_KEY_RETURN;

    KeyPressSignal_type mKeyPress_KEY_ENTER;
    KeyReleaseSignal_type mKeyRelease_KEY_ENTER;

    KeyPressSignal_type mKeyPress_KEY_INSERT;
    KeyReleaseSignal_type mKeyRelease_KEY_INSERT;

    KeyPressSignal_type mKeyPress_KEY_DELETE;
    KeyReleaseSignal_type mKeyRelease_KEY_DELETE;

    KeyPressSignal_type mKeyPress_KEY_PAUSE;
    KeyReleaseSignal_type mKeyRelease_KEY_PAUSE;

    KeyPressSignal_type mKeyPress_KEY_PRINT;
    KeyReleaseSignal_type mKeyRelease_KEY_PRINT;

    KeyPressSignal_type mKeyPress_KEY_SYSREQ;
    KeyReleaseSignal_type mKeyRelease_KEY_SYSREQ;

    KeyPressSignal_type mKeyPress_KEY_HOME;
    KeyReleaseSignal_type mKeyRelease_KEY_HOME;

    KeyPressSignal_type mKeyPress_KEY_END;
    KeyReleaseSignal_type mKeyRelease_KEY_END;

    KeyPressSignal_type mKeyPress_KEY_PRIOR;
    KeyReleaseSignal_type mKeyRelease_KEY_PRIOR;

    KeyPressSignal_type mKeyPress_KEY_NEXT;
    KeyReleaseSignal_type mKeyRelease_KEY_NEXT;

    KeyPressSignal_type mKeyPress_KEY_CAPS_LOCK;
    KeyReleaseSignal_type mKeyRelease_KEY_CAPS_LOCK;

    KeyPressSignal_type mKeyPress_KEY_NUM_LOCK;
    KeyReleaseSignal_type mKeyRelease_KEY_NUM_LOCK;

    KeyPressSignal_type mKeyPress_KEY_SCROLL_LOCK;
    KeyReleaseSignal_type mKeyRelease_KEY_SCROLL_LOCK;

    KeyPressSignal_type mKeyPress_KEY_F1;
    KeyReleaseSignal_type mKeyRelease_KEY_F1;

    KeyPressSignal_type mKeyPress_KEY_F2;
    KeyReleaseSignal_type mKeyRelease_KEY_F2;

    KeyPressSignal_type mKeyPress_KEY_F3;
    KeyReleaseSignal_type mKeyRelease_KEY_F3;

    KeyPressSignal_type mKeyPress_KEY_F4;
    KeyReleaseSignal_type mKeyRelease_KEY_F4;

    KeyPressSignal_type mKeyPress_KEY_F5;
    KeyReleaseSignal_type mKeyRelease_KEY_F5;

    KeyPressSignal_type mKeyPress_KEY_F6;
    KeyReleaseSignal_type mKeyRelease_KEY_F6;

    KeyPressSignal_type mKeyPress_KEY_F7;
    KeyReleaseSignal_type mKeyRelease_KEY_F7;

    KeyPressSignal_type mKeyPress_KEY_F8;
    KeyReleaseSignal_type mKeyRelease_KEY_F8;

    KeyPressSignal_type mKeyPress_KEY_F9;
    KeyReleaseSignal_type mKeyRelease_KEY_F9;

    KeyPressSignal_type mKeyPress_KEY_F10;
    KeyReleaseSignal_type mKeyRelease_KEY_F10;

    KeyPressSignal_type mKeyPress_KEY_F11;
    KeyReleaseSignal_type mKeyRelease_KEY_F11;

    KeyPressSignal_type mKeyPress_KEY_F12;
    KeyReleaseSignal_type mKeyRelease_KEY_F12;

    KeyPressSignal_type mKeyPress_KEY_F13;
    KeyReleaseSignal_type mKeyRelease_KEY_F13;

    KeyPressSignal_type mKeyPress_KEY_F14;
    KeyReleaseSignal_type mKeyRelease_KEY_F14;

    KeyPressSignal_type mKeyPress_KEY_F15;
    KeyReleaseSignal_type mKeyRelease_KEY_F15;

    KeyPressSignal_type mKeyPress_KEY_F16;
    KeyReleaseSignal_type mKeyRelease_KEY_F16;

    KeyPressSignal_type mKeyPress_KEY_F17;
    KeyReleaseSignal_type mKeyRelease_KEY_F17;

    KeyPressSignal_type mKeyPress_KEY_F18;
    KeyReleaseSignal_type mKeyRelease_KEY_F18;

    KeyPressSignal_type mKeyPress_KEY_F19;
    KeyReleaseSignal_type mKeyRelease_KEY_F19;

    KeyPressSignal_type mKeyPress_KEY_F20;
    KeyReleaseSignal_type mKeyRelease_KEY_F20;

    KeyPressSignal_type mKeyPress_KEY_F21;
    KeyReleaseSignal_type mKeyRelease_KEY_F21;

    KeyPressSignal_type mKeyPress_KEY_F22;
    KeyReleaseSignal_type mKeyRelease_KEY_F22;

    KeyPressSignal_type mKeyPress_KEY_F23;
    KeyReleaseSignal_type mKeyRelease_KEY_F23;

    KeyPressSignal_type mKeyPress_KEY_F24;
    KeyReleaseSignal_type mKeyRelease_KEY_F24;

    KeyPressSignal_type mKeyPress_KEY_F25;
    KeyReleaseSignal_type mKeyRelease_KEY_F25;

    KeyPressSignal_type mKeyPress_KEY_F26;
    KeyReleaseSignal_type mKeyRelease_KEY_F26;

    KeyPressSignal_type mKeyPress_KEY_F27;
    KeyReleaseSignal_type mKeyRelease_KEY_F27;

    KeyPressSignal_type mKeyPress_KEY_F28;
    KeyReleaseSignal_type mKeyRelease_KEY_F28;

    KeyPressSignal_type mKeyPress_KEY_F29;
    KeyReleaseSignal_type mKeyRelease_KEY_F29;

    KeyPressSignal_type mKeyPress_KEY_F30;
    KeyReleaseSignal_type mKeyRelease_KEY_F30;

    KeyPressSignal_type mKeyPress_KEY_F31;
    KeyReleaseSignal_type mKeyRelease_KEY_F31;

    KeyPressSignal_type mKeyPress_KEY_F32;
    KeyReleaseSignal_type mKeyRelease_KEY_F32;

    KeyPressSignal_type mKeyPress_KEY_F33;
    KeyReleaseSignal_type mKeyRelease_KEY_F33;

    KeyPressSignal_type mKeyPress_KEY_F34;
    KeyReleaseSignal_type mKeyRelease_KEY_F34;

    KeyPressSignal_type mKeyPress_KEY_F35;
    KeyReleaseSignal_type mKeyRelease_KEY_F35;

    KeyPressSignal_type mKeyPress_KEY_SUPER_L;
    KeyReleaseSignal_type mKeyRelease_KEY_SUPER_L;

    KeyPressSignal_type mKeyPress_KEY_SUPER_R;
    KeyReleaseSignal_type mKeyRelease_KEY_SUPER_R;

    KeyPressSignal_type mKeyPress_KEY_MENU;
    KeyReleaseSignal_type mKeyRelease_KEY_MENU;

    KeyPressSignal_type mKeyPress_KEY_HYPER_L;
    KeyReleaseSignal_type mKeyRelease_KEY_HYPER_L;

    KeyPressSignal_type mKeyPress_KEY_HYPER_R;
    KeyReleaseSignal_type mKeyRelease_KEY_HYPER_R;

    KeyPressSignal_type mKeyPress_KEY_HELP;
    KeyReleaseSignal_type mKeyRelease_KEY_HELP;

    KeyPressSignal_type mKeyPress_KEY_SPACE;
    KeyReleaseSignal_type mKeyRelease_KEY_SPACE;

    KeyPressSignal_type mKeyPress_KEY_ANY;
    KeyReleaseSignal_type mKeyRelease_KEY_ANY;

    KeyPressSignal_type mKeyPress_KEY_EXCLAM;
    KeyReleaseSignal_type mKeyRelease_KEY_EXCLAM;

    KeyPressSignal_type mKeyPress_KEY_QUOTE_DBL;
    KeyReleaseSignal_type mKeyRelease_KEY_QUOTE_DBL;

    KeyPressSignal_type mKeyPress_KEY_NUMBER_SIGN;
    KeyReleaseSignal_type mKeyRelease_KEY_NUMBER_SIGN;

    KeyPressSignal_type mKeyPress_KEY_DOLLAR;
    KeyReleaseSignal_type mKeyRelease_KEY_DOLLAR;

    KeyPressSignal_type mKeyPress_KEY_PERCENT;
    KeyReleaseSignal_type mKeyRelease_KEY_PERCENT;

    KeyPressSignal_type mKeyPress_KEY_AMPERSAND;
    KeyReleaseSignal_type mKeyRelease_KEY_AMPERSAND;

    KeyPressSignal_type mKeyPress_KEY_APOSTROPHE;
    KeyReleaseSignal_type mKeyRelease_KEY_APOSTROPHE;

    KeyPressSignal_type mKeyPress_KEY_PAREN_LEFT;
    KeyReleaseSignal_type mKeyRelease_KEY_PAREN_LEFT;

    KeyPressSignal_type mKeyPress_KEY_PAREN_RIGHT;
    KeyReleaseSignal_type mKeyRelease_KEY_PAREN_RIGHT;

    KeyPressSignal_type mKeyPress_KEY_ASTERISK;
    KeyReleaseSignal_type mKeyRelease_KEY_ASTERISK;

    KeyPressSignal_type mKeyPress_KEY_PLUS;
    KeyReleaseSignal_type mKeyRelease_KEY_PLUS;

    KeyPressSignal_type mKeyPress_KEY_COMMA;
    KeyReleaseSignal_type mKeyRelease_KEY_COMMA;

    KeyPressSignal_type mKeyPress_KEY_MINUS;
    KeyReleaseSignal_type mKeyRelease_KEY_MINUS;

    KeyPressSignal_type mKeyPress_KEY_PERIOD;
    KeyReleaseSignal_type mKeyRelease_KEY_PERIOD;

    KeyPressSignal_type mKeyPress_KEY_SLASH;
    KeyReleaseSignal_type mKeyRelease_KEY_SLASH;

    KeyPressSignal_type mKeyPress_KEY_COLON;
    KeyReleaseSignal_type mKeyRelease_KEY_COLON;

    KeyPressSignal_type mKeyPress_KEY_SEMICOLON;
    KeyReleaseSignal_type mKeyRelease_KEY_SEMICOLON;

    KeyPressSignal_type mKeyPress_KEY_LESS;
    KeyReleaseSignal_type mKeyRelease_KEY_LESS;

    KeyPressSignal_type mKeyPress_KEY_EQUAL;
    KeyReleaseSignal_type mKeyRelease_KEY_EQUAL;

    KeyPressSignal_type mKeyPress_KEY_GREATER;
    KeyReleaseSignal_type mKeyRelease_KEY_GREATER;

    KeyPressSignal_type mKeyPress_KEY_QUESTION;
    KeyReleaseSignal_type mKeyRelease_KEY_QUESTION;

    KeyPressSignal_type mKeyPress_KEY_AT;
    KeyReleaseSignal_type mKeyRelease_KEY_AT;

    KeyPressSignal_type mKeyPress_KEY_BRACKET_LEFT;
    KeyReleaseSignal_type mKeyRelease_KEY_BRACKET_LEFT;

    KeyPressSignal_type mKeyPress_KEY_BACKSLASH;
    KeyReleaseSignal_type mKeyRelease_KEY_BACKSLASH;

    KeyPressSignal_type mKeyPress_KEY_BRACKET_RIGHT;
    KeyReleaseSignal_type mKeyRelease_KEY_BRACKET_RIGHT;

    KeyPressSignal_type mKeyPress_KEY_ASCII_CIRCUM;
    KeyReleaseSignal_type mKeyRelease_KEY_ASCII_CIRCUM;

    KeyPressSignal_type mKeyPress_KEY_UNDERSCORE;
    KeyReleaseSignal_type mKeyRelease_KEY_UNDERSCORE;

    KeyPressSignal_type mKeyPress_KEY_QUOTE_LEFT;
    KeyReleaseSignal_type mKeyRelease_KEY_QUOTE_LEFT;

    KeyPressSignal_type mKeyPress_KEY_BRACE_LEFT;
    KeyReleaseSignal_type mKeyRelease_KEY_BRACE_LEFT;

    KeyPressSignal_type mKeyPress_KEY_BAR;
    KeyReleaseSignal_type mKeyRelease_KEY_BAR;

    KeyPressSignal_type mKeyPress_KEY_BRACE_RIGHT;
    KeyReleaseSignal_type mKeyRelease_KEY_BRACE_RIGHT;

    KeyPressSignal_type mKeyPress_KEY_ASCII_TILDE;
    KeyReleaseSignal_type mKeyRelease_KEY_ASCII_TILDE;

};
} //end device
} //end xplorer
} //end ves

#endif //VES_XPLORER_DEVICE_KEYBOARDMOUSE_H
