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

#include <ves/xplorer/eventmanager/EventMapper.h>
#include <ves/xplorer/eventmanager/EventManager.h>
#include <ves/xplorer/eventmanager/BooleanPropagationCombiner.h>
#include <ves/xplorer/eventmanager/ConnectSignals.h>

#include <gadget/Type/KeyboardMouse/Keys.h>

#include <boost/ref.hpp>

#include <sstream>


namespace ves
{
namespace xplorer
{
namespace eventmanager
{

vprSingletonImp( EventMapper );

EventMapper::EventMapper()
    :
    m_logger( Poco::Logger::get( "xplorer.EventMapper" ) )
{
    m_logStream = ves::xplorer::LogStreamPtr( new Poco::LogStream( m_logger ) );
    LOG_TRACE( "ctor" );
    // Connect to signals to get all keypresses, keyreleases, buttonpresses,
    // and buttonreleases
    CONNECTSIGNALS_4_COMBINER( "%Mouse.ButtonPress%",bool ( gadget::Keys, int, int, int ),
                      BooleanPropagationCombiner,
                      &EventMapper::ButtonPressEvent, mConnections,
                      button_SignalType, normal_Priority );

    CONNECTSIGNALS_4_COMBINER( "%Mouse.ButtonRelease%",bool ( gadget::Keys, int, int, int ),
                      BooleanPropagationCombiner,
                      &EventMapper::ButtonReleaseEvent, mConnections,
                      button_SignalType, normal_Priority );

    CONNECTSIGNALS_5_COMBINER( "%Mouse.DoubleClick%", bool( gadget::Keys, int, int, int, int ),
                      BooleanPropagationCombiner,
                      &EventMapper::MouseDoubleClickEvent, mConnections,
                      button_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "%KeyPress%", bool ( gadget::Keys, int, char ),
                      BooleanPropagationCombiner,
                      &EventMapper::KeyPressEvent, mConnections,
                      keyboard_SignalType, normal_Priority );

    CONNECTSIGNALS_3_COMBINER( "%KeyRelease%", bool ( gadget::Keys, int, char ),
                      BooleanPropagationCombiner,
                      &EventMapper::KeyReleaseEvent, mConnections,
                      keyboard_SignalType, normal_Priority );


//    ConnectSignals_3<bool ( gadget::Keys, int, char ), BooleanPropagationCombiner>(
//                    "%KeyRelease%", &EventMapper::KeyReleaseEvent, this, mConnections,
//            EventManager::keyboard_SignalType, EventManager::normal_Priority );

    SetupDefaultBindings();
}

EventMapper::~EventMapper()
{
    // Delete signal objects generated in calls to AddMappableEvent
    BehaviorMapType::iterator iter = mSyncNoneBehaviorMap.begin();
    while( iter != mSyncNoneBehaviorMap.end() )
    {
        delete iter->second;
        ++iter;
    }

    BehaviorMapType::iterator g_iter = mSyncGraphicsBehaviorMap.begin();
    while( g_iter != mSyncGraphicsBehaviorMap.end() )
    {
        delete g_iter->second;
        ++g_iter;
    }
}

void EventMapper::PushMapEvent( const std::string& KeyButton, const std::string& Behavior )
{
    LOG_INFO( "PushMapEvent: Mapping " << KeyButton << " to " << Behavior );

    // If there's already a history for this KeyButton, push the existing mapping
    // down into it. If not, create an empty history. Future mappings for this
    // KeyButton will then find the history.
    mEventHistoryMapType::iterator iter = mEventHistoryMap.find( KeyButton );
    if( iter != mEventHistoryMap.end() )
    {
        iter->second->push_back( mEventBehaviorMap[ KeyButton ] );
    }
    else
    {
        std::vector< std::string >* tHistory = new std::vector< std::string >;
        mEventHistoryMap[ KeyButton ] = tHistory;
    }

    mEventBehaviorMap[ KeyButton ] = Behavior;
}

void EventMapper::PopMapEvent( const std::string& KeyButton )
{
    LOG_INFO( "PopMapEvent: " << KeyButton );
    // If there's a non-empty history for this KeyButton, jettison the current
    // mapping and restore it to the previous one, then remove the "previous one"
    // from the history
    mEventHistoryMapType::const_iterator iter = mEventHistoryMap.find( KeyButton );
    if( iter != mEventHistoryMap.end() )
    {
        std::vector< std::string >* history = iter->second;
        if( !history->empty() )
        {
            LOG_INFO( "Restoring mapping to: " << history->back() );
            mEventBehaviorMap[ KeyButton ] = history->back();
            history->pop_back();
        }
        else
        {
            LOG_INFO( "Empty history for this event" );
        }
    }
    else
    {
        LOG_INFO( "No history for this event" );
    }
}

bool EventMapper::ButtonPressEvent( gadget::Keys button, int x, int y, int state )
{
    LOG_TRACE( "ButtonPressEvent: " << button << ", " << x << ", " << y << ", "
               << state );
    std::string EventName( "ButtonPress_" );
    QueueAndEmit( EventName, button );

    // Don't prevent propagation of this signal!
    return false;
}

bool EventMapper::ButtonReleaseEvent( gadget::Keys button, int x, int y, int state )
{
    LOG_TRACE( "ButtonReleaseEvent: " << button << ", " << x << ", " << y << ", "
               << state );
    std::string EventName( "ButtonRelease_" );
    QueueAndEmit( EventName, button );

    // Don't prevent propagation of this signal!
    return false;
}

bool EventMapper::MouseDoubleClickEvent( gadget::Keys button, int x, int y, int z, int state )
{
    LOG_TRACE( "MouseDoubleClickEvent: " << button << ", " << x << ", " << y << ", "
               << z << "," << state );
    std::string EventName( "DoubleClick_" );
    QueueAndEmit( EventName, button );

    // Don't prevent propagation of this signal!
    return false;
}

bool EventMapper::KeyPressEvent( gadget::Keys key, int modifiers, char unicode )
{
    LOG_TRACE( "KeyPressEvent: " << key << ", " << modifiers << ", " << unicode );
    std::string EventName( "KeyPress_" );
    QueueAndEmit( EventName, key );

    // Don't prevent propagation of this signal!
    return false;
}

bool EventMapper::KeyReleaseEvent( gadget::Keys key, int modifiers, char unicode )
{
    LOG_TRACE( "KeyReleaseEvent: " << key << ", " << modifiers << ", " << unicode );
    std::string EventName( "KeyRelease_" );
    QueueAndEmit( EventName, key );

    // Don't prevent propagation of this signal!
    return false;
}

void EventMapper::QueueAndEmit( std::string& eventName, gadget::Keys key )
{
    eventName.append( getKeyName( key ) );
    LOG_TRACE( "QueueAndEmit: " << eventName );

    // See if this event is mapped to a signal
    EventBehaviorMapType::const_iterator ebIter = mEventBehaviorMap.find( eventName );
    if( ebIter != mEventBehaviorMap.end() )
    {
        // See if the signal is a sync_none type. If so, emit it now.
        BehaviorMapType::const_iterator bmIter = mSyncNoneBehaviorMap.find( ebIter->second );
        if( bmIter != mSyncNoneBehaviorMap.end() )
        {
            voidSignalType* sig = bmIter->second;
            (*sig)();
        }
        else
        {
            // Queue the event since it must be synced to
            // the graphics loop
            mSyncGraphicsQueue.push_back( ebIter->second );
        }
    }
}

void EventMapper::LatePreFrameUpdate()
{
    EmitSyncGraphicsSignals();
}

void EventMapper::EmitSyncGraphicsSignals()
{
    LOG_TRACE( "EmitSyncGraphicsSignals" );
    if( mSyncGraphicsQueue.empty() )
    {
        return;
    }

    std::vector< std::string >::iterator queue = mSyncGraphicsQueue.begin();
    while( queue != mSyncGraphicsQueue.end() )
    {
        BehaviorMapType::const_iterator bmIter = mSyncGraphicsBehaviorMap.find( *queue );
        if( bmIter != mSyncGraphicsBehaviorMap.end() )
        {
            voidSignalType* sig = bmIter->second;
            (*sig)();
        }
        ++queue;
    }

    mSyncGraphicsQueue.clear();
}

void EventMapper::AddMappableBehavior( const std::string& BehaviorName, syncType sync )
{
    LOG_INFO( "AddMappableBehavior: " << BehaviorName << ", sync type " << sync );
    // Check behavior maps to see if this behavior already exists
    BehaviorMapType::const_iterator iter =
            mSyncNoneBehaviorMap.find( BehaviorName );
    if( iter != mSyncNoneBehaviorMap.end() )
    {
        // Behavior already exists in our map. No need to add it again.
        LOG_WARNING( "Behavior already exists; not adding." );
        return;
    }

    iter = mSyncGraphicsBehaviorMap.find( BehaviorName );
    if( iter != mSyncGraphicsBehaviorMap.end() )
    {
        // Behavior already exists in our map. No need to add it again.
        LOG_WARNING( "Behavior already exists; not adding." );
        return;
    }

    // If we've made it this far, the behavior does not exist in our maps
    // yet and we need to register a signal for it then add it to the proper
    // map based on sync type.

    // Register signal for behavior
    voidSignalType* signal = new ( voidSignalType );
    std::string name("EventMapper.");
    name.append( BehaviorName );
    ves::xplorer::eventmanager::EventManager::instance()->RegisterSignal(
            new ves::xplorer::eventmanager::SignalWrapper< voidSignalType >( signal ),
            name );

    // Add signal to appropriate map based on sync type
    if( sync == syncNone )
    {
        mSyncNoneBehaviorMap[ BehaviorName ] = signal;
    }
    else if( sync == syncGraphics )
    {
        mSyncGraphicsBehaviorMap[ BehaviorName ] = signal;
    }
}

void EventMapper::SetupDefaultBindings()
{
    LOG_TRACE( "SetupDefaultBindings" );

    // FrameAll -- Sync to Graphics
    AddMappableBehavior( "FrameAll", syncGraphics );
    PushMapEvent( "KeyRelease_KEY_F", "FrameAll" );


    // HideShowUI -- Sync to Graphics
    AddMappableBehavior( "HideShowUI", syncGraphics );
    PushMapEvent( "KeyRelease_KEY_F1", "HideShowUI" );
}

const std::string EventMapper::getKeyName(const gadget::Keys keyId) const
{
    using namespace gadget;
    switch(keyId)
    {
        case KEY_NONE: return std::string("KEY_NONE");
        case KEY_UP: return std::string("KEY_UP");
        case KEY_DOWN: return std::string("KEY_DOWN");
        case KEY_LEFT: return std::string("KEY_LEFT");
        case KEY_RIGHT: return std::string("KEY_RIGHT");
        case KEY_SHIFT: return std::string("KEY_SHIFT");
        case KEY_CTRL: return std::string("KEY_CTRL");
        case KEY_ALT: return std::string("KEY_ALT");
        case KEY_COMMAND: return std::string("KEY_COMMAND");
        case KEY_1: return std::string("KEY_1");
        case KEY_2: return std::string("KEY_2");
        case KEY_3: return std::string("KEY_3");
        case KEY_4: return std::string("KEY_4");
        case KEY_5: return std::string("KEY_5");
        case KEY_6: return std::string("KEY_6");
        case KEY_7: return std::string("KEY_7");
        case KEY_8: return std::string("KEY_8");
        case KEY_9: return std::string("KEY_9");
        case KEY_0: return std::string("KEY_0");
        case KEY_A: return std::string("KEY_A");
        case KEY_B: return std::string("KEY_B");
        case KEY_C: return std::string("KEY_C");
        case KEY_D: return std::string("KEY_D");
        case KEY_E: return std::string("KEY_E");
        case KEY_F: return std::string("KEY_F");
        case KEY_G: return std::string("KEY_G");
        case KEY_H: return std::string("KEY_H");
        case KEY_I: return std::string("KEY_I");
        case KEY_J: return std::string("KEY_J");
        case KEY_K: return std::string("KEY_K");
        case KEY_L: return std::string("KEY_L");
        case KEY_M: return std::string("KEY_M");
        case KEY_N: return std::string("KEY_N");
        case KEY_O: return std::string("KEY_O");
        case KEY_P: return std::string("KEY_P");
        case KEY_Q: return std::string("KEY_Q");
        case KEY_R: return std::string("KEY_R");
        case KEY_S: return std::string("KEY_S");
        case KEY_T: return std::string("KEY_T");
        case KEY_U: return std::string("KEY_U");
        case KEY_V: return std::string("KEY_V");
        case KEY_W: return std::string("KEY_W");
        case KEY_X: return std::string("KEY_X");
        case KEY_Y: return std::string("KEY_Y");
        case KEY_Z: return std::string("KEY_Z");
        case KEY_ESC: return std::string("KEY_ESC");

        case MOUSE_POSX: return std::string("MOUSE_POSX");
        case MOUSE_NEGX: return std::string("MOUSE_NEGX");
        case MOUSE_POSY: return std::string("MOUSE_POSY");
        case MOUSE_NEGY: return std::string("MOUSE_NEGY");
        case MBUTTON1: return std::string("MBUTTON1");
        case MBUTTON2: return std::string("MBUTTON2");
        case MBUTTON3: return std::string("MBUTTON3");
        case MBUTTON4: return std::string("MBUTTON4");
        case MBUTTON5: return std::string("MBUTTON5");
        case MBUTTON6: return std::string("MBUTTON5");
        case MBUTTON7: return std::string("MBUTTON7");
        case MBUTTON8: return std::string("MBUTTON8");
        case MBUTTON9: return std::string("MBUTTON9");
        case NO_MBUTTON: return std::string("NO_MBUTTON");

        case MOUSE_SCROLL_UP: return std::string("MOUSE_SCROLL_UP");
        case MOUSE_SCROLL_DOWN: return std::string("MOUSE_SCROLL_DOWN");
        case MOUSE_SCROLL_LEFT: return std::string("MOUSE_SCROLL_LEFT");
        case MOUSE_SCROLL_RIGHT: return std::string("MOUSE_SCROLL_RIGHT");

        case KEY_TAB          : return std::string("KEY_TAB");
        case KEY_BACKTAB      : return std::string("KEY_BACKTAB");
        case KEY_BACKSPACE    : return std::string("KEY_BACKSPACE");
        case KEY_RETURN       : return std::string("KEY_RETURN");
        case KEY_ENTER        : return std::string("KEY_ENTER");
        case KEY_INSERT       : return std::string("KEY_INSERT");
        case KEY_DELETE       : return std::string("KEY_DELETE");
        case KEY_PAUSE        : return std::string("KEY_PAUSE");
        case KEY_PRINT        : return std::string("KEY_PRINT");
        case KEY_SYSREQ       : return std::string("KEY_SYSREQ");
        case KEY_HOME         : return std::string("KEY_HOME");
        case KEY_END          : return std::string("KEY_END");
        case KEY_PRIOR        : return std::string("KEY_PRIOR");
        case KEY_NEXT         : return std::string("KEY_NEXT");
        case KEY_CAPS_LOCK    : return std::string("KEY_CAPS_LOCK");
        case KEY_NUM_LOCK     : return std::string("KEY_NUM_LOCK");
        case KEY_SCROLL_LOCK  : return std::string("KEY_SCROLL_LOCK");
        case KEY_F1           : return std::string("KEY_F1");
        case KEY_F2           : return std::string("KEY_F2");
        case KEY_F3           : return std::string("KEY_F3");
        case KEY_F4           : return std::string("KEY_F4");
        case KEY_F5           : return std::string("KEY_F5");
        case KEY_F6           : return std::string("KEY_F6");
        case KEY_F7           : return std::string("KEY_F7");
        case KEY_F8           : return std::string("KEY_F8");
        case KEY_F9           : return std::string("KEY_F9");
        case KEY_F10          : return std::string("KEY_F10");
        case KEY_F11          : return std::string("KEY_F11");
        case KEY_F12          : return std::string("KEY_F12");
        case KEY_F13          : return std::string("KEY_F13");
        case KEY_F14          : return std::string("KEY_F14");
        case KEY_F15          : return std::string("KEY_F15");
        case KEY_F16          : return std::string("KEY_F16");
        case KEY_F17          : return std::string("KEY_F17");
        case KEY_F18          : return std::string("KEY_F18");
        case KEY_F19          : return std::string("KEY_F19");
        case KEY_F20          : return std::string("KEY_F20");
        case KEY_F21          : return std::string("KEY_F21");
        case KEY_F22          : return std::string("KEY_F22");
        case KEY_F23          : return std::string("KEY_F23");
        case KEY_F24          : return std::string("KEY_F24");
        case KEY_F25          : return std::string("KEY_F25");
        case KEY_F26          : return std::string("KEY_F26");
        case KEY_F27          : return std::string("KEY_F27");
        case KEY_F28          : return std::string("KEY_F28");
        case KEY_F29          : return std::string("KEY_F29");
        case KEY_F30          : return std::string("KEY_F30");
        case KEY_F31          : return std::string("KEY_F31");
        case KEY_F32          : return std::string("KEY_F32");
        case KEY_F33          : return std::string("KEY_F33");
        case KEY_F34          : return std::string("KEY_F34");
        case KEY_F35          : return std::string("KEY_F35");

        case KEY_SUPER_L : return std::string("KEY_SUPER_L");
        case KEY_SUPER_R : return std::string("KEY_SUPER_R");
        case KEY_MENU    : return std::string("KEY_MENU");
        case KEY_HYPER_L : return std::string("KEY_HYPER_L");
        case KEY_HYPER_R : return std::string("KEY_HYPER_R");
        case KEY_HELP    : return std::string("KEY_HELP");
        case KEY_SPACE   : return std::string("KEY_SPACE");
        case KEY_ANY     : return std::string("KEY_ANY");

        case KEY_EXCLAM        : return std::string("KEY_EXCLAM");
        case KEY_QUOTE_DBL     : return std::string("KEY_QUOTE_DBL");
        case KEY_NUMBER_SIGN   : return std::string("KEY_NUMBER_SIGN");
        case KEY_DOLLAR        : return std::string("KEY_DOLLAR");
        case KEY_PERCENT       : return std::string("KEY_PERCENT");
        case KEY_AMPERSAND     : return std::string("KEY_AMPERSAND");
        case KEY_APOSTROPHE    : return std::string("KEY_APOSTROPHE");
        case KEY_PAREN_LEFT    : return std::string("KEY_PAREN_LEFT");
        case KEY_PAREN_RIGHT   : return std::string("KEY_PAREN_RIGHT");
        case KEY_ASTERISK      : return std::string("KEY_ASTERISK");
        case KEY_PLUS          : return std::string("KEY_PLUS");
        case KEY_COMMA         : return std::string("KEY_COMMA");
        case KEY_MINUS         : return std::string("KEY_MINUS");
        case KEY_PERIOD        : return std::string("KEY_PERIOD");
        case KEY_SLASH         : return std::string("KEY_SLASH");
        case KEY_COLON         : return std::string("KEY_COLON");
        case KEY_SEMICOLON     : return std::string("KEY_SEMICOLON");
        case KEY_LESS          : return std::string("KEY_LESS");
        case KEY_EQUAL         : return std::string("KEY_EQUAL");
        case KEY_GREATER       : return std::string("KEY_GREATER");
        case KEY_QUESTION      : return std::string("KEY_QUESTION");
        case KEY_AT            : return std::string("KEY_AT");
        case KEY_BRACKET_LEFT  : return std::string("KEY_BRACKET_LEFT");
        case KEY_BACKSLASH     : return std::string("KEY_BACKSLASH");
        case KEY_BRACKET_RIGHT : return std::string("KEY_BRACKET_RIGHT");
        case KEY_ASCII_CIRCUM  : return std::string("KEY_ASCII_CIRCUM");
        case KEY_UNDERSCORE    : return std::string("KEY_UNDERSCORE");
        case KEY_QUOTE_LEFT    : return std::string("KEY_QUOTE_LEFT");
        case KEY_BRACE_LEFT    : return std::string("KEY_BRACE_LEFT");
        case KEY_BAR           : return std::string("KEY_BAR");
        case KEY_BRACE_RIGHT   : return std::string("KEY_BRACE_RIGHT");
        case KEY_ASCII_TILDE   : return std::string("KEY_ASCII_TILDE");

        case KEY_UNKNOWN : return std::string("KEY_UNKNOWN");
        case LAST_KEY    : return std::string("LAST_KEY");
        default : return std::string("KEY_UNKNOWN");
   }
}

}
}
}
