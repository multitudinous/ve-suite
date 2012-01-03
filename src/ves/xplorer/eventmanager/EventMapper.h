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

#include <string>
#include <map>
#include <vector>

#include <ves/VEConfig.h>
#include <ves/xplorer/eventmanager/ScopedConnectionList.h>
#include <ves/xplorer/Logging.h>

#include <boost/signals2/signal.hpp>

#include <gadget/Type/KeyboardMouse/Keys.h>

// --- VR Juggler includes --- //
#include <vpr/Util/Singleton.h>



namespace ves
{
namespace xplorer
{
namespace eventmanager
{

/**
 * EventMapper exists to provide a way to map key presses / releases and button
 * presses / releases to signals that cause certain behaviors to be executed.
 * It's our version of "hot-key" mappings.
 *
 * In its current state, EventMapper just sets up some sensible default bindings.
 * In future, EventManager will expose an interface that can be used to re-map
 * the bindings at runtime.
 **/
class VE_EVENTMANAGER_EXPORTS EventMapper
{
public:

    enum syncType
    {
        syncNone,
        syncGraphics
    };

    /// Add a new mapping for KeyButton so that it executes Behavior. The mapping
    /// is added to a stack so the previous mapping can be restored by
    /// calling PopMapEvent.
    /// @param KeyButton Valid
    /// KeyButton strings are formed with the prefix "KeyPress_" or "KeyRelease_"
    /// followed by the suffix "KEY_#" where # represents a valid juggler keyname.
    /// Alpha characters are always capitalized (eg. "KEY_F" is a valid suffix,
    /// whereas "KEY_f" is not). The other type of valid KeyButton string is made
    /// from either the prefix "ButtonPress_" or "ButtonRelease_" and the suffix
    /// "MBUTTON#" where # is a number from 1 to 9.
    /// @param Behavior The name of the behavior signal to send out. The
    /// behavior should have been previously added via AddMappableBehavior.
    void PushMapEvent( const std::string& KeyButton, const std::string& Behavior );

    /// Pops the most recent mapping involving KeyButton off the stack and replaces
    /// the KeyButton->Behavior mapping with the previous one.
    void PopMapEvent( const std::string& KeyButton );

    /// Adds a mappable behavior. This will cause a signal with signature void()
    /// and name EventMapper.[BehaviorName] to be registered with EventManager.
    /// Once a behavior has been added in this way, it can be mapped to a KeyButton
    /// by calling PushMapEvent.
    /// @BehaviorName The name of the behavior (eg. "FrameAll")
    /// @sync One of (syncNone,syncGraphics): indicated whether this operation
    /// must be synced to the draw loop. syncNone behaviors are called immediately
    /// upon arrival of the mapped KeyButton; syncGraphics behaviors are stored in
    /// a queue and are processed during application's latePreFrameUpdate, when
    /// it is safe to touch the scenegraph.
    void AddMappableBehavior( const std::string& BehaviorName, syncType sync = syncNone );

    void LatePreFrameUpdate();

private:
    /// Constructor
    EventMapper( );

    /// Destructor
    virtual ~EventMapper( );

    /// Singleton declarations
    vprSingletonHeader( EventMapper );

    void SetupDefaultBindings();

    /// Receives mouse double clicks connected via EventManager
    bool MouseDoubleClickEvent( gadget::Keys button, int x, int y, int z, int state );

    /// Receives mouse buttonpress events connected via EventManager
    /// state = modifier mask OR'd with button mask
    bool ButtonPressEvent( gadget::Keys button, int x, int y, int state );

    /// Receives mouse buttonrelease events connected via EventManager
    /// state = modifier mask OR'd with button mask
    bool ButtonReleaseEvent( gadget::Keys button, int x, int y, int state );

    /// Receives key press events connected via EventManager
    bool KeyPressEvent( gadget::Keys key, int modifiers, char unicode );

    /// Receives key release events connected via EventManager
    bool KeyReleaseEvent( gadget::Keys key, int modifiers, char unicode );

    void QueueAndEmit( std::string& eventName, gadget::Keys key );

    void EmitSyncGraphicsSignals();

    const std::string getKeyName(const gadget::Keys keyId) const;

private:
    ves::xplorer::eventmanager::ScopedConnectionList mConnections;

    typedef boost::signals2::signal< void () > voidSignalType;

    typedef std::map< std::string, std::string > EventBehaviorMapType;
    EventBehaviorMapType mEventBehaviorMap;

    typedef std::map< std::string, voidSignalType* > BehaviorMapType;
    BehaviorMapType mSyncNoneBehaviorMap;

    BehaviorMapType mSyncGraphicsBehaviorMap;

    typedef std::map< std::string, std::vector< std::string >* > mEventHistoryMapType;
    mEventHistoryMapType mEventHistoryMap;

    std::vector< std::string > mSyncGraphicsQueue;

    Poco::Logger& m_logger;
    ves::xplorer::LogStreamPtr m_logStream;
};

}
}
}

